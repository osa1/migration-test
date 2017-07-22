{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE GADTs              #-}

-- | Log table gets a "severity" column. Existing logs get "DEBUG" severity.
module DB2 where

--------------------------------------------------------------------------------
import Control.Monad
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as LBS
import Data.List.Split (chunksOf)
import Data.Monoid
import Data.String
import qualified Data.Text as T
import Data.Time
import Data.Typeable
import GHC.Generics (Generic)
import Hedgehog
import Prelude hiding (init, max, min)
--------------------------------------------------------------------------------
import Database.Selda hiding (text)
import Database.Selda.Backend
import Database.Selda.SQLite
--------------------------------------------------------------------------------
import qualified DB1
--------------------------------------------------------------------------------

data Severity = DEBUG | INFO | WARN
  deriving (Show, Eq, Typeable, Generic)

instance JSON.ToJSON Severity
instance JSON.FromJSON Severity

instance SqlType Severity where
  mkLit sev = LCustom (LBlob (LBS.toStrict (JSON.encode sev)))

  fromSql sql =
    case sql of
      SqlBlob bs ->
        case JSON.eitherDecodeStrict bs of
          Left err -> error err
          Right sev -> sev
      _ -> error ("Unexpected severity column value: " ++ show sql)

  defaultValue = LCustom (LBlob (LBS.toStrict (JSON.encode DEBUG)))
                 -- FIXME not sure why this is needed

logTbl :: TableName -> Table (T.Text :*: Severity :*: UTCTime :*: T.Text)
logTbl tbl_name = table tbl_name (required "who" :*:
                                  required "severity" :*:
                                  required "when" :*:
                                  required "log")

data Log = Log
  { _who  :: T.Text
  , _when :: UTCTime
  , _sev  :: Severity
  , _msg  :: T.Text
  } deriving (Show, Eq)

logTblName :: IsString s => s
logTblName = "logs"

createDb :: SeldaM ()
createDb = createTable (logTbl logTblName)

insertLogs :: [Log] -> SeldaM ()
insertLogs logs =
    -- sqlite can insert at most 999 things
    forM_ (chunksOf (999 `div` 4) logs) $
      insert_ (logTbl logTblName) . map mk_log
  where
    mk_log Log{..} = _who :*: _sev :*: _when :*: _msg

getLogs :: TableName -> SeldaM [Log]
getLogs tbl_name = do
    logs <- query (select (logTbl tbl_name))
    return (map (\(who :*: sev :*: when_ :*: msg) -> Log who when_ sev msg) logs)

getLogsInRange :: UTCTime -> UTCTime -> TableName -> SeldaM [Log]
getLogsInRange min max tbl_name = do
    let _ :*: _ :*: sel_when :*: _ = selectors (logTbl tbl_name)
    logs <- query $ do
      l <- select (logTbl tbl_name)
      restrict (l ! sel_when .>= literal min)
      restrict (l ! sel_when .<= literal max)
      order (l ! sel_when) ascending
      return l
    return (map (\(who :*: sev :*: when_ :*: msg) -> Log who when_ sev msg) logs)

upgrade :: SeldaM ()
upgrade = do
    db <- seldaBackend

    -- rename old table
    void $ liftIO $
      runStmt db ("ALTER TABLE " <> DB1.logTblName <> " RENAME TO logs_old") []

    -- create new table
    createTable (logTbl logTblName)

    -- move old logs to new table
    old_logs <- DB1.getLogs "logs_old"
    let new_logs = map (\DB1.Log{..} -> _who :*: DEBUG :*: _when :*: _msg) old_logs
    -- sqlite can insert at most 999 things
    forM_ (chunksOf (999 `div` 4) new_logs) (insert_ (logTbl logTblName))

    -- remove backup table
    dropTable (DB1.logTbl "logs_old")

downgrade :: SeldaM ()
downgrade = do
    db <- seldaBackend

    -- rename DB2's log table
    void $ liftIO $
      runStmt db ("ALTER TABLE " <> logTblName <> " RENAME TO logs_old") []

    -- create DB1's log table
    createTable (DB1.logTbl DB1.logTblName)

    -- move DB2 logs to DB1's table
    old_logs <- query (select (logTbl "logs_old"))
    let new_logs =
          map (\(old_who :*: _ :*: old_when :*: old_msg) ->
                DB1.Log old_who old_when old_msg) old_logs
    DB1.insertLogs new_logs

    -- remove backup table
    dropTable (logTbl "logs_old")

--------------------------------------------------------------------------------
testMigration :: IO Bool
testMigration = checkSequential $ Group
    { groupName = "DB1-DB2 migration"
    , groupProperties =
        [ ("upgrade then downgrade preserves data", withTests 20 migration_prop_1)
        , ("upgrade preserves data", withTests 20 migration_prop_2)
        ]
    }

-- | Roundtrip property:
--
-- * Create a fresh table in memory.
-- * Generate a bunch of random DB0 data and write them to the DB.
-- * Upgrade
-- * Downgrade
-- * Read everything, make sure the data is in original format.
--
migration_prop_1 :: Property
migration_prop_1 = property $ do
    logs <- forAll DB1.genLogs
    logs' <- liftIO $ withSQLite ":memory:" $ do
      DB1.createDb
      DB1.insertLogs logs
      upgrade
      downgrade
      DB1.getLogs DB1.logTblName
    logs === logs'

-- | Upgrade preserves data:
--
-- * Create fresh table in memory.
-- * Generate a bunch of random DB0 data and write them to the DB.
-- * Upgrade
-- * Read everything, make sure the data is in original format.
--
migration_prop_2 :: Property
migration_prop_2 = property $ do
    logs <- forAll DB1.genLogs
    logs' <- liftIO $ withSQLite ":memory:" $ do
      DB1.createDb
      DB1.insertLogs logs
      upgrade
      getLogs logTblName
    map toDB2Log logs === logs'

toDB2Log :: DB1.Log -> Log
toDB2Log DB1.Log{..} = Log _who _when DEBUG _msg
