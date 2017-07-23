-- | Log table gets a date column.
module DB1
  ( module DB1
  , DB0.Log (..)
  ) where

--------------------------------------------------------------------------------
import Control.Monad
import Data.List.Split (chunksOf)
import Data.Monoid
import Data.String
import qualified Data.Text as T
import Data.Time
import Hedgehog
import Hedgehog.Gen hiding (map)
import Hedgehog.Range
import Prelude hiding (init, max, min)
--------------------------------------------------------------------------------
import Database.Selda hiding (text)
import Database.Selda.Backend
import Database.Selda.SQLite
--------------------------------------------------------------------------------
import qualified DB0
import Version
--------------------------------------------------------------------------------

logTbl :: TableName -> Table (T.Text :*: UTCTime :*: T.Text)
logTbl tbl_name = table tbl_name (required "who" :*: required "when" :*: required "log")

logTblName :: IsString s => s
logTblName = "logs"

createDb :: SeldaM ()
createDb = createTable (logTbl logTblName)

getLogs :: TableName -> SeldaM [DB0.Log]
getLogs tbl_name = do
    logs <- query (select (logTbl tbl_name))
    return (map (\(who :*: when_ :*: msg) -> DB0.Log who when_ msg) logs)

insertLogs :: [DB0.Log] -> SeldaM ()
insertLogs logs =
    -- sqlite can insert at most 999 things
    forM_ (chunksOf (999 `div` 3) logs) $ \logs' ->
      insert_ (logTbl logTblName) (map mk_log logs')
  where
    mk_log DB0.Log{..} = _who :*: _when :*: _msg

getLogsInRange :: UTCTime -> UTCTime -> TableName -> SeldaM [DB0.Log]
getLogsInRange min max tbl_name = do
    let _ :*: sel_when :*: _ = selectors (logTbl tbl_name)
    logs <- query $ do
      l <- select (logTbl tbl_name)
      restrict (l ! sel_when .>= literal min)
      restrict (l ! sel_when .<= literal max)
      order (l ! sel_when) ascending
      return l
    return (map (\(who :*: when_ :*: msg) -> DB0.Log who when_ msg) logs)

upgrade :: SeldaM ()
upgrade = do
    db <- seldaBackend

    assertVersion 0

    -- rename old table
    void $ liftIO $
      runStmt db ("ALTER TABLE " <> DB0.logTblName <> " RENAME TO logs_old") []

    -- create new table
    createTable (logTbl logTblName)

    -- move old logs to new table
    old_logs <- DB0.getLogs "logs_old"
    let new_logs = map (\DB0.Log{..} -> _who :*: _when :*: _msg) old_logs
    -- sqlite can insert at most 999 things
    forM_ (chunksOf (999 `div` 3) new_logs) (insert_ (logTbl logTblName))

    -- remove backup table
    dropTable (DB0.logTbl "logs_old")

    -- update version number
    bumpVersion
    assertVersion 1

downgrade :: SeldaM ()
downgrade = do
    db <- seldaBackend

    assertVersion 1

    -- rename DB1's log table
    void $ liftIO $
      runStmt db ("ALTER TABLE " <> logTblName <> " RENAME TO logs_old") []

    -- create DB0's log table
    createTable (DB0.logTbl DB0.logTblName)

    -- move DB1 logs to DB0's table
    old_logs <- query (select (logTbl "logs_old"))
    let new_logs =
          map (\(old_who :*: old_when :*: old_msg) -> (DB0.Log old_who old_when old_msg)) old_logs
    DB0.insertLogs new_logs

    -- remove backup table
    dropTable (logTbl "logs_old")

    -- update version number
    downVersion
    assertVersion 0

--------------------------------------------------------------------------------
testMigration :: IO Bool
testMigration = checkSequential $ Group
    { groupName = "DB0-DB1 migration"
    , groupProperties =
        [ ("upgrade then downgrade preserves data", withTests 20 migration_prop_1)
        , ("upgrade preserves data", withTests 20 migration_prop_2)
        ]
    }

genLog :: Gen DB0.Log
genLog = do
    day <- fromGregorian <$> integral (linear 1 2000)
                         <*> integral (linear 1 12)
                         <*> integral (linear 0 31)
    dayTime_int :: Int <- integral (linear 0 86401)
    msg <- text (linear 0 10000) unicode
    return (DB0.Log "osa1" (UTCTime day (fromIntegral dayTime_int)) msg)

genLogs :: Gen [DB0.Log]
genLogs = list (linear 0 10000) genLog

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
    logs <- forAll genLogs
    (new_logs, old_ver, new_ver) <- liftIO $ withSQLite ":memory:" $ do
      DB0.createDb
      DB0.insertLogs logs
      old_ver <- getVersion
      upgrade
      downgrade
      new_ver <- getVersion
      new_logs <- DB0.getLogs DB0.logTblName
      return (new_logs, old_ver, new_ver)
    logs === new_logs
    old_ver === new_ver

-- | Upgrade preserves data:
--
-- * Create fresh table in memory.
-- * Generate a bunch of random DB0 data and write them to the DB.
-- * Upgrade
-- * Read everything, make sure the data is in original format.
--
migration_prop_2 :: Property
migration_prop_2 = property $ do
    logs <- forAll genLogs
    (new_logs, old_ver, new_ver) <- liftIO $ withSQLite ":memory:" $ do
      DB0.createDb
      DB0.insertLogs logs
      old_ver <- getVersion
      upgrade
      new_ver <- getVersion
      new_logs <- getLogs logTblName
      return (new_logs, old_ver, new_ver)
    logs === new_logs
    old_ver + 1 === new_ver
