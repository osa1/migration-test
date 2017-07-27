{-# LANGUAGE TupleSections #-}

-- | New `users` table. `logs` table have foreign key to users
module DB3 where

--------------------------------------------------------------------------------
import Control.Monad
import qualified Data.Map as M
import Data.Monoid
import qualified Data.Text as T
import Data.Time
import Hedgehog
import Hedgehog.Range
import Prelude hiding (init, log, max, min)
--------------------------------------------------------------------------------
import Database.Selda hiding (text)
import Database.Selda.Backend
import Database.Selda.SQLite
--------------------------------------------------------------------------------
import qualified DB2
import Version
--------------------------------------------------------------------------------

usersTbl :: TableName -> Table (RowID :*: T.Text)
usersTbl tbl_name = table tbl_name (autoPrimary "id" :*: required "username")

logTbl :: TableName -> Table (RowID :*: DB2.Severity :*: UTCTime :*: T.Text)
logTbl tbl_name = table tbl_name (required "who" `fk` (users_tbl, user_id_col) :*:
                                  required "severity" :*:
                                  required "when" :*:
                                  required "log")
  where
    users_tbl = usersTbl "users"
    user_id_col :*: _ = selectors users_tbl

getLogs :: TableName -> SeldaM [DB2.Log]
getLogs tbl_name = do
    logs <- query (select (logTbl tbl_name))
    forM logs $ \(who_pk :*: sev :*: when_ :*: log) -> do
      who <- query $ do
        user_pk :*: uname <- select (usersTbl "users")
        restrict (user_pk .== literal who_pk)
        return uname
      case who of
        [] -> error ("Can't find user with pk " ++ show who_pk)
        uname : _ -> return (DB2.Log uname when_ sev log)

upgrade :: SeldaM ()
upgrade = do
    db <- seldaBackend

    assertVersion 2

    -- rename old table
    void $ liftIO $
      runStmt db ("ALTER TABLE " <> DB2.logTblName <> " RENAME TO logs_old") []

    -- create new table
    createTable (logTbl "logs")

    -- collect old users
    old_logs <- query (select (DB2.logTbl "logs_old"))
    let old_users = map (\(who :*: _) -> who) old_logs

    -- create users table
    createTable (usersTbl "users")
    user_pks :: M.Map T.Text RowID <-
      fmap M.fromList $
      forM old_users $
      \user -> (user,) <$> insertWithPK (usersTbl "users") [def :*: user]

    -- move logs
    forM_ old_logs $ \(who :*: sev :*: when_ :*: log) ->
      case M.lookup who user_pks of
        Nothing -> error ("Can't find " ++ show who ++ " in users: " ++ show user_pks)
        Just user_pk -> insert_ (logTbl "logs") [user_pk :*: sev :*: when_ :*: log]

    -- remove backup table
    dropTable (DB2.logTbl "logs_old")

    bumpVersion
    assertVersion 3

downgrade :: SeldaM ()
downgrade = do
    db <- seldaBackend

    assertVersion 3

    -- rename DB3 table
    void $ liftIO $
      runStmt db ("ALTER TABLE logs RENAME TO logs_old") []

    -- create DB2 table
    createTable (DB2.logTbl DB2.logTblName)

    -- move logs
    old_logs <- query (select (logTbl "logs_old"))
    forM_ old_logs $ \(who :*: sev :*: when_ :*: log) -> do
      uname <- query $ do
        user_pk :*: uname <- select (usersTbl "users")
        restrict (user_pk .== literal who)
        return uname
      case uname of
        [] -> error "Can't get user from PK"
        (u : _) -> insert_ (DB2.logTbl "logs") [u :*: sev :*: when_ :*: log]

    -- remove backup table
    dropTable (logTbl "logs_old")

    -- remove users table
    dropTable (usersTbl "users")

    downVersion
    assertVersion 2

--------------------------------------------------------------------------------
testMigration :: IO Bool
testMigration = checkSequential $ Group
    { groupName = "DB2-DB3 migration"
    , groupProperties =
        [ ("upgrade then downgrade preserves data", withTests 20 migration_prop_1)
        , ("upgrade preserves data", withTests 20 migration_prop_2)
        ]
    }

-- | Roundtrip property:
migration_prop_1 :: Property
migration_prop_1 = property $ do
    logs <- forAll DB2.genLogs
    (new_logs, old_ver, new_ver) <- liftIO $ withSQLite ":memory:" $ do
      DB2.createDb
      DB2.insertLogs logs
      old_ver <- getVersion
      upgrade
      downgrade
      new_ver <- getVersion
      new_logs <- DB2.getLogs DB2.logTblName
      return (new_logs, old_ver, new_ver)
    logs === new_logs
    old_ver === new_ver

-- | Upgrade preserves data.
migration_prop_2 :: Property
migration_prop_2 = property $ do
    logs <- forAll DB2.genLogs
    (new_logs, old_ver, new_ver) <- liftIO $ withSQLite ":memory:" $ do
      DB2.createDb
      DB2.insertLogs logs
      old_ver <- getVersion
      upgrade
      new_ver <- getVersion
      new_logs <- getLogs "logs"
      return (new_logs, old_ver, new_ver)
    logs === new_logs
    old_ver + 1 === new_ver

-- toDB2Log :: DB2.Log -> Log
-- toDB2Log DB1.Log{..} = Log _who _when DEBUG _msg
