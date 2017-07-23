module Main where

--------------------------------------------------------------------------------
import Data.Time.Clock
import qualified Data.Vector as V
import Prelude hiding (init)
--------------------------------------------------------------------------------
import Database.Selda
import Database.Selda.SQLite
--------------------------------------------------------------------------------
import qualified DB0
import qualified DB1
import qualified DB2
import Version
--------------------------------------------------------------------------------

main :: IO ()
main = do
    now <- getCurrentTime

    putStrLn "Creating test_db..."
    withSQLite "test_db" $ do
      DB0.createDb
      DB0.insertLogs [DB0.Log "osa1" now "tired of persistent"]
      DB1.upgrade
      DB1.downgrade
      DB1.upgrade
      DB2.upgrade
      DB2.downgrade
      DB2.upgrade

    putStrLn "Creating fresh_db..."
    withSQLite "fresh_db" init

    putStrLn "Creating migration_1..."
    withSQLite "migration_1" DB0.createDb
    withSQLite "migration_1" migrateDB

    putStrLn "Creating migration_2..."
    withSQLite "migration_2" DB0.createDb
    withSQLite "migration_2" DB1.upgrade
    withSQLite "migration_2" migrateDB

init :: SeldaM ()
init = sequence_ migrations

migrations :: V.Vector (SeldaM ())
migrations = V.fromList
    [ DB0.createDb
    , DB1.upgrade
    , DB2.upgrade
    ]

migrateDB :: SeldaM ()
migrateDB = do
  DbVersion ver <- getVersion
  sequence_ (V.drop (ver + 1) migrations)
