module Main where

--------------------------------------------------------------------------------
import Data.Time.Clock
import Prelude hiding (init)
--------------------------------------------------------------------------------
import Database.Selda
import Database.Selda.SQLite
--------------------------------------------------------------------------------
import qualified DB0
import qualified DB1
import qualified DB2
--------------------------------------------------------------------------------

main :: IO ()
main = do
    now <- getCurrentTime
    withSQLite "test_db" $ do
      DB0.createDb
      DB0.insertLogs [DB0.Log "osa1" now "tired of persistent"]
      DB1.upgrade
      DB1.downgrade
      DB1.upgrade
      DB2.upgrade
      DB2.downgrade
      DB2.upgrade

    withSQLite "fresh_db" init

init :: SeldaM ()
init = sequence_
    [ DB0.createDb
    , DB1.upgrade
    , DB2.upgrade
    ]
