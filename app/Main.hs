module Main where

import qualified DB1
import qualified DB2
import qualified DB3

main :: IO ()
main = do
  DB1.testMigration >>= print
  DB2.testMigration >>= print
  DB3.testMigration >>= print
