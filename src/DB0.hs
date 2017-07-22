module DB0 where

--------------------------------------------------------------------------------
import Control.Lens
import Control.Monad
import qualified Data.Aeson as JSON
import Data.Aeson.Lens
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe
import Data.String
import Data.List.Split (chunksOf)
import qualified Data.Text as T
import Data.Time (UTCTime)
--------------------------------------------------------------------------------
import Database.Selda
--------------------------------------------------------------------------------

data Log = Log
  { _who  :: T.Text
  , _when :: UTCTime
  , _msg  :: T.Text
  } deriving (Show, Eq)

logTbl :: TableName -> Table (T.Text :*: BS.ByteString)
logTbl tbl_name = table tbl_name (required "who" :*: required "log")

logTblName :: IsString s => s
logTblName = "logs"

createDb :: SeldaM ()
createDb = createTable (logTbl logTblName)

insertLogs :: [Log] -> SeldaM ()
insertLogs logs =
    -- sqlite can insert at most 999 things
    forM_ (chunksOf (999 `div` 2) logs) $ \logs' ->
      insert_ (logTbl logTblName) (map mk_log logs')
  where
    mk_log Log{..} = _who :*: msg'
      where
        msg' = LBS.toStrict $ JSON.encode $ JSON.object
          [ ("when", JSON.toJSON _when)
          , ("msg", JSON.toJSON _msg)
          ]

getLogs :: TableName -> SeldaM [Log]
getLogs tbl_name = do
    logs <- query (select (logTbl tbl_name))
    forM logs $ \(who :*: log_bs) -> do
      case JSON.eitherDecodeStrict log_bs :: Either String JSON.Value of
        Left err -> error ("Can't decode log data: " ++ err)
        Right json -> do
          when_ :: UTCTime <-
            case JSON.fromJSON (fromJust (json ^? key "when")) of
              JSON.Error err -> error err
              JSON.Success a -> return a

          msg :: T.Text <-
            case JSON.fromJSON (fromJust (json ^? key "msg")) of
              JSON.Error err -> error err
              JSON.Success a -> return a

          return (Log who when_ msg)
