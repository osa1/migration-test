{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Definition of "version" table. This table never changes with migrations.
module Version where

--------------------------------------------------------------------------------
import Control.Monad
--------------------------------------------------------------------------------
import Database.Selda
import Database.Selda.Backend
--------------------------------------------------------------------------------

newtype DbVersion = DbVersion { dbVersionInt :: Int }
  deriving (Show, Eq, Num)

-- GND doesn't work, probably because GADTs used in method return types
instance SqlType DbVersion where
  mkLit (DbVersion ver) = LCustom (LInt ver)

  fromSql = DbVersion . fromSql

  defaultValue = LCustom (LInt 0)

versionTbl :: Table DbVersion
versionTbl = table "version" (required "version")

createVersionTbl :: DbVersion -> SeldaM ()
createVersionTbl ver = do
    createTable versionTbl
    insert_ versionTbl [ver]

getVersion :: SeldaM DbVersion
getVersion =
    query (select versionTbl) >>= \case
      []    -> error "getVersion: version table is empty"
      [ver] -> return ver
      vers  -> error ("getVersion: multiple versions found: " ++ show vers)

bumpVersion :: SeldaM ()
bumpVersion = do
    ver <- getVersion
    update_ versionTbl (const (literal True)) (const (literal (ver + 1)))

downVersion :: SeldaM ()
downVersion = do
    ver <- getVersion
    update_ versionTbl (const (literal True)) (const (literal (ver - 1)))

assertVersion :: DbVersion -> SeldaM ()
assertVersion expected = do
    actual <- getVersion
    unless (actual == expected) $
      error $ "Unexpected db version (expected: " ++
              show expected ++ " found: " ++ show actual ++ ")"
