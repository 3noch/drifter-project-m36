{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE EmptyDataDecls      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Drifter.ProjectM36
  ( Method(..)
  , PM36Migration
  , runMigrations
  , getChangeNames
  , migrationsRelSchema

  , ChangeName(..)
  , Change(..)
  ) where

import           Control.Monad                (forM_)
import qualified Data.Map.Strict              as Map
import           Data.Maybe
import           Data.Proxy                   (Proxy (..))
import           Data.Set                     (Set)
import qualified Data.Set                     as Set
import           Data.Text                    (Text)
import           Data.Time                    (UTCTime, getCurrentTime)
import           Drifter                      (Change (..), ChangeName (..),
                                               Method, resolveDependencyOrder)
import           GHC.Generics                 (Generic)
import           ProjectM36.Client.Simple
import           ProjectM36.Tupleable         (Tupleable)

import           Drifter.ProjectM36.RelSchema


data PM36Migration
data instance Method PM36Migration = MigrationStep { getMigrationStep :: Db (Either RelationalError ()) }


data SchemaChange = SchemaChange{
  changeName        :: Text,
  changeDescription :: Text,
  changeTime        :: UTCTime
} deriving (Eq, Show, Generic)
instance Tupleable SchemaChange
instance HasRelationName SchemaChange where relationName _ = "schema_migrations"


migrationsRelSchema :: RelSchema
migrationsRelSchema = mkRelSchema (Proxy :: Proxy SchemaChange) $ Map.fromList [
    ("changeName",        (TextAtomType,     Unique)),
    ("changeDescription", (TextAtomType,     NonUnique)),
    ("changeTime",        (DateTimeAtomType, NonUnique))
  ]


getChangeNames :: Db [ChangeName]
getChangeNames =
  map ChangeName . textAtoms <$>
    selectAttr "changeName" (fromRelation (Proxy :: Proxy SchemaChange))


runChange :: UTCTime -> Set ChangeName -> Change PM36Migration -> Db ()
runChange time existingChanges Change{..}
  | changeName `Set.member` existingChanges = pure ()
  | otherwise = do
      orCancelTransaction =<< getMigrationStep changeMethod
      orCancelTransaction =<< relationInsert [SchemaChange{
        changeName        = changeNameText changeName,
        changeDescription = fromMaybe "" changeDescription,
        changeTime        = time
      }]


runMigrations :: DbConn -> [Change PM36Migration] -> IO (Either DbError ())
runMigrations conn changes = do
  now <- getCurrentTime
  withTransaction conn $ do
    orCancelTransaction =<< defineRelSchemaIfNotExists migrationsRelSchema
    existingChanges <- Set.fromList <$> getChangeNames
    forM_ (resolveDependencyOrder changes) (runChange now existingChanges)
