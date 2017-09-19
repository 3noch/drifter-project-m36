{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Drifter.ProjectM36.RelSchema where

import           Control.Monad            (forM)
import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as Map
import           Data.Maybe               (catMaybes, mapMaybe)
import           Data.Proxy               (Proxy (..))
import qualified Data.Set                 as Set
import           Data.Text                (Text)
import qualified Data.Vector              as V
import           GHC.Generics             (Generic)

import           ProjectM36.Base
import           ProjectM36.Client        (databaseContextExprForUniqueKey)
import           ProjectM36.Client.Simple
import           ProjectM36.Error         (RelationalError (RelVarAlreadyDefinedError))
import           ProjectM36.Tupleable     (Tupleable, fromTuple, toInsertExpr)


-- | Type class for all types that have a named relation variable in a database.
class HasRelationName a where
  relationName :: proxy a -> Text


data Uniqueness = Unique | NonUnique deriving (Bounded, Enum, Eq, Show)

-- | A simple representation of a relation schema. It is intentionally detached from a particular type or
-- any 'Tupleable' instance because migrations must keep track of how types change over time.
data RelSchema = RelSchema {
    _relSchemaName  :: Text,
    _relSchemaAttrs :: Map Text (AtomType, Uniqueness)
  } deriving (Eq, Show, Generic)

-- | ^ Smart constructor for 'RelSchema' that can use the derived relation name.
mkRelSchema :: forall a proxy. (HasRelationName a) => proxy a -> Map Text (AtomType, Uniqueness) -> RelSchema
mkRelSchema _ = RelSchema (relationName (Proxy :: Proxy a))

-- | Define a named relation in the database.
defineRelSchema :: RelSchema -> Db (Either RelationalError ())
defineRelSchema (RelSchema name attrs) = do
  res <- executeOrErr schemaDef
  case res of
    Left _   -> pure res
    Right () -> executeOrErr uniqueConstraints
  where
    attrList = Map.toList attrs
    schemaDef = Define name [NakedAttributeExpr (Attribute attr type_) | (attr, (type_, _)) <- attrList]
    uniqueConstraints = databaseContextExprForUniqueKey name [attr | (attr, (_, Unique)) <- attrList]

-- | Defined a named relation in the database if it's not already defined.
defineRelSchemaIfNotExists :: RelSchema -> Db (Either RelationalError ())
defineRelSchemaIfNotExists schema = do
  res <- defineRelSchema schema
  pure $ case res of
    Right ()                           -> res
    Left (RelVarAlreadyDefinedError _) -> Right ()
    Left _                             -> res

-- | Inserts a 'Tupleable' record into its corresponding relation variable.
relationInsert :: forall a t. (Tupleable a, HasRelationName a, Traversable t) => t a -> Db (Either RelationalError ())
relationInsert record =
  case toInsertExpr record (relationName (Proxy :: Proxy a)) of
    (Left err)       -> pure (Left err)
    Right insertExpr -> executeOrErr insertExpr


relationAttr :: AttributeName -> Relation -> [Atom]
relationAttr attrName relation = catMaybes (tupleAttr attrName <$> relationTuples relation)

relationTuples :: Relation -> [RelationTuple]
relationTuples (Relation _ (RelationTupleSet tuples)) = tuples

tupleAttr :: AttributeName -> RelationTuple -> Maybe Atom
tupleAttr attrName (RelationTuple names atoms) =
  snd <$> V.find (\(Attribute name _, _) -> name == attrName) (V.zip names atoms)

-- TODO: Use a fold of some sort to make it faster.
tupleAttrs :: [AttributeName] -> RelationTuple -> Maybe [Atom]
tupleAttrs attrNames tuple = traverse (`tupleAttr` tuple) attrNames

textAtoms :: [Atom] -> [Text]
textAtoms atoms = [txt | TextAtom txt <- atoms]

project :: [AttributeName] -> RelationalExprBase a -> RelationalExprBase a
project attrNames = Project (AttributeNames (Set.fromList attrNames))

fromRelation :: forall a proxy. (HasRelationName a) => proxy a -> RelationalExprBase ()
fromRelation _ = RelationVariable (relationName (Proxy :: Proxy a)) ()

whereEquals :: Atomable val => Text -> val -> RelationalExprBase b -> RelationalExprBase b
whereEquals col val = Restrict (AttributeEqualityPredicate col (NakedAtomExpr (toAtom val)))


-- | Adds a projection for the given attributes and uses 'query'' to get them.
selectAttrs :: [AttributeName] -> RelationalExprBase () -> Db [[Atom]]
selectAttrs attrNames q =
  mapMaybe (tupleAttrs attrNames) . relationTuples
    <$> query (project attrNames q)


selectAttr :: AttributeName -> RelationalExprBase () -> Db [Atom]
selectAttr attrName q = relationAttr attrName <$> query (project [attrName] q)

-- | Select a record from a named relation variable using its 'Tupleable' instance.
relationSelect
  :: forall a. (Tupleable a, HasRelationName a)
  => (RelationalExprBase () -> RelationalExprBase ())  -- a function to transform the query expression (e.g. @whereEquals "attr" 5@)
  -> Db [a]
relationSelect expr = do
  tuples <- relationTuples <$> query (expr $ fromRelation (Proxy :: Proxy a))
  forM tuples $ orCancelTransaction . fromTuple

-- | Like 'relationSelect' but where the query expression is 'id'.
relationSelectAll :: forall a. (Tupleable a, HasRelationName a) => Db [a]
relationSelectAll = relationSelect id
