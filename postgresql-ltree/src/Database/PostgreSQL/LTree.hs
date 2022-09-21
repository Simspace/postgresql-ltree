-- | This module provides types and functions for PostgreSQL's @ltree@ https://www.postgresql.org/docs/current/ltree.html
--
-- You will want to use a specific implementation, e.g. @postgresql-simple-ltree@.
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Database.PostgreSQL.LTree
  ( LTree(..)
  , Label(unLabel)
  , map
  , fromList
  , toList
  , rootLabel
  , parentLabel
  , parent
  , numLabels
  , mkLabel
  , labelify
  , nullLabel
  , unsafeMkLabel
  , uuidToLabel
  , empty
  , null
  , singleton
  , snoc
  , render
  , unsafeUncheckedParse
  , parse
  , isImmediateParentOf
  , isImmediateChildOf
  , parseUUIDFromLabel
  , allLabelsUnique
  ) where

import Prelude hiding (map, null)

import Data.Aeson (FromJSON(parseJSON), ToJSON(toJSON))
import Data.Coerce (coerce)
import Data.Sequence (Seq((:<|), (:|>)), (|>))
import Data.Text (Text)
import Data.UUID (UUID)

import qualified Data.Attoparsec.Text as Atto
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.UUID as UUID

-- | Wrapper for Postgres' @ltree@ (label tree) type.
newtype LTree = LTree { unLTree :: Seq Label }
  deriving newtype (Show, Eq, Ord)

-- | Wrapper for a single label in an @ltree@.
-- The constructor IS NOT exported to ensure we only construct valid
-- labels. See 'mkLabel' for constructing a 'Label'.
newtype Label = Label { unLabel :: Text }
  deriving newtype (Show, Eq, Ord)

-- | Produce a new 'LTree' by applying the supplied function to each 'Label'.
map :: (Label -> Label) -> LTree -> LTree
map f = LTree . fmap f . unLTree

-- | Convert a list to an 'LTree'.
fromList :: [Label] -> LTree
fromList = LTree . Seq.fromList

-- | Convert an 'LTree' to a list.
toList :: LTree -> [Label]
toList = Foldable.toList . unLTree

-- | Get the first 'Label' from an 'LTree' if one exists.
rootLabel :: LTree -> Maybe Label
rootLabel (LTree (x :<| _)) = Just x
rootLabel _ = Nothing

-- | Get the second-to-last 'Label' in an 'LTree'.
parentLabel :: LTree -> Maybe Label
parentLabel (LTree x) = Seq.lookup (Seq.length x - 2) x

-- | Get the parent path of an 'LTree'.
parent :: LTree -> Maybe LTree
parent (LTree (xs :|> _)) = Just $ LTree xs
parent _ = Nothing

-- | Get the length of an 'LTree'.
numLabels :: LTree -> Int
numLabels (LTree x) = Seq.length x

-- | Safely construct a 'Label' from 'Text'. If the supplied 'Text'
-- contains characters unsupported by @ltree@. On failure, returns 'Left'
-- with an error message.
mkLabel :: Text -> Either String Label
mkLabel t =
  if Text.null t then
    Left "ltree label must be non-empty"
  else if List.null invalidChars then
    Right $ Label t
  else
    Left $ "Invalid ltree label chars found: " <> show invalidChars
  where
  invalidChars = List.nub $ Text.unpack $ Text.filter (not . isValidLabelChar) t

-- | Convert arbitrary 'Text' into a 'Label' by replacing `" -._"` with `"_"`
-- and dropping all other invalid characters. Returns `Nothing` if there are no
-- valid characters left.
labelify :: Text -> Maybe Label
labelify t
    | Text.null t' = Nothing
    | otherwise = Just $ Label t'
  where
    mapChars c
        | Set.member c $ Set.fromList " -._" = '_'
        | otherwise = c
    t' = Text.filter isValidLabelChar $ Text.map mapChars t

-- | A Label representing `"null"`. Useful as a backup to avoid partiality.
nullLabel :: Label
nullLabel = Label "null"

-- | Same as 'mkLabel' except throws an error for an invalid 'Text' input.
unsafeMkLabel :: Text -> Label
unsafeMkLabel = either error id . mkLabel

-- | A 'UUID' can always be converted into a 'Label' without error by
-- dropping the hyphens. The resulting 'Label' will only contain
-- numbers and lower-case alpha characters.
uuidToLabel :: UUID -> Label
uuidToLabel = Label . Text.filter (/= '-') . UUID.toText

-- | Predicate for which characters are supported by @ltree@.
isValidLabelChar :: Char -> Bool
isValidLabelChar = flip Set.member valid
  where
  valid = mconcat
    [ Set.singleton '_'
    , Set.fromList ['0'..'9']
    , Set.fromList ['A'..'Z']
    , Set.fromList ['a'..'z']
    ]

-- | An empty 'LTree'.
empty :: LTree
empty = LTree mempty

-- | Test whether an 'LTree' is empty.
null :: LTree -> Bool
null = Seq.null . unLTree

-- | Construct an 'LTree' from a single 'Label'.
singleton :: Label -> LTree
singleton = LTree . Seq.singleton

-- | Append a single 'Label' to the end of an 'LTree'; should be O(1)
-- since it's delegating to 'Data.Sequence.|>'
snoc :: LTree -> Label -> LTree
snoc (LTree xs) x = LTree (xs |> x)

-- | Render an @ltree@ as it would appear in the database.
render :: LTree -> Text
render = Text.intercalate "." . coerce . toList

-- | Unsafely parse an 'LTree' from 'Text' assuming each 'Label'
-- is valid. Use this only if you sure the input is a valid 'LTree';
-- e.g. it was fetched from a field the database of type @ltree@.
unsafeUncheckedParse :: Text -> LTree
unsafeUncheckedParse = fromList . coerce . Text.splitOn "."

-- | Parse an 'LTree' from 'Text'. If any 'Label' present is invalid,
-- returns 'Left'.
parse :: Text -> Either String LTree
parse = fmap fromList . traverse mkLabel . Text.splitOn "."

-- | Test whether the first 'LTree' is an immediate parent of the second;
-- e.g. @a.b@ is an immediate parent of @a.b.c@
isImmediateParentOf :: LTree -> LTree -> Bool
isImmediateParentOf (LTree xs) (LTree (ys :|> _)) | xs == ys = True
isImmediateParentOf _ _ = False

-- | Test whether the first 'LTree' is an immediate child of the second;
-- e.g. @a.b.c@ is an immediate child of @a.b@
isImmediateChildOf :: LTree -> LTree -> Bool
isImmediateChildOf = flip isImmediateParentOf

-- | Attempt to parse a 'UUID' from a 'Label'.
parseUUIDFromLabel :: Label -> Either String UUID
parseUUIDFromLabel (Label t) =
  Atto.parseOnly p t
  where
  p = do
    a <- Atto.take 8
    b <- Atto.take 4
    c <- Atto.take 4
    d <- Atto.take 4
    e <- Atto.take 12
    Atto.endOfInput
    maybe
      (fail "Label is not a valid UUID")
      pure
      (UUID.fromText $ Text.intercalate "-" [a, b, c, d, e])

-- | Test whether all labels in the 'LTree' are unique.
allLabelsUnique :: LTree -> Bool
allLabelsUnique (LTree xs) = length xs == (Set.size . Set.fromList . Foldable.toList $ xs)

instance FromJSON Label where
  parseJSON v = do
    text <- parseJSON v
    either fail pure $ mkLabel text

instance ToJSON Label where
  toJSON = toJSON . unLabel

instance FromJSON LTree where
  parseJSON v = do
    text <- parseJSON v
    either fail pure $ parse text

instance ToJSON LTree where
  toJSON = toJSON . render
