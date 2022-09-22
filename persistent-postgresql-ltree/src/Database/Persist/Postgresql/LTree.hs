{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Persist.Postgresql.LTree () where

import Data.Bifunctor (first)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import Database.Persist
    ( PersistField, PersistValue(PersistLiteral_), LiteralType(Escaped)
    , fromPersistValue, toPersistValue
    )
import Database.Persist.Sql (PersistFieldSql, SqlType(SqlOther), sqlType)
import Database.PostgreSQL.LTree (LTree, parse, render)

instance PersistField LTree where
    toPersistValue l = PersistLiteral_ Escaped . T.encodeUtf8 $ render l
    fromPersistValue (PersistLiteral_ _ b) = first T.pack . parse $ T.decodeUtf8With T.lenientDecode b
    fromPersistValue _ = Left "Invalid LTree"

instance PersistFieldSql LTree where
    sqlType _ = SqlOther "ltree"
