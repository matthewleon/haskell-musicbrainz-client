{-# LANGUAGE OverloadedStrings #-}

module Network.Protocol.MusicBrainz.XML2.Utils (
    nameCreditsToArtistName
  , nameCreditsToArtistSortName
) where

import Control.Applicative ((<|>), liftA2)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T

import Network.Protocol.MusicBrainz.XML2.Types

nameCreditsToArtistName :: [NameCredit] -> Text
nameCreditsToArtistName = T.concat . map (nameSpan _nameCreditArtistName)

nameCreditsToArtistSortName :: [NameCredit] -> Text
nameCreditsToArtistSortName = T.concat . map (nameSpan (liftA2 (<|>) _nameCreditArtistSortName _nameCreditArtistName))

nameSpan :: (NameCredit -> Maybe Text) -> NameCredit -> Text
nameSpan f = T.concat . catMaybes . liftA2 (:) f (return . _nameCreditJoinPhrase)
