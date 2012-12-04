{-# LANGUAGE OverloadedStrings #-}

module Network.Protocol.MusicBrainz.XML2.Utils (
    nameCreditsToArtistName
  , nameCreditsToArtistSortName
) where

import Control.Applicative ((<|>), liftA2)
import Data.Text (Text)
import qualified Data.Text as T

import Network.Protocol.MusicBrainz.XML2.Types

nameCreditsToArtist :: (NameCredit -> Text) -> [NameCredit] -> Text
nameCreditsToArtist _ [] = T.empty
nameCreditsToArtist f [x] = f x
nameCreditsToArtist f (x:xs) = T.concat [f x, " (feat. ", commaed (map f xs), ")"]
    where
        commaed :: [Text] -> Text
        commaed ns
            | length ns == 2 = T.intercalate " and " ns
            | otherwise = T.intercalate ", " ns

nameCreditsToArtistName :: [NameCredit] -> Text
nameCreditsToArtistName = nameCreditsToArtist (maybe T.empty id . _nameCreditArtistName)

nameCreditsToArtistSortName :: [NameCredit] -> Text
nameCreditsToArtistSortName = nameCreditsToArtist (maybe T.empty id . liftA2 (<|>) _nameCreditArtistSortName _nameCreditArtistName)
