module Network.Protocol.MusicBrainz.XML2.Types (
    MBID(..)
  , Release(..)
  , TextRepresentation(..)
  , Medium(..)
  , TrackList(..)
  , Track(..)
  , Recording(..)
  , NameCredit(..)
) where

import Data.Text (Text)
import Data.Time.Calendar (Day)
import Data.Vector (Vector)

newtype MBID = MBID { unMBID :: Text }
    deriving (Eq, Show)

data Release = Release {
    _releaseId :: MBID
  , _releaseTitle :: Text
  , _releaseStatus :: Maybe Text
  , _releaseQuality :: Maybe Text
  , _releasePackaging :: Maybe Text
  , _releaseTextRepresentation :: Maybe TextRepresentation
  , _releaseArtistCredit :: [NameCredit]
  , _releaseDate :: Maybe Day
  , _releaseCountry :: Maybe Text
  , _releaseBarcode :: Maybe Text
  , _releaseASIN :: Maybe Text
  , _releaseMedia :: Vector Medium
} deriving (Eq, Show)

data TextRepresentation = TextRepresentation {
    _textRepLanguage :: Maybe Text
  , _textRepScript :: Maybe Text
} deriving (Eq, Show)

data Medium = Medium {
    _mediumTitle :: Maybe Text
  , _mediumPosition :: Maybe Integer
  , _mediumTrackList :: Maybe TrackList
} deriving (Eq, Show)

data TrackList = TrackList {
    _trackListOffset :: Integer
  , _trackListTracks :: Vector Track
} deriving (Eq, Show)

data Track = Track {
    _trackPosition :: Maybe Integer
  , _trackNumber :: Maybe Integer
  , _trackLength :: Maybe Integer
  , _trackRecording :: Recording
} deriving (Eq, Show)

data Recording = Recording {
    _recordingId :: MBID
  , _recordingTitle :: Maybe Text
  , _recordingLength :: Maybe Integer
  , _recordingArtistCredit :: [NameCredit]
} deriving (Eq, Show)

data NameCredit = NameCredit {
    _nameCreditArtistId :: MBID
  , _nameCreditArtistName :: Maybe Text
  , _nameCreditArtistSortName :: Maybe Text
} deriving (Eq, Show)
