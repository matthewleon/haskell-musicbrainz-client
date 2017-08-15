module Network.Protocol.MusicBrainz.Types (
    MBID(..)
  , Release(..)
  , TextRepresentation(..)
  , Medium(..)
  , Track(..)
  , Recording(..)
  , ArtistCredit(..)
  , Artist(..)
  , ReleaseGroup(..)
  , LabelInfo(..)
  , Label(..)
  , ReleaseEvent(..)
  , Area(..)
  , ISO3166Code(..)
  , CoverArtArchive(..)
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
  , _releaseArtistCredit :: [ArtistCredit]
  , _releaseDate :: Maybe Day
  , _releaseCountry :: Maybe Text
  , _releaseEvents :: [ReleaseEvent]
  , _releaseBarcode :: Maybe Text
  , _releaseASIN :: Maybe Text
  , _releaseCoverArtArchive :: Maybe CoverArtArchive
  , _releaseMedia :: Vector Medium
} deriving (Eq, Show)

data TextRepresentation = TextRepresentation {
    _textRepLanguage :: Maybe Text
  , _textRepScript :: Maybe Text
} deriving (Eq, Show)

data Medium = Medium {
    _mediumTitle :: Maybe Text
  , _mediumPosition :: Maybe Integer
  , _mediumFormat :: Maybe Text
  , _mediumTrackCount :: Integer
  , _mediumTrackOffset :: Maybe Integer
  , _mediumTrackList :: Maybe [Track]
} deriving (Eq, Show)

data Track = Track {
    _trackId :: MBID
  , _trackArtistCredit :: [ArtistCredit]
  , _trackPosition :: Maybe Integer
  , _trackNumber :: Maybe Text
  , _trackLength :: Maybe Integer
  , _trackRecording :: Recording
} deriving (Eq, Show)

data Recording = Recording {
    _recordingId :: MBID
  , _recordingTitle :: Maybe Text
  , _recordingLength :: Maybe Integer
  , _recordingArtistCredit :: [ArtistCredit]
} deriving (Eq, Show)

data ArtistCredit = ArtistCredit {
    _artistCreditArtist :: Artist
  , _artistCreditJoinPhrase :: Maybe Text
  , _artistCreditName :: Maybe Text
} deriving (Eq, Show)

data Artist = Artist {
    _artistId :: MBID
  , _artistName :: Maybe Text
  , _artistSortName :: Maybe Text
  , _artistDisambiguation :: Maybe Text
} deriving (Eq, Show)

data ReleaseGroup = ReleaseGroup {
    _releaseGroupId :: MBID
  , _releaseGroupType :: Text
  , _releaseGroupTitle :: Maybe Text
  , _releaseGroupFirstReleaseDate :: Maybe Text
  , _releaseGroupPrimaryType :: Maybe Text
  , _releaseGroupArtistCredit :: [ArtistCredit]
} deriving (Eq, Show)

data LabelInfo = LabelInfo {
    _labelInfoCatalogNumber :: Maybe Text
  , _labelInfoLabel :: Label
} deriving (Eq, Show)

data Label = Label {
    _labelId :: MBID
  , _labelName :: Maybe Text
  , _labelSortName :: Maybe Text
  , _labelLabelCode :: Maybe Text
} deriving (Eq, Show)

data ReleaseEvent = ReleaseEvent {
    _releaseEventDate :: Maybe Day
  , _releaseEventArea :: Maybe Area
} deriving (Eq, Show)

data Area = Area {
    _areaId :: MBID
  , _areaName :: Maybe Text
  , _areaSortName :: Maybe Text
  , _areaISO3166_1Codes :: [ISO3166Code]
  , _areaISO3166_2Codes :: [ISO3166Code]
  , _areaISO3166_3Codes :: [ISO3166Code]
} deriving (Eq, Show)

data CoverArtArchive = CoverArtArchive {
    _coverArtArchiveArtwork :: Maybe Bool
  , _coverArtArchiveCount :: Maybe Integer
  , _coverArtArchiveFront :: Maybe Bool
  , _coverArtArchiveBack :: Maybe Bool
} deriving (Eq, Show)

newtype ISO3166Code = ISO3166Code { unISO3166Code :: Text }
    deriving (Eq, Show)
