{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Network.Protocol.MusicBrainz.XML2.WebService (
    getRecordingById
  , getReleaseById
) where

import Network.Protocol.MusicBrainz.XML2.Types

import Control.Monad (when, replicateM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Conduit (Source, Sink, ($=), ($$), MonadResource, MonadThrow, runResourceT)
import Data.Conduit.List (sourceList)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Data.Time.Calendar (Day)
import Data.Time.Format (parseTime)
import qualified Data.Vector as V
import Data.XML.Types (Event)
import Network.HTTP.Conduit (http, withManager, parseUrl, Response(..), simpleHttp)
import Network.HTTP.Types (ok200)
import System.Locale (defaultTimeLocale)
import Text.XML.Stream.Parse (parseBytes, def, content, tagNoAttr, tagName, requireAttr, force, many)

-- not until conduit 0.5
sourceLbs :: Monad m => BL.ByteString -> Source m ByteString
sourceLbs = sourceList . BL.toChunks

musicBrainzWSRequest :: MonadIO m => Text -> Text -> [Text] -> m BL.ByteString
musicBrainzWSRequest reqtype param incparams = do
    let url = "https://musicbrainz.org/ws/2/" ++ T.unpack reqtype ++ "/" ++ T.unpack param ++ incs incparams
    simpleHttp url
    where
        incs [] = ""
	incs xs = ("?inc="++) . intercalate "+" . map T.unpack $ xs

getRecordingById :: (MonadBaseControl IO m, MonadIO m, MonadThrow m) => MBID -> m Recording
getRecordingById mbid = do
    lbs <- musicBrainzWSRequest "recording" (unMBID mbid) ["artist-credits"]
    rs <- runResourceT $ sourceLbs lbs $= parseBytes def $$ sinkRecordings
    return $ head rs

getReleaseById :: (MonadBaseControl IO m, MonadIO m, MonadThrow m) => MBID -> m Release
getReleaseById mbid = do
    lbs <- musicBrainzWSRequest "release" (unMBID mbid) ["recordings", "artist-credits"]
    rs <- runResourceT $ sourceLbs lbs $= parseBytes def $$ sinkReleases
    return $ head rs

sinkRecordings :: MonadThrow m => Sink Event m [Recording]
sinkRecordings = force "metadata required" (tagNoAttr "{http://musicbrainz.org/ns/mmd-2.0#}metadata" $ many parseRecording)

sinkReleases :: MonadThrow m => Sink Event m [Release]
sinkReleases = force "metadata required" (tagNoAttr "{http://musicbrainz.org/ns/mmd-2.0#}metadata" $ many parseRelease)

parseRecording :: MonadThrow m => Sink Event m (Maybe Recording)
parseRecording = tagName "{http://musicbrainz.org/ns/mmd-2.0#}recording" (requireAttr "id") $ \rid -> do
    title <- tagNoAttr "{http://musicbrainz.org/ns/mmd-2.0#}title" content
    len <- tagNoAttr "{http://musicbrainz.org/ns/mmd-2.0#}length" content
    ncs <- tagNoAttr "{http://musicbrainz.org/ns/mmd-2.0#}artist-credit" $ many parseNameCredits
    return Recording { _recordingId = MBID rid, _recordingTitle = title, _recordingLength = fmap forceReadDec len, _recordingArtistCredit = fromMaybe [] ncs }

parseNameCredits :: MonadThrow m => Sink Event m (Maybe NameCredit)
parseNameCredits = tagNoAttr "{http://musicbrainz.org/ns/mmd-2.0#}name-credit" $ force "artist required" (tagName "{http://musicbrainz.org/ns/mmd-2.0#}artist" (requireAttr "id") $ \aid -> do
    name <- tagNoAttr "{http://musicbrainz.org/ns/mmd-2.0#}name" content
    sortName <- tagNoAttr "{http://musicbrainz.org/ns/mmd-2.0#}sort-name" content
    return NameCredit { _nameCreditArtistId = MBID aid, _nameCreditArtistName = name, _nameCreditArtistSortName = sortName }
    )

forceReadDec :: Integral a => Text -> a
forceReadDec = (\(Right (d, _)) -> d) . TR.decimal

parseRelease :: MonadThrow m => Sink Event m (Maybe Release)
parseRelease = tagName "{http://musicbrainz.org/ns/mmd-2.0#}release" (requireAttr "id") $ \rid -> do
    title <- tagNoAttr "{http://musicbrainz.org/ns/mmd-2.0#}title" content
    status <- tagNoAttr "{http://musicbrainz.org/ns/mmd-2.0#}status" content
    quality <- tagNoAttr "{http://musicbrainz.org/ns/mmd-2.0#}quality" content
    packaging <- tagNoAttr "{http://musicbrainz.org/ns/mmd-2.0#}packaging" content
    tr <- parseTextRepresentation
    ncs <- tagNoAttr "{http://musicbrainz.org/ns/mmd-2.0#}artist-credit" $ many parseNameCredits
    date <- tagNoAttr "{http://musicbrainz.org/ns/mmd-2.0#}date" content
    country <- tagNoAttr "{http://musicbrainz.org/ns/mmd-2.0#}country" content
    barcode <- tagNoAttr "{http://musicbrainz.org/ns/mmd-2.0#}barcode" content
    asin <- tagNoAttr "{http://musicbrainz.org/ns/mmd-2.0#}asin" content
    media <- tagName "{http://musicbrainz.org/ns/mmd-2.0#}medium-list" (requireAttr "count") $ \_ -> many parseMedium
    return Release {
        _releaseId = MBID rid
      , _releaseTitle = title
      , _releaseStatus = status
      , _releaseQuality = quality
      , _releasePackaging = packaging
      , _releaseTextRepresentation = tr
      , _releaseArtistCredit = fromMaybe [] ncs
      , _releaseDate = parseTime defaultTimeLocale "%Y-%m-%d" . T.unpack =<< date
      , _releaseCountry = country
      , _releaseBarcode = barcode
      , _releaseASIN = asin
      , _releaseMedia = V.fromList (fromMaybe [] media)
    }

parseTextRepresentation :: MonadThrow m => Sink Event m (Maybe TextRepresentation)
parseTextRepresentation = tagNoAttr "{http://musicbrainz.org/ns/mmd-2.0#}text-representation" $ do
    lang <- tagNoAttr "{http://musicbrainz.org/ns/mmd-2.0#}language" content
    script <- tagNoAttr "{http://musicbrainz.org/ns/mmd-2.0#}script" content
    return TextRepresentation {
      _textRepLanguage = lang
    , _textRepScript = script
    }

parseMedium :: MonadThrow m => Sink Event m (Maybe Medium)
parseMedium = tagNoAttr "{http://musicbrainz.org/ns/mmd-2.0#}medium" $ do
    title <- tagNoAttr "{http://musicbrainz.org/ns/mmd-2.0#}title" content
    position <- tagNoAttr "{http://musicbrainz.org/ns/mmd-2.0#}position" content
    tracklist <- tagName "{http://musicbrainz.org/ns/mmd-2.0#}track-list" (requireAttr "count" >>= \c -> requireAttr "offset" >>= \o -> return (c, o)) $ \(c',o') -> do
        tracks <- many parseTrack
        return TrackList { _trackListOffset = forceReadDec o', _trackListTracks = V.fromList tracks }
    return Medium { _mediumTitle = title, _mediumPosition = fmap forceReadDec position, _mediumTrackList = tracklist }

parseTrack :: MonadThrow m => Sink Event m (Maybe Track)
parseTrack = tagNoAttr "{http://musicbrainz.org/ns/mmd-2.0#}track" $ do
    position <- tagNoAttr "{http://musicbrainz.org/ns/mmd-2.0#}position" content
    number <- tagNoAttr "{http://musicbrainz.org/ns/mmd-2.0#}number" content
    len <- tagNoAttr "{http://musicbrainz.org/ns/mmd-2.0#}length" content
    recording <- force "recording required" parseRecording
    return Track {
      _trackPosition = fmap forceReadDec position
    , _trackNumber = fmap forceReadDec number
    , _trackLength = fmap forceReadDec len
    , _trackRecording = recording
    }
