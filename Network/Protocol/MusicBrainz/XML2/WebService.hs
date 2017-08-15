{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Network.Protocol.MusicBrainz.XML2.WebService (
    getRecordingById
  , getReleaseById
  , searchReleasesByArtistAndRelease
) where

import Network.Protocol.MusicBrainz.Types

import Control.Applicative (liftA2, (<|>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Resource (MonadThrow, runResourceT)
import qualified Data.ByteString.Lazy as BL
--import qualified Data.ByteString.Lazy.Char8 as C
import Data.Conduit (Consumer, ($=), ($$))
import Data.Conduit.Binary (sourceLbs)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Data.Time.Format (parseTimeM)
import qualified Data.Vector as V
import Data.XML.Types (Event)
import Network.HTTP.Base (urlEncode)
import Network.HTTP.Conduit (Request, newManager, httpLbs, parseUrlThrow, requestHeaders, tlsManagerSettings, responseBody)
import Network.HTTP.Types.Header (hUserAgent)
import Data.Time.Locale.Compat (defaultTimeLocale)
import Text.XML.Stream.Parse (parseBytes, def, content, tagNoAttr, tag', requireAttr, attr, force, many, AttrParser)
import Text.XML (Name(..))

musicBrainzWSLookup :: MonadIO m => Text -> Text -> Text -> [Text] -> m BL.ByteString
musicBrainzWSLookup agent reqtype param incparams = do
    let url = "https://musicbrainz.org/ws/2/" ++ T.unpack reqtype ++ "/" ++ T.unpack param ++ incs incparams
    liftIO $ print url
    userAgentSimpleHttp agent url
    where
        incs [] = ""
        incs xs = ("?inc="++) . intercalate "+" . map T.unpack $ xs

musicBrainzWSSearch :: MonadIO m => Text -> Text -> Text -> Maybe Int -> Maybe Int -> m BL.ByteString
musicBrainzWSSearch agent reqtype query mlimit moffset = do
    let url = "https://musicbrainz.org/ws/2/" ++ T.unpack reqtype ++ "/?query=" ++ urlEncode (T.unpack query) ++ limit mlimit ++ offset moffset
    liftIO $ print url
    userAgentSimpleHttp agent url
    where
        limit Nothing = ""
        limit (Just l) = "&limit=" ++ show l
        offset Nothing = ""
        offset (Just o) = "&offset=" ++ show o

getRecordingById :: (MonadBaseControl IO m, MonadIO m, MonadThrow m) => Text -> MBID -> m Recording
getRecordingById agent mbid = do
    lbs <- musicBrainzWSLookup agent "recording" (unMBID mbid) ["artist-credits"]
    rs <- runResourceT $ sourceLbs lbs $= parseBytes def $$ sinkRecordings
    return $ head rs

getReleaseById :: (MonadBaseControl IO m, MonadIO m, MonadThrow m) => Text -> MBID -> m Release
getReleaseById agent mbid = do
    lbs <- musicBrainzWSLookup agent "release" (unMBID mbid) ["recordings", "artist-credits"]
    rs <- runResourceT $ sourceLbs lbs $= parseBytes def $$ sinkReleases
    return $ head rs

sinkRecordings :: MonadThrow m => Consumer Event m [Recording]
sinkRecordings = force "metadata required" (tagNoAttr "{http://musicbrainz.org/ns/mmd-2.0#}metadata" $ many parseRecording)

sinkReleases :: MonadThrow m => Consumer Event m [Release]
sinkReleases = force "metadata required" (tagNoAttr "{http://musicbrainz.org/ns/mmd-2.0#}metadata" $ many (fmap (fmap snd) parseRelease))

sinkReleaseList :: MonadThrow m => Consumer Event m [(Int, Release)]
sinkReleaseList = force "metadata required" (tag' "{http://musicbrainz.org/ns/mmd-2.0#}metadata" (attr "created") $ \_ ->
    force "release-list required" (tag' "{http://musicbrainz.org/ns/mmd-2.0#}release-list" (liftA2 (,) (requireAttr "count") (requireAttr "offset")) $ \_ -> many parseRelease))

parseRecording :: MonadThrow m => Consumer Event m (Maybe Recording)
parseRecording = tag' "{http://musicbrainz.org/ns/mmd-2.0#}recording" (requireAttr "id") $ \rid -> do
    title <- tagNoAttr "{http://musicbrainz.org/ns/mmd-2.0#}title" content
    len <- tagNoAttr "{http://musicbrainz.org/ns/mmd-2.0#}length" content
    ncs <- tagNoAttr "{http://musicbrainz.org/ns/mmd-2.0#}artist-credit" $ many parseArtistCredit
    return Recording { _recordingId = MBID rid, _recordingTitle = title, _recordingLength = fmap forceReadDec len, _recordingArtistCredit = fromMaybe [] ncs }

parseArtistCredit :: MonadThrow m => Consumer Event m (Maybe ArtistCredit)
parseArtistCredit = tag' "{http://musicbrainz.org/ns/mmd-2.0#}name-credit" buggyJoinPhrase $ \mjp -> force "artist required" (tag' "{http://musicbrainz.org/ns/mmd-2.0#}artist" (requireAttr "id") $ \aid -> do
    name <- tagNoAttr "{http://musicbrainz.org/ns/mmd-2.0#}name" content
    sortName <- tagNoAttr "{http://musicbrainz.org/ns/mmd-2.0#}sort-name" content
    _ <- tagNoAttr "{http://musicbrainz.org/ns/mmd-2.0#}disambiguation" content
    let a = Artist { _artistId = MBID aid, _artistName = name, _artistSortName = sortName, _artistDisambiguation = Nothing }
    return ArtistCredit { _artistCreditArtist = a, _artistCreditJoinPhrase = mjp, _artistCreditName = name }
    )

-- what's up with this
buggyJoinPhrase :: AttrParser (Maybe Text)
buggyJoinPhrase = fmap Just (requireAttr "{http://musicbrainz.org/ns/mmd-2.0#}joinphrase")
    <|> attr "{http://musicbrainz.org/ns/mmd-2.0#}joinphrase" { nameNamespace = Nothing }

forceReadDec :: Integral a => Text -> a
forceReadDec = (\(Right (d, _)) -> d) . TR.decimal

parseRelease :: MonadThrow m => Consumer Event m (Maybe (Int, Release))
parseRelease = tag' "{http://musicbrainz.org/ns/mmd-2.0#}release" (liftA2 (,) (requireAttr "id") (attr "{http://musicbrainz.org/ns/ext#-2.0}score")) $ \(rid,score) -> do
    title <- force "title required" (tagNoAttr "{http://musicbrainz.org/ns/mmd-2.0#}title" content)
    status <- tagNoAttr "{http://musicbrainz.org/ns/mmd-2.0#}status" content
    quality <- tagNoAttr "{http://musicbrainz.org/ns/mmd-2.0#}quality" content
    packaging <- tagNoAttr "{http://musicbrainz.org/ns/mmd-2.0#}packaging" content
    tr <- parseTextRepresentation
    ncs <- tagNoAttr "{http://musicbrainz.org/ns/mmd-2.0#}artist-credit" $ many parseArtistCredit
    _ <- parseReleaseGroup
    date <- tagNoAttr "{http://musicbrainz.org/ns/mmd-2.0#}date" content
    country <- tagNoAttr "{http://musicbrainz.org/ns/mmd-2.0#}country" content
    rel <- tagNoAttr "{http://musicbrainz.org/ns/mmd-2.0#}release-event-list" $ many parseReleaseEvent
    barcode <- tagNoAttr "{http://musicbrainz.org/ns/mmd-2.0#}barcode" content
    amazonASIN <- tagNoAttr "{http://musicbrainz.org/ns/mmd-2.0#}asin" content
    coverArtArchive <- parseCoverArtArchive
    _ <- tagNoAttr "{http://musicbrainz.org/ns/mmd-2.0#}label-info-list" parseLabelInfo
    media <- tag' "{http://musicbrainz.org/ns/mmd-2.0#}medium-list" (requireAttr "count") $ const (tagNoAttr "{http://musicbrainz.org/ns/mmd-2.0#}track-count" content >> many parseMedium)
    return (maybe 0 forceReadDec score, Release {
        _releaseId = MBID rid
      , _releaseTitle = title
      , _releaseStatus = status
      , _releaseQuality = quality
      , _releasePackaging = packaging
      , _releaseTextRepresentation = tr
      , _releaseArtistCredit = fromMaybe [] ncs
      , _releaseDate = parseTimeM True defaultTimeLocale "%Y-%m-%d" . T.unpack =<< date
      , _releaseCountry = country
      , _releaseEvents = fromMaybe [] rel
      , _releaseBarcode = barcode
      , _releaseASIN = amazonASIN
      , _releaseCoverArtArchive = coverArtArchive
      , _releaseMedia = V.fromList (fromMaybe [] media)
    })

parseTextRepresentation :: MonadThrow m => Consumer Event m (Maybe TextRepresentation)
parseTextRepresentation = tagNoAttr "{http://musicbrainz.org/ns/mmd-2.0#}text-representation" $ do
    lang <- tagNoAttr "{http://musicbrainz.org/ns/mmd-2.0#}language" content
    script <- tagNoAttr "{http://musicbrainz.org/ns/mmd-2.0#}script" content
    return TextRepresentation {
      _textRepLanguage = lang
    , _textRepScript = script
    }

parseMedium :: MonadThrow m => Consumer Event m (Maybe Medium)
parseMedium = tagNoAttr "{http://musicbrainz.org/ns/mmd-2.0#}medium" $ do
    title <- tagNoAttr "{http://musicbrainz.org/ns/mmd-2.0#}title" content
    position <- tagNoAttr "{http://musicbrainz.org/ns/mmd-2.0#}position" content
    format <- tagNoAttr "{http://musicbrainz.org/ns/mmd-2.0#}format" content
    Just med <- tag' "{http://musicbrainz.org/ns/mmd-2.0#}track-list" (liftA2 (,) (requireAttr "count") (attr "offset")) $ \(c,o) -> do -- not Just
        tracks <- many parseTrack
        return Medium {
            _mediumTitle = title
          , _mediumPosition = fmap forceReadDec position
          , _mediumFormat = format
          , _mediumTrackCount = forceReadDec c
          , _mediumTrackOffset = fmap forceReadDec o
          , _mediumTrackList = Just tracks -- not Just
          }
    return med

parseTrack :: MonadThrow m => Consumer Event m (Maybe Track)
parseTrack = tag' "{http://musicbrainz.org/ns/mmd-2.0#}track" (requireAttr "id") $ \i -> do
    position <- tagNoAttr "{http://musicbrainz.org/ns/mmd-2.0#}position" content
    number <- tagNoAttr "{http://musicbrainz.org/ns/mmd-2.0#}number" content
    len <- tagNoAttr "{http://musicbrainz.org/ns/mmd-2.0#}length" content
    artistcredit <- tagNoAttr "{http://musicbrainz.org/ns/mmd-2.0#}artist-credit" $ many parseArtistCredit
    recording <- force "recording required" parseRecording
    return Track {
      _trackId = MBID i
    , _trackArtistCredit = fromMaybe [] artistcredit
    , _trackPosition = fmap forceReadDec position
    , _trackNumber = number
    , _trackLength = fmap forceReadDec len
    , _trackRecording = recording
    }

parseReleaseGroup :: MonadThrow m => Consumer Event m (Maybe ReleaseGroup)
parseReleaseGroup = tag' "{http://musicbrainz.org/ns/mmd-2.0#}release-group" (liftA2 (,) (attr "type") (attr "id")) $ \(t,i) -> do
    title <- tagNoAttr "{http://musicbrainz.org/ns/mmd-2.0#}title" content
    frd <- tagNoAttr "{http://musicbrainz.org/ns/mmd-2.0#}first-release-date" content
    pt <- tagNoAttr "{http://musicbrainz.org/ns/mmd-2.0#}primary-type" content
    sts <- tagNoAttr "{http://musicbrainz.org/ns/mmd-2.0#}secondary-type-list" $ many parseSecondaryType
    ncs <- tagNoAttr "{http://musicbrainz.org/ns/mmd-2.0#}artist-credit" $ many parseArtistCredit
    return ReleaseGroup {
      _releaseGroupId = MBID <$> i
    , _releaseGroupType = t
    , _releaseGroupTitle = title
    , _releaseGroupFirstReleaseDate = frd
    , _releaseGroupPrimaryType = pt
    , _releaseGroupSecondaryTypes = fromMaybe [] sts
    , _releaseGroupArtistCredit = fromMaybe [] ncs
    }

    where
    parseSecondaryType :: MonadThrow m => Consumer Event m (Maybe Text)
    parseSecondaryType = tagNoAttr "{http://musicbrainz.org/ns/mmd-2.0#}secondary-type" content


parseLabelInfo :: MonadThrow m => Consumer Event m (Maybe LabelInfo)
parseLabelInfo = tagNoAttr "{http://musicbrainz.org/ns/mmd-2.0#}label-info" $ do
    catno <- tagNoAttr "{http://musicbrainz.org/ns/mmd-2.0#}catalog-number" content
    label <- force "label required" parseLabel
    return LabelInfo {
      _labelInfoCatalogNumber = catno
    , _labelInfoLabel = label
    }

parseLabel :: MonadThrow m => Consumer Event m (Maybe Label)
parseLabel = tag' "{http://musicbrainz.org/ns/mmd-2.0#}label" (requireAttr "id") $ \i -> do
    name <- tagNoAttr "{http://musicbrainz.org/ns/mmd-2.0#}name" content
    sortname <- tagNoAttr "{http://musicbrainz.org/ns/mmd-2.0#}sort-name" content
    labelcode <- tagNoAttr "{http://musicbrainz.org/ns/mmd-2.0#}label-code" content
    return Label {
      _labelId = MBID i
    , _labelName = name
    , _labelSortName = sortname
    , _labelLabelCode = labelcode
    }

parseReleaseEvent :: MonadThrow m => Consumer Event m (Maybe ReleaseEvent)
parseReleaseEvent = tagNoAttr "{http://musicbrainz.org/ns/mmd-2.0#}release-event" $ do
    date <- tagNoAttr "{http://musicbrainz.org/ns/mmd-2.0#}date" content
    area <- parseArea
    return ReleaseEvent {
      _releaseEventDate = parseTimeM True defaultTimeLocale "%Y-%m-%d" . T.unpack =<< date
    , _releaseEventArea = area
    }

parseArea :: MonadThrow m => Consumer Event m (Maybe Area)
parseArea = tag' "{http://musicbrainz.org/ns/mmd-2.0#}area" (requireAttr "id") $ \i -> do
    name <- tagNoAttr "{http://musicbrainz.org/ns/mmd-2.0#}name" content
    sortname <- tagNoAttr "{http://musicbrainz.org/ns/mmd-2.0#}sort-name" content
    isocodes1 <- tagNoAttr "{http://musicbrainz.org/ns/mmd-2.0#}iso-3166-1-code-list" $ many parseISO3166Code
    isocodes2 <- tagNoAttr "{http://musicbrainz.org/ns/mmd-2.0#}iso-3166-2-code-list" $ many parseISO3166Code
    isocodes3 <- tagNoAttr "{http://musicbrainz.org/ns/mmd-2.0#}iso-3166-3-code-list" $ many parseISO3166Code
    return Area {
      _areaId = MBID i
    , _areaName = name
    , _areaSortName = sortname
    , _areaISO3166_1Codes = fromMaybe [] isocodes1
    , _areaISO3166_2Codes = fromMaybe [] isocodes2
    , _areaISO3166_3Codes = fromMaybe [] isocodes3
    }

parseISO3166Code :: MonadThrow m => Consumer Event m (Maybe ISO3166Code)
parseISO3166Code = tagNoAttr "{http://musicbrainz.org/ns/mmd-2.0#}iso-3166-1-code" (ISO3166Code <$> content)

parseCoverArtArchive :: MonadThrow m => Consumer Event m (Maybe CoverArtArchive)
parseCoverArtArchive = tagNoAttr "{http://musicbrainz.org/ns/mmd-2.0#}cover-art-archive" $ do
    artwork <- tagNoAttr "{http://musicbrainz.org/ns/mmd-2.0#}artwork" content
    count <- tagNoAttr "{http://musicbrainz.org/ns/mmd-2.0#}count" content
    front <- tagNoAttr "{http://musicbrainz.org/ns/mmd-2.0#}front" content
    back <- tagNoAttr "{http://musicbrainz.org/ns/mmd-2.0#}back" content
    return CoverArtArchive {
      _coverArtArchiveArtwork = if artwork == Just "true" then Just True else Just False
    , _coverArtArchiveCount = fmap forceReadDec count
    , _coverArtArchiveFront = if front == Just "true" then Just True else Just False
    , _coverArtArchiveBack = if back == Just "true" then Just True else Just False
    }

searchReleasesByArtistAndRelease :: (MonadIO m, MonadBaseControl IO m, MonadThrow m) => Text -> Text -> Text -> Maybe Int -> Maybe Int -> m [(Int, Release)]
searchReleasesByArtistAndRelease agent artist release mlimit moffset = do
    lbs <- musicBrainzWSSearch agent "release" (T.concat ["artist:\"", artist, "\" AND release:\"", release, "\""]) mlimit moffset
    releases <- runResourceT $ sourceLbs lbs $= parseBytes def $$ sinkReleaseList
    liftIO $ print releases
    return releases

userAgentSimpleHttp :: MonadIO m => Text -> String -> m BL.ByteString
userAgentSimpleHttp userAgent url = liftIO $ do
  man <- newManager tlsManagerSettings
  initReq <- liftIO $ parseUrlThrow url
  let utf8UserAgent = TextEncoding.encodeUtf8 userAgent
      req = initReq {
    requestHeaders = (hUserAgent, utf8UserAgent) : requestHeaders initReq
  }
  responseBody <$> httpLbs (setConnectionClose req) man
  --putStrLn $ C.unpack res

  where
  setConnectionClose :: Request -> Request
  setConnectionClose req = req{requestHeaders = ("Connection", "close") : requestHeaders req}
