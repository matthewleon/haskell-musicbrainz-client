{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Network.Protocol.MusicBrainz.JSON.WebService (
    getRecordingById
  , getReleaseById
  , searchReleasesByArtistAndRelease
) where

import Network.Protocol.MusicBrainz.Types

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as BL
import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TextEncoding
import Network.HTTP.Base (urlEncode)
import Network.HTTP.Conduit (Request, newManager, httpLbs, parseUrlThrow, requestHeaders, tlsManagerSettings, responseBody)
import Network.HTTP.Types.Header (hUserAgent)

musicBrainzWSLookup :: MonadIO m => Text -> Text -> Text -> [Text] -> m BL.ByteString
musicBrainzWSLookup agent reqtype param incparams = do
    let url = "https://musicbrainz.org/ws/2/" ++ T.unpack reqtype ++ "/" ++ T.unpack param ++ incs incparams ++ fj
    userAgentSimpleHttp agent url
    where
        incs [] = ""
        incs xs = ("?inc="++) . intercalate "+" . map T.unpack $ xs
        fj = "&fmt=json"

musicBrainzWSSearch :: MonadIO m => Text -> Text -> Text -> Maybe Int -> Maybe Int -> m BL.ByteString
musicBrainzWSSearch agent reqtype query mlimit moffset = do
    let url = "https://musicbrainz.org/ws/2/" ++ T.unpack reqtype ++ "/?query=" ++ urlEncode (T.unpack query) ++ limit mlimit ++ offset moffset ++ fj
    userAgentSimpleHttp agent url
    where
        limit Nothing = ""
        limit (Just l) = "&limit=" ++ show l
        offset Nothing = ""
        offset (Just o) = "&offset=" ++ show o
        fj = "&fmt=json"

getRecordingById :: MonadIO m => Text -> MBID -> m (Either String Recording)
getRecordingById agent mbid = do
    lbs <- musicBrainzWSLookup agent "recording" (unMBID mbid) ["artist-credits"]
    return $ eitherDecode lbs

getReleaseById :: MonadIO m => Text -> MBID -> m (Either String Release)
getReleaseById agent mbid = do
    lbs <- musicBrainzWSLookup agent "release" (unMBID mbid) ["recordings", "artist-credits"]
    return $ eitherDecode lbs

searchReleasesByArtistAndRelease :: MonadIO m => Text -> Text -> Text -> Maybe Int -> Maybe Int -> m (Either String [(Int, Release)])
searchReleasesByArtistAndRelease agent artist release mlimit moffset = do
    lbs <- musicBrainzWSSearch agent "release" (T.concat ["artist:\"", artist, "\" AND release:\"", release, "\""]) mlimit moffset
    return $ eitherDecode lbs

userAgentSimpleHttp :: MonadIO m => Text -> String -> m BL.ByteString
userAgentSimpleHttp userAgent url = liftIO $ do
  man <- newManager tlsManagerSettings
  initReq <- liftIO $ parseUrlThrow url
  let utf8UserAgent = TextEncoding.encodeUtf8 userAgent
      req = initReq {
    requestHeaders = (hUserAgent, utf8UserAgent) : requestHeaders initReq
  }
  responseBody <$> httpLbs (setConnectionClose req) man

  where
  setConnectionClose :: Request -> Request
  setConnectionClose req = req{requestHeaders = ("Connection", "close") : requestHeaders req}
