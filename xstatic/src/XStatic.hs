{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module XStatic (xstaticApp, XStaticFile (..), embedFile) where

import Data.Binary.Builder (Builder, fromByteString)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 (breakEnd, pack)
import Data.ByteString.Char8 qualified as BS8
import Data.FileEmbed (embedFile)
import Data.Map.Strict qualified as Map
import Data.Version (Version, showVersion)
import Network.HTTP.Types.Header (ResponseHeaders)
import Network.HTTP.Types.Status qualified as HTTP
import Network.Wai qualified

data XStaticFile = XStaticFile
    { name :: ByteString
    , content :: ByteString
    , contentVersion :: Version
    , contentType :: ByteString
    }

xstaticApp :: [XStaticFile] -> Network.Wai.Application
xstaticApp xs = \req resp ->
    let (_, fn) = breakEnd (== '/') (Network.Wai.rawPathInfo req)
     in
      resp $ case Map.lookup fn files of
            Just (builder, headers) ->
              let body = case Network.Wai.requestMethod req of
                    "HEAD" -> mempty
                    _ -> builder
               in Network.Wai.responseBuilder HTTP.status200 headers body
            Nothing -> Network.Wai.responseLBS HTTP.status404 mempty mempty
  where
    files :: Map.Map ByteString (Builder, ResponseHeaders)
    files = Map.fromList $ map toItem xs

    toItem :: XStaticFile -> (ByteString, (Builder, ResponseHeaders))
    toItem xf =
        ( name xf
        ,
            ( fromByteString $ content xf
            , ("cache-control", "public, max-age=604800")
                : ("content-length", pack . show . BS.length $ content xf)
                : ("content-type", contentType xf)
                : ("connection", "keep-alive")
                : ("content-encoding", "gzip")
                : ("etag", versionToEtag $ contentVersion xf)
                : ("keep-alive", "timeout=5, max=100")
                : []
            )
        )

versionToEtag :: Version -> ByteString
versionToEtag = BS8.unwords . BS8.split '.' . pack . showVersion
