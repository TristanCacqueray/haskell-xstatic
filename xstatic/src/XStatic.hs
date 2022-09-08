{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module XStatic (
    -- * xstatic api
    XStaticFile (..),
    xstaticApp,
    xstaticMiddleware,

    -- * helpers
    isGzip,
) where

import Data.Binary.Builder (Builder, fromByteString)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 (pack)
import Data.ByteString.Unsafe qualified as BS (unsafeDrop)
import Data.Map.Strict qualified as Map
import Network.HTTP.Types.Header (ResponseHeaders)
import Network.HTTP.Types.Status qualified as HTTP
import Network.Wai qualified

{- | A static file definition.

 Use the @xstatic-th@ or @file-embed@ library to embed a local file.
-}
data XStaticFile = XStaticFile
    { xfPath :: ByteString
    -- ^ The expected request path.
    , xfContent :: ByteString
    -- ^ The file content.
    , xfETag :: ByteString
    -- ^ The etag header value.
    , xfType :: ByteString
    -- ^ The content type, e.g. \"application\/javascript\" or \"text\/css\".
    }

{- | Create a wai application to serve 'XStaticFile'.

 The \"\/xstatic\/\" request path prefix is ignored.
-}
xstaticApp :: [XStaticFile] -> Network.Wai.Application
xstaticApp xs = \req resp -> do
    let basePath = Network.Wai.rawPathInfo req
        requestPath
            | "/xstatic/" `BS.isPrefixOf` basePath = BS.unsafeDrop 8 basePath
            | otherwise = basePath

    resp $ case Map.lookup requestPath files of
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
        ( xfPath xf
        ,
            ( fromByteString $ xfContent xf
            , (addGzipHeader $ xfContent xf)
                [ ("cache-control", "public, max-age=604800")
                , ("content-length", pack . show . BS.length $ xfContent xf)
                , ("content-type", xfType xf)
                , ("connection", "keep-alive")
                , ("etag", xfETag xf)
                , ("keep-alive", "timeout=5, max=100")
                ]
            )
        )

-- | Serve 'XStaticFile' using 'xstaticApp' when the provided application returns a 404.
xstaticMiddleware :: [XStaticFile] -> Network.Wai.Middleware
xstaticMiddleware xs app req resp = app req handleAppResp
  where
    staticApp = xstaticApp xs
    handleAppResp appResp = case HTTP.statusCode (Network.Wai.responseStatus appResp) of
        404 -> staticApp req resp
        _ -> resp appResp

addGzipHeader :: ByteString -> ResponseHeaders -> ResponseHeaders
addGzipHeader fileContent
    | isGzip fileContent = (("content-encoding", "gzip") :)
    | otherwise = id

isGzip :: ByteString -> Bool
isGzip = BS.isPrefixOf "\x1f\x8b\x08" -- the gzip magic number for deflate
