{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module XStatic.Htmx (htmx, htmxExtWS, htmxExtSSE, htmxExtDebug, htmxExtJsonEnc) where

import Data.ByteString (ByteString)
import Paths_xstatic_htmx (version)
import XStatic (XStaticFile (..), embedFile)

mkStaticFile :: ByteString -> ByteString -> XStaticFile
mkStaticFile n c =
    XStaticFile
        { name = n
        , content = c
        , contentType = "text/javascript; charset=UTF-8"
        , contentVersion = version
        }

htmx, htmxExtWS, htmxExtSSE, htmxExtDebug, htmxExtJsonEnc :: XStaticFile
htmx = mkStaticFile "htmx.min.js" $(embedFile "data/htmx.min.js.gz")
htmxExtWS = mkStaticFile "ext-ws.js" $(embedFile "data/ext-ws.js.gz")
htmxExtSSE = mkStaticFile "ext-sse.js" $(embedFile "data/ext-sse.js.gz")
htmxExtDebug = mkStaticFile "ext-debug.js" $(embedFile "data/ext-debug.js.gz")
htmxExtJsonEnc = mkStaticFile "ext-json-enc.js" $(embedFile "data/ext-json-enc.js.gz")
