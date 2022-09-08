{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module XStatic.Htmx (htmx, htmxExtWS, htmxExtSSE, htmxExtDebug, htmxExtJsonEnc) where

import Paths_xstatic_htmx (version)
import XStatic.TH (XStaticFile, embedXStaticFileVersion)

htmx, htmxExtWS, htmxExtSSE, htmxExtDebug, htmxExtJsonEnc :: XStaticFile
htmx = $(embedXStaticFileVersion "data/htmx.min.js.gz" version)
htmxExtWS = $(embedXStaticFileVersion "data/ext-ws.js.gz" version)
htmxExtSSE = $(embedXStaticFileVersion "data/ext-sse.js.gz" version)
htmxExtDebug = $(embedXStaticFileVersion "data/ext-debug.js.gz" version)
htmxExtJsonEnc = $(embedXStaticFileVersion "data/ext-json-enc.js.gz" version)
