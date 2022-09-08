{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module XStatic.Htmx (htmx, htmxExtWS, htmxExtSSE, htmxExtDebug, htmxExtJsonEnc) where

import Paths_xstatic_htmx (version)
import XStatic.TH (XStaticFile, embedXStaticFile)

htmx, htmxExtWS, htmxExtSSE, htmxExtDebug, htmxExtJsonEnc :: XStaticFile
htmx = $(embedXStaticFile "data/htmx.min.js.gz" version)
htmxExtWS = $(embedXStaticFile "data/ext-ws.js.gz" version)
htmxExtSSE = $(embedXStaticFile "data/ext-sse.js.gz" version)
htmxExtDebug = $(embedXStaticFile "data/ext-debug.js.gz" version)
htmxExtJsonEnc = $(embedXStaticFile "data/ext-json-enc.js.gz" version)
