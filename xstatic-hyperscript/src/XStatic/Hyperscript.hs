{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module XStatic.Hyperscript (hyperscript, hyperscriptSSE) where

import Paths_xstatic_hyperscript (version)
import XStatic.TH (XStaticFile, embedXStaticFile)

hyperscript, hyperscriptSSE :: XStaticFile
hyperscript = $(embedXStaticFile "data/hyperscript.min.js.gz" version)
hyperscriptSSE = $(embedXStaticFile "data/eventsource.js.gz" version)
