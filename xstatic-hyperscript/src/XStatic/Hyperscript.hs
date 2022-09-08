{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module XStatic.Hyperscript (hyperscript, hyperscriptSSE) where

import Paths_xstatic_hyperscript (version)
import XStatic.TH (XStaticFile, embedXStaticFileVersion)

hyperscript, hyperscriptSSE :: XStaticFile
hyperscript = $(embedXStaticFileVersion "data/hyperscript.min.js.gz" version)
hyperscriptSSE = $(embedXStaticFileVersion "data/eventsource.js.gz" version)
