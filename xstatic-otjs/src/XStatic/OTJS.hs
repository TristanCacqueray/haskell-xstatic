{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module XStatic.OTJS (otJs) where

import Paths_xstatic_otjs (version)
import XStatic.TH (XStaticFile, embedXStaticFileVersion)

otJs :: XStaticFile
otJs = $(embedXStaticFileVersion "data/ot.js.gz" version)
