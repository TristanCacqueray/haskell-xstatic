{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module XStatic.NoVNC (noVNC, noVNCJs, noVNCJsMap) where

import Paths_xstatic_novnc (version)
import XStatic.TH (XStaticFile, embedXStaticFileVersion)

noVNC :: [XStaticFile]
noVNC = [noVNCJs, noVNCJsMap]

noVNCJs, noVNCJsMap :: XStaticFile
noVNCJs = $(embedXStaticFileVersion "data/noVNC.js.gz" version)
noVNCJsMap = $(embedXStaticFileVersion "data/noVNC.js.map.gz" version)
