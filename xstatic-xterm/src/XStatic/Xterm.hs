{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module XStatic.Xterm (xterm, xtermJs, xtermJsMap, xtermCss, xtermFitAddonJs, xtermFitAddonJsMap) where

import Paths_xstatic_xterm (version)
import XStatic.TH (XStaticFile, embedXStaticFileVersion)

xterm :: [XStaticFile]
xterm = [xtermJs, xtermJsMap, xtermCss]

xtermJs, xtermJsMap, xtermCss :: XStaticFile
xtermJs = $(embedXStaticFileVersion "data/xterm.js.gz" version)
xtermJsMap = $(embedXStaticFileVersion "data/xterm.js.map.gz" version)
xtermCss = $(embedXStaticFileVersion "data/xterm.css.gz" version)

xtermFitAddonJs, xtermFitAddonJsMap :: XStaticFile
xtermFitAddonJs = $(embedXStaticFileVersion "data/xterm-addon-fit.js.gz" version)
xtermFitAddonJsMap = $(embedXStaticFileVersion "data/xterm-addon-fit.js.map.gz" version)
