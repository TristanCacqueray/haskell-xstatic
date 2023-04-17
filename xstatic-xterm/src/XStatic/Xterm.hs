{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module XStatic.Xterm (xterm, xtermJs, xtermJsMap, xtermCss, xtermFitAddonJs, xtermFitAddonJsMap, xtermWebGLAddonJs, xtermWebGLAddonJsMap) where

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

xtermWebGLAddonJs, xtermWebGLAddonJsMap :: XStaticFile
xtermWebGLAddonJs = $(embedXStaticFileVersion "data/xterm-addon-webgl.js.gz" version)
xtermWebGLAddonJsMap = $(embedXStaticFileVersion "data/xterm-addon-webgl.js.map.gz" version)
