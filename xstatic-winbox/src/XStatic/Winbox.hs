{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module XStatic.Winbox (winbox, winboxCss, winboxJs, winboxJsMap) where

import Paths_xstatic_winbox (version)
import XStatic.TH (XStaticFile, embedXStaticFileVersion)

winbox :: [XStaticFile]
winbox = [winboxCss, winboxJs, winboxJsMap]

winboxCss, winboxJs, winboxJsMap :: XStaticFile
winboxJs = $(embedXStaticFileVersion "data/winbox.js.gz" version)
winboxJsMap = $(embedXStaticFileVersion "data/winbox.js.map.gz" version)
winboxCss = $(embedXStaticFileVersion "data/winbox.min.css.gz" version)
