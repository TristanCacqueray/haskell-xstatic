{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module XStatic.Remixicon (remixicon, remixiconCss, remixiconWoff2) where

import Paths_xstatic_remixicon (version)
import XStatic.TH (XStaticFile, embedXStaticFileVersion)

remixicon :: [XStaticFile]
remixicon = [remixiconCss, remixiconWoff2]

remixiconCss, remixiconWoff2 :: XStaticFile
remixiconCss = $(embedXStaticFileVersion "data/remixicon.min.css.gz" version)
remixiconWoff2 = $(embedXStaticFileVersion "data/remixicon.woff2.gz" version)
