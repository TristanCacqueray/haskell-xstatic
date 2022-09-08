{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module XStatic.Remixicon (remixicon, remixiconCss, remixiconWoff2) where

import Paths_xstatic_remixicon (version)
import XStatic.TH (XStaticFile, embedXStaticFile)

remixicon :: [XStaticFile]
remixicon = [remixiconCss, remixiconWoff2]

remixiconCss, remixiconWoff2 :: XStaticFile
remixiconCss = $(embedXStaticFile "data/remixicon.min.css.gz" version)
remixiconWoff2 = $(embedXStaticFile "data/remixicon.woff2.gz" version)
