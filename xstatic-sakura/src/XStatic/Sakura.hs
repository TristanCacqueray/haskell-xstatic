{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module XStatic.Sakura (sakuraCss, sakuraDarkSolarizedCss, normalizeCss) where

import Paths_xstatic_sakura (version)
import XStatic.TH (XStaticFile, embedXStaticFile)

sakuraCss, sakuraDarkSolarizedCss, normalizeCss :: XStaticFile
sakuraCss = $(embedXStaticFile "data/sakura.css.gz" version)
sakuraDarkSolarizedCss = $(embedXStaticFile "data/sakura-dark-solarized.css.gz" version)
normalizeCss = $(embedXStaticFile "data/normalize.css.gz" version)
