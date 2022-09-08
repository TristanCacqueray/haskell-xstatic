{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module XStatic.Sakura (sakuraCss, sakuraDarkSolarizedCss, normalizeCss) where

import Paths_xstatic_sakura (version)
import XStatic.TH (XStaticFile, embedXStaticFileVersion)

sakuraCss, sakuraDarkSolarizedCss, normalizeCss :: XStaticFile
sakuraCss = $(embedXStaticFileVersion "data/sakura.css.gz" version)
sakuraDarkSolarizedCss = $(embedXStaticFileVersion "data/sakura-dark-solarized.css.gz" version)
normalizeCss = $(embedXStaticFileVersion "data/normalize.css.gz" version)
