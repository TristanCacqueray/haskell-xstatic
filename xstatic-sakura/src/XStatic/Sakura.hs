{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module XStatic.Sakura (sakuraCss, sakuraDarkSolarizedCss, normalizeCss) where

import Paths_xstatic_sakura (version)
import XStatic (XStaticFile (..), embedFile)

sakuraCss :: XStaticFile
sakuraCss =
    XStaticFile
        { name = "sakura.css"
        , content = $(embedFile "data/sakura.css.gz")
        , contentType = "text/css"
        , contentVersion = version
        }

sakuraDarkSolarizedCss :: XStaticFile
sakuraDarkSolarizedCss =
    XStaticFile
        { name = "sakura-dark-solarized.css"
        , content = $(embedFile "data/sakura-dark-solarized.css.gz")
        , contentType = "text/css"
        , contentVersion = version
        }


normalizeCss :: XStaticFile
normalizeCss =
    XStaticFile
        { name = "normalize.css"
        , content = $(embedFile "data/normalize.css.gz")
        , contentType = "text/css"
        , contentVersion = version
        }
