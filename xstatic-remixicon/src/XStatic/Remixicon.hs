{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module XStatic.Remixicon (remixicon, remixiconCss, remixiconWoff2) where

import Paths_xstatic_remixicon (version)
import XStatic (XStaticFile (..), embedFile)

remixicon :: [XStaticFile]
remixicon = [remixiconCss, remixiconWoff2]

remixiconCss :: XStaticFile
remixiconCss =
    XStaticFile
        { name = "remixicon.min.css"
        , content = $(embedFile "data/remixicon.min.css.gz")
        , contentType = "text/css"
        , contentVersion = version
        }

remixiconWoff2 :: XStaticFile
remixiconWoff2 =
    XStaticFile
        { name = "remixicon.woff2"
        , content = $(embedFile "data/remixicon.woff2.gz")
        , contentType = "application/font-woff2"
        , contentVersion = version
        }
