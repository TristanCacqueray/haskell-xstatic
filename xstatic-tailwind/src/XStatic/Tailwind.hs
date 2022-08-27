{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module XStatic.Tailwind (tailwind) where

import Paths_xstatic_tailwind (version)
import XStatic (XStaticFile (..), embedFile)

tailwind :: XStaticFile
tailwind =
    XStaticFile
        { name = "tailwind.min.js"
        , content = $(embedFile "data/tailwind.min.js.gz")
        , contentType = "text/javascript; charset=UTF-8"
        , contentVersion = version
        }
