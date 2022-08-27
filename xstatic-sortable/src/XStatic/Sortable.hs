{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module XStatic.Sortable (sortable) where

import Paths_xstatic_sortable (version)
import XStatic (XStaticFile (..), embedFile)

sortable :: XStaticFile
sortable =
    XStaticFile
        { name = "Sortable.min.js"
        , content = $(embedFile "data/Sortable.min.js.gz")
        , contentType = "text/javascript; charset=UTF-8"
        , contentVersion = version
        }
