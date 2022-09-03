{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module XStatic.Hyperscript (hyperscript, hyperscriptSSE) where

import Paths_xstatic_hyperscript (version)
import XStatic (XStaticFile (..), embedFile)

hyperscript :: XStaticFile
hyperscript =
    XStaticFile
        { name = "hyperscript.min.js"
        , content = $(embedFile "data/_hyperscript.min.js.gz")
        , contentType = "text/javascript; charset=UTF-8"
        , contentVersion = version
        }

hyperscriptSSE :: XStaticFile
hyperscriptSSE =
    XStaticFile
        { name = "eventsource.js"
        , content = $(embedFile "data/eventsource.js.gz")
        , contentType = "text/javascript; charset=UTF-8"
        , contentVersion = version
        }
