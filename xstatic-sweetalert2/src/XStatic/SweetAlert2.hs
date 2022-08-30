{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module XStatic.SweetAlert2 (sweetAlert2) where

import Paths_xstatic_sweetalert2 (version)
import XStatic (XStaticFile (..), embedFile)

sweetAlert2 :: XStaticFile
sweetAlert2 =
    XStaticFile
        { name = "sweetalert2.all.js"
        , content = $(embedFile "data/sweetalert2.all.min.js.gz")
        , contentType = "text/javascript; charset=UTF-8"
        , contentVersion = version
        }
