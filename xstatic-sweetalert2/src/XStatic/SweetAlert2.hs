{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module XStatic.SweetAlert2 (sweetAlert2) where

import Paths_xstatic_sweetalert2 (version)
import XStatic.TH (XStaticFile, embedXStaticFileVersion)

sweetAlert2 :: XStaticFile
sweetAlert2 = $(embedXStaticFileVersion "data/sweetalert2.all.min.js.gz" version)
