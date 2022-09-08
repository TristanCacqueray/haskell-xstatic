{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module XStatic.SweetAlert2 (sweetAlert2) where

import Paths_xstatic_sweetalert2 (version)
import XStatic.TH (XStaticFile, embedXStaticFile)

sweetAlert2 :: XStaticFile
sweetAlert2 = $(embedXStaticFile "data/sweetalert2.all.min.js.gz" version)
