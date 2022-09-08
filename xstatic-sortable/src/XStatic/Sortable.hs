{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module XStatic.Sortable (sortable) where

import Paths_xstatic_sortable (version)
import XStatic.TH (XStaticFile, embedXStaticFileVersion)

sortable :: XStaticFile
sortable = $(embedXStaticFileVersion "data/Sortable.min.js.gz" version)
