{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module XStatic.Sortable (sortable) where

import Paths_xstatic_sortable (version)
import XStatic.TH (XStaticFile, embedXStaticFile)

sortable :: XStaticFile
sortable = $(embedXStaticFile "data/Sortable.min.js.gz" version)
