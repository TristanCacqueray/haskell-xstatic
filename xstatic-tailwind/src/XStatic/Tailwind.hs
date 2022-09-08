{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module XStatic.Tailwind (tailwind) where

import Paths_xstatic_tailwind (version)
import XStatic.TH (XStaticFile, embedXStaticFile)

tailwind :: XStaticFile
tailwind = $(embedXStaticFile "data/tailwind.min.js.gz" version)
