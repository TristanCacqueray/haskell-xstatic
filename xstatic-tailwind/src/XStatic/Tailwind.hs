{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module XStatic.Tailwind (tailwind) where

import Paths_xstatic_tailwind (version)
import XStatic.TH (XStaticFile, embedXStaticFileVersion)

tailwind :: XStaticFile
tailwind = $(embedXStaticFileVersion "data/tailwind.min.js.gz" version)
