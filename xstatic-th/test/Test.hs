{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Paths_xstatic_th (version)
import XStatic
import XStatic.TH

xf :: XStaticFile
xf = $(embedXStaticFile "./xstatic-th.cabal" version)

main :: IO ()
main
    | validName && validVersion && validType = pure ()
    | otherwise = error $ "Bad template"
  where
    validName = name xf == "xstatic-th.cabal"
    validVersion = contentVersion xf == version
    validType = contentType xf == "application/octet-stream"
