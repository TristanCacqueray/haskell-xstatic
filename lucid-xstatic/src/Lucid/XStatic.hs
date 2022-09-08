{-# LANGUAGE OverloadedStrings #-}

module Lucid.XStatic (xstaticScripts) where

import Data.Foldable (traverse_)
import Data.Text.Encoding
import Lucid
import XStatic

xstaticScripts :: [XStaticFile] -> Html ()
xstaticScripts = traverse_ xrender
  where
    xrender :: XStaticFile -> Html ()
    xrender xf =
        let src = "/xstatic" <> decodeUtf8 (xfPath xf)
         in case xfType xf of
                "application/javascript" -> with (script_ mempty) [src_ src]
                "text/css" -> link_ [href_ src, rel_ "stylesheet"]
                _ -> pure ()
