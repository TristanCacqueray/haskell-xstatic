{-# LANGUAGE OverloadedStrings #-}

{- | Add XStaticFile to Html document.

@
indexHtml :: Html ()
indexHtml = do
    doctypehtml_ do
        head_ do
            meta_ [charset_ "utf-8"]
            meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
            xstaticScripts xfiles
  where
    xfiles = XStatic.htmx : XStatic.xterm
@
-}
module Lucid.XStatic (xstaticScripts) where

import Data.Foldable (traverse_)
import Data.Text.Encoding
import Lucid
import XStatic

-- | Adds 'script_' and 'link_' for javascript and css files.
xstaticScripts :: [XStaticFile] -> Html ()
xstaticScripts = traverse_ xrender
  where
    xrender :: XStaticFile -> Html ()
    xrender xf =
        let src = "/xstatic" <> decodeUtf8 (xfPath xf) <> "?v=" <> decodeUtf8 (xfETag xf)
         in case xfType xf of
                "application/javascript" -> script_ [src_ src] ("" :: Html ())
                "text/css" -> link_ [href_ src, rel_ "stylesheet"]
                _ -> pure ()
