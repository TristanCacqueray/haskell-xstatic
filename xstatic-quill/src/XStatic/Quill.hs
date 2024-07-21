{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module XStatic.Quill (quill, quillJs, quillJsMap, quillSnowCss) where

import Paths_xstatic_quill (version)
import XStatic.TH (XStaticFile, embedXStaticFileVersion)

quill :: [XStaticFile]
quill = [quillJs, quillJsMap, quillSnowCss]

quillJs, quillJsMap, quillSnowCss :: XStaticFile
quillJs = $(embedXStaticFileVersion "data/quill.js.gz" version)
quillJsMap = $(embedXStaticFileVersion "data/quill.js.map.gz" version)
quillSnowCss = $(embedXStaticFileVersion "data/quill.snow.css.gz" version)
