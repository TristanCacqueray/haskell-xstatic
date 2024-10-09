{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module XStatic.CodeMirror (codemirror, codemirrorJs, codemirrorJsMap) where

import Paths_xstatic_codemirror (version)
import XStatic.TH (XStaticFile, embedXStaticFileVersion)

codemirror :: [XStaticFile]
codemirror = [codemirrorJs, codemirrorJsMap]

codemirrorJs, codemirrorJsMap :: XStaticFile
codemirrorJs = $(embedXStaticFileVersion "data/codemirror.js.gz" version)
codemirrorJsMap = $(embedXStaticFileVersion "data/codemirror.js.map.gz" version)
