{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module XStatic.QuillCursors (quillCursorsJs) where

import Paths_xstatic_quill_cursors (version)
import XStatic.TH (XStaticFile, embedXStaticFileVersion)

quillCursorsJs :: XStaticFile
quillCursorsJs = $(embedXStaticFileVersion "data/quill-cursors.js.gz" version)
