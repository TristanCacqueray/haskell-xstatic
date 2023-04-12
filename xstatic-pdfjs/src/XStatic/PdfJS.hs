{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module XStatic.PdfJS (pdfJs, pdfJsWorker) where

import Paths_xstatic_pdfjs (version)
import XStatic.TH (XStaticFile, embedXStaticFileVersion)

pdfJs, pdfJsWorker :: XStaticFile
pdfJs = $(embedXStaticFileVersion "data/pdf.min.js.gz" version)
pdfJsWorker = $(embedXStaticFileVersion "data/pdf.worker.min.js.gz" version)
