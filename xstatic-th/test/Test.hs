{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Codec.Compression.GZip qualified as GZip
import Data.ByteString qualified as BS
import Test.Tasty
import Test.Tasty.HUnit

import Paths_xstatic_th (version)
import XStatic
import XStatic.TH

main :: IO ()
main = defaultMain $ do
    testGroup
        "xstatic-th"
        [ testCase "Test embedXStaticFileVersion" $ do
            let xf :: XStaticFile
                xf = $(embedXStaticFileVersion "./xstatic-th.cabal" version)

            xfPath xf @?= "/xstatic-th.cabal"
            isGzip (xfContent xf) @? "Uncompressed file"
            xfType xf @?= "application/octet-stream"
        , testCase "Test embedXStaticFile" $ do
            let xf :: XStaticFile
                xf = $(embedXStaticFile "./data/test.js")
            xfPath xf @?= "/test.js"
            xfETag xf @?= "724ba28f4a9a1b472057ff99511ed393a45552e1"
            xfType xf @?= "application/javascript"
            xfContent xf @?= BS.toStrict (GZip.compress "true\n")
        ]
