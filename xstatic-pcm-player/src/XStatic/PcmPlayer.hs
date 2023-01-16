{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module XStatic.PcmPlayer (pcmPlayer, pcmPlayerJs, pcmPlayerJsMap) where

import Paths_xstatic_pcm_player (version)
import XStatic.TH (XStaticFile, embedXStaticFileVersion)

pcmPlayer :: [XStaticFile]
pcmPlayer = [pcmPlayerJs, pcmPlayerJsMap]

pcmPlayerJs, pcmPlayerJsMap :: XStaticFile
pcmPlayerJs = $(embedXStaticFileVersion "data/pcm-player.js.gz" version)
pcmPlayerJsMap = $(embedXStaticFileVersion "data/pcm-player.js.map.gz" version)
