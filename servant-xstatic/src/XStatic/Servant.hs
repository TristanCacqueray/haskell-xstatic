{-# LANGUAGE ImportQualifiedPost #-}

module XStatic.Servant (xstaticApp) where

import Servant.API (Raw)
import Servant.Server (ServerT, Tagged (..))
import XStatic (XStaticFile)
import XStatic qualified

xstaticApp :: [XStaticFile] -> ServerT Raw m
xstaticApp = Tagged . XStatic.xstaticApp
