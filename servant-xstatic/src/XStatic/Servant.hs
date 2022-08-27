{-# LANGUAGE ImportQualifiedPost #-}
module XStatic.Servant (xstaticApp) where

import XStatic qualified
import XStatic (XStaticFile)
import Servant.API (Raw)
import Servant.Server (ServerT, Tagged (..))

xstaticApp :: [XStaticFile] -> ServerT Raw m
xstaticApp = Tagged . XStatic.xstaticApp
