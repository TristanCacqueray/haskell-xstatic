{-# LANGUAGE ImportQualifiedPost #-}

module Servant.XStatic (xstaticServant) where

import Servant.API (Raw)
import Servant.Server (ServerT, Tagged (..))
import XStatic (XStaticFile)
import XStatic qualified

xstaticServant :: [XStaticFile] -> ServerT Raw m
xstaticServant = Tagged . XStatic.xstaticApp
