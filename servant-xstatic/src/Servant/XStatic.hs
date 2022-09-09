{-# LANGUAGE ImportQualifiedPost #-}

{- | Serve a XStaticFiles in your servant api:

@
type ServerAPI =
    Get '[HTML] (Html ())
        :\<|\> "xstatic" :> Raw

server :: Server ServerAPI
server = pure mempty \<|\> xstaticServant xfiles
  where
    xfiles = [XStatic.htmx]
@
-}

module Servant.XStatic (xstaticServant) where

import Servant.API (Raw)
import Servant.Server (ServerT, Tagged (..))
import XStatic (XStaticFile)
import XStatic qualified

-- | Create a 'Raw' handler.
xstaticServant :: [XStaticFile] -> ServerT Raw m
xstaticServant = Tagged . XStatic.xstaticApp
