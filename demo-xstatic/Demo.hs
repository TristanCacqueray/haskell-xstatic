{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Concurrent (threadDelay)
import Data.ByteString.Lazy (fromStrict)
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp as Warp (run)
import XStatic as XStatic
import XStatic.TH (embedXStaticFile)
import XStatic.Ace qualified as XStatic
import XStatic.Htmx qualified as XStatic
import XStatic.Remixicon qualified as XStatic
import XStatic.Sortable qualified as XStatic
import XStatic.Tailwind qualified as XStatic

staticApp :: Wai.Application
staticApp = XStatic.xstaticApp $ body : XStatic.aceJs : XStatic.htmx : XStatic.tailwind : XStatic.sortable : XStatic.remixicon <> XStatic.aceBundle

body :: XStaticFile
body = $(embedXStaticFile "index.html")

main :: IO ()
main = do
    putStrLn "Listening on :8080"
    Warp.run 8080 $ \req resp -> case Wai.rawPathInfo req of
        "/search" -> resp $ Wai.responseLBS HTTP.status200 [] "<i class=ri-git-merge-line /> search result"
        "/sort" -> do
            items <- HTTP.parseSimpleQuery <$> Wai.getRequestBodyChunk req
            let respBody =
                    mconcat
                        ( "<div class=htmx-indicator>Updating...</div>"
                            : map (\(_, v) -> "<div><input type=hidden name=item value=" <> v <> " />Item " <> v <> "</div>") items
                        )
            threadDelay 300000
            resp $ Wai.responseLBS HTTP.status200 [] (fromStrict respBody)
        _ -> do
            staticApp req resp
