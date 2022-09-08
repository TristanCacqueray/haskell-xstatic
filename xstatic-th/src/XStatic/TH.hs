{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Create a 'XStaticFile' served as /\/my-file.txt/ or /\/xstatic\/my-file.txt/:

@
myFile :: XStaticFile
myFile = $(embedXStaticFile ".\/data\/my-file.txt")
@
-}
module XStatic.TH (
    embedXStaticFile,
    embedXStaticFileVersion,

    -- * re-export from "XStatic"
    XStaticFile,
) where

import Codec.Compression.GZip qualified as GZip
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 (pack)
import Data.ByteString.Char8 qualified as BS8
import Data.Digest.Pure.SHA qualified as SHA
import Data.List qualified
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import Data.Version (Version (..), showVersion)
import Language.Haskell.TH (Exp (AppE))
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax qualified as TH
import Network.Mime qualified as Mime
import XStatic (XStaticFile (..), isGzip)

doEmbedXStaticFile ::
    FilePath ->
    Maybe ByteString ->
    TH.Q TH.Exp
doEmbedXStaticFile fp etag = do
    buf <- TH.runIO (BS.readFile fp)
    contentE <- TH.lift (ensureCompress buf)
    nameE <- TH.lift clean_name
    mimeE <- TH.lift (Mime.defaultMimeLookup (Text.pack fp_without_gz))
    etagE <- TH.lift (fromMaybe (hashBuf buf) etag)

    mkXF <- [|XStaticFile|]
    pure $!
        mkXF
            `AppE` nameE
            `AppE` contentE
            `AppE` etagE
            `AppE` mimeE
  where
    hashBuf = pack . SHA.showDigest . SHA.sha1 . BS.fromStrict
    ensureCompress buf
        | isGzip buf = buf
        | otherwise = BS.toStrict (GZip.compress (BS.fromStrict buf))

    fp_without_gz = stripSuffix ".gz" fp
    clean_name =
        pack
            . mappend "/"
            . stripSuffix "index.html"
            . stripPrefix "data/"
            . dropWhile (\c -> c == '.' || c == '/')
            $ fp_without_gz
    stripSuffix :: String -> String -> String
    stripSuffix s n = reverse $ stripPrefix (reverse s) (reverse n)
    stripPrefix :: String -> String -> String
    stripPrefix p n = fromMaybe n (Data.List.stripPrefix p n)

{- | Embed a static file in its compressed form.

The following rules are applied to convert a local filepath to the expected request path:

  * /./, /\// and \"/data\//\" prefix are removed
  * \"/.gz/\" and \"/index.html/\" suffix are removed
-}
embedXStaticFile :: FilePath -> TH.Q TH.Exp
embedXStaticFile fp = doEmbedXStaticFile fp Nothing

-- | Same as 'embedXStaticFile', but using the provided 'Version' for the 'xfETag' value.
embedXStaticFileVersion :: FilePath -> Version -> TH.Q TH.Exp
embedXStaticFileVersion fp version = doEmbedXStaticFile fp (Just $ versionToEtag version)

versionToEtag :: Version -> ByteString
versionToEtag = mconcat . BS8.split '.' . pack . showVersion
