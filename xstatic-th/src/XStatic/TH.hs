{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module XStatic.TH (embedXStaticFile) where

import Codec.Compression.GZip qualified as GZip
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 (pack)
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import Data.Version
import Language.Haskell.TH (Exp (AppE))
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax qualified as TH
import Network.Mime qualified as Mime
import XStatic (XStaticFile (..))

-- | Embed a file as a XStaticFile by ensuring compression and discovering the MimeType using the extension.
embedXStaticFile ::
    -- | The base name strips '.gz' suffix, and ('.', '/' and "data/") prefix.
    FilePath ->
    -- | The package version for the ETag
    Version ->
    TH.Q TH.Exp
embedXStaticFile fp version = do
    buf <- TH.runIO (BS.readFile fp)
    contentE <- TH.lift (ensureCompress buf)
    nameE <- TH.lift clean_name
    mimeE <- TH.lift (Mime.defaultMimeLookup (stripSuffixT ".gz" $ Text.pack fp))

    mkVersion <- [|Version|]
    versionNum <- TH.lift (versionBranch version)
    versionTag <- TH.lift ([] :: [String])
    let v = mkVersion `AppE` versionNum `AppE` versionTag

    mkXF <- [|XStaticFile|]
    pure $!
        mkXF
            `AppE` nameE
            `AppE` contentE
            `AppE` v
            `AppE` mimeE
  where
    ensureCompress buf
        | BS.isPrefixOf "\x1f\x8b\x08" buf = buf
        | otherwise = BS.toStrict (GZip.compress (BS.fromStrict buf))
    clean_name = stripSuffix ".gz" $ stripPrefix "data/" $ pack $ dropWhile (\c -> c == '.' || c == '/') fp
    stripSuffixT s n = fromMaybe n (Text.stripSuffix s n)
    stripSuffix s n = fromMaybe n (BS.stripSuffix s n)
    stripPrefix p n = fromMaybe n (BS.stripPrefix p n)
