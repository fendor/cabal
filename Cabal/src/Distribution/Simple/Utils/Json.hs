-- | Utility json lib for Cabal
-- TODO: Remove it again.
module Distribution.Simple.Utils.Json
    ( Json(..)
    , renderJson
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Builder as BS

data Json = JsonArray [Json]
          | JsonBool !Bool
          | JsonNull
          | JsonNumber !Int
          | JsonObject [(ByteString, Json)]
          | JsonString !String
          | JsonRaw !ByteString

renderJson :: Json -> ByteString
renderJson = BS.toStrict . BS.toLazyByteString . jsonBuilder

jsonBuilder :: Json -> BS.Builder
jsonBuilder (JsonArray objs)   =
  surround "[" "]" $ intercalate (BS.stringUtf8 ",") (map jsonBuilder objs)
jsonBuilder (JsonBool True)    = BS.stringUtf8 "true"
jsonBuilder (JsonBool False)   = BS.stringUtf8 "false"
jsonBuilder  JsonNull          = BS.stringUtf8 "null"
jsonBuilder (JsonNumber n)     = BS.intDec n
jsonBuilder (JsonObject attrs) =
  surround "{" "}" $ intercalate (BS.stringUtf8 ",") $ map render attrs
  where
    render (k,v) = (surround "\"" "\"" $ BS.byteString k) <> BS.stringUtf8 ":" <> jsonBuilder v
jsonBuilder (JsonString s)     = surround "\"" "\"" $ showString' s
jsonBuilder (JsonRaw bs)       = BS.byteString bs

surround :: String -> String -> BS.Builder -> BS.Builder
surround begin end middle = BS.stringUtf8 begin <> middle <> BS.stringUtf8 end

showString' :: String -> BS.Builder
showString' xs = showStringWorker xs
    where
        showStringWorker :: String -> BS.Builder
        showStringWorker ('\"':as) = BS.stringUtf8 "\\\"" <> showStringWorker as
        showStringWorker ('\\':as) = BS.stringUtf8 "\\\\" <> showStringWorker as
        showStringWorker ('\'':as) = BS.stringUtf8 "\\\'" <> showStringWorker as
        showStringWorker (x:as) = BS.charUtf8 x <> showStringWorker as
        showStringWorker [] = BS.stringUtf8 ""

intercalate :: BS.Builder -> [BS.Builder] -> BS.Builder
intercalate sep = go
  where
    go :: [BS.Builder] -> BS.Builder
    go []     = mempty
    go [x]    = x
    go (x:xs) = x <> sep <> go xs
