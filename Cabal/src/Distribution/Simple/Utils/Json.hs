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
jsonBuilder (JsonString s)     = surround "\"" "\"" (escapeStringBuilder s)
jsonBuilder (JsonRaw bs)       = BS.byteString bs

surround :: String -> String -> BS.Builder -> BS.Builder
surround begin end middle = BS.stringUtf8 begin <> middle <> BS.stringUtf8 end

escapeStringBuilder :: String -> BS.Builder
escapeStringBuilder = escape
    where
        -- | Minimally escape a 'String' in accordance with
        -- [RFC 8259, "7. Strings"](https://tools.ietf.org/html/rfc8259#section-7)
        escape :: String -> BS.Builder
        escape [] = mempty
        escape (x:xs) = case x of
          '\\' -> BS.stringUtf8 "\\\\" <> escape xs
          '"'  -> BS.stringUtf8 "\\\"" <> escape xs
          '\b' -> BS.stringUtf8 "\\b"  <> escape xs
          '\f' -> BS.stringUtf8 "\\f"  <> escape xs
          '\n' -> BS.stringUtf8 "\\n"  <> escape xs
          '\r' -> BS.stringUtf8 "\\r"  <> escape xs
          '\t' -> BS.stringUtf8 "\\t"  <> escape xs
          c    -> BS.charUtf8   c      <> escape xs

intercalate :: BS.Builder -> [BS.Builder] -> BS.Builder
intercalate sep = go
  where
    go :: [BS.Builder] -> BS.Builder
    go []     = mempty
    go [x]    = x
    go (x:xs) = x <> sep <> go xs
