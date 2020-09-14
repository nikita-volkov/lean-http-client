module LeanHttpClient.Mason where

import LeanHttpClient.Prelude hiding (intersperse)
import Mason.Builder
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.ByteString as ByteString
import qualified Data.Text.Punycode as Punycode


percentEncodedPathSegmentText :: Text -> Builder
percentEncodedPathSegmentText =
  percentEncodedPathSegmentBytes . Text.encodeUtf8

percentEncodedPathSegmentBytes :: ByteString -> Builder
percentEncodedPathSegmentBytes =
  foldMap percentEncodedPathSegmentByte . ByteString.unpack

percentEncodedPathSegmentByte :: Word8 -> Builder
percentEncodedPathSegmentByte x =
  let
    reserved =
      x >= 65 && x <= 90 ||
      x >= 97 && x <= 122 ||
      x >= 48 && x <= 57 ||
      x >= 43 && x <= 46 ||
      x == 95 || x == 126 ||
      x == 58 || x == 64 || x == 38 || x == 61 || x == 36
    in if reserved
      then
        word8 x
      else
        percentEncodedByte x

percentEncodedQuerySegmentText :: Text -> Builder
percentEncodedQuerySegmentText =
  percentEncodedQuerySegmentBytes . Text.encodeUtf8

percentEncodedQuerySegmentBytes :: ByteString -> Builder
percentEncodedQuerySegmentBytes =
  foldMap percentEncodedQuerySegmentByte . ByteString.unpack

percentEncodedQuerySegmentByte :: Word8 -> Builder
percentEncodedQuerySegmentByte x =
  let
    reserved =
      x >= 65 && x <= 90 ||
      x >= 97 && x <= 122 ||
      x >= 48 && x <= 57 ||
      x == 45 || x == 95 || x == 46 || x == 126
    in if reserved
      then
        word8 x
      else
        percentEncodedByte x

percentEncodedByte :: Word8 -> Builder
percentEncodedByte x =
  case divMod x 16 of
    (a, b) -> word8 37 <> asciiHexDigit a <> asciiHexDigit b

asciiHexDigit :: Word8 -> Builder
asciiHexDigit x =
  word8 (if x < 10 then 48 + x else 55 + x)

domain :: Text -> Builder
domain =
  intersperse (char7 '.') . fmap domainSegment . Text.split (== '.')

domainSegment :: Text -> Builder
domainSegment value =
  if Text.all (< '\x80') value
    then textUtf8 value
    else byteString "xn--" <> byteString (Punycode.encode value)
