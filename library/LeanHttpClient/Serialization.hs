module LeanHttpClient.Serialization where

import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Punycode as Punycode
import LeanHttpClient.Prelude
import PtrPoker.Write
import qualified PtrPoker.Write as Write

-- * Execution

execute :: Write -> ByteString
execute =
  Write.writeToByteString

-- * Definitions

percentEncodedPathSegmentText :: Text -> Write
percentEncodedPathSegmentText =
  percentEncodedPathSegmentBytes . Text.encodeUtf8

percentEncodedPathSegmentBytes :: ByteString -> Write
percentEncodedPathSegmentBytes =
  foldMap percentEncodedPathSegmentByte . ByteString.unpack

percentEncodedPathSegmentByte :: Word8 -> Write
percentEncodedPathSegmentByte x =
  let reserved =
        x >= 65 && x <= 90
          || x >= 97 && x <= 122
          || x >= 48 && x <= 57
          || x >= 43 && x <= 46
          || x == 95
          || x == 126
          || x == 58
          || x == 64
          || x == 38
          || x == 61
          || x == 36
   in if reserved
        then word8 x
        else percentEncodedByte x

percentEncodedQuerySegmentText :: Text -> Write
percentEncodedQuerySegmentText =
  percentEncodedQuerySegmentBytes . Text.encodeUtf8

percentEncodedQuerySegmentBytes :: ByteString -> Write
percentEncodedQuerySegmentBytes =
  foldMap percentEncodedQuerySegmentByte . ByteString.unpack

percentEncodedQuerySegmentByte :: Word8 -> Write
percentEncodedQuerySegmentByte x =
  let reserved =
        x >= 65 && x <= 90
          || x >= 97 && x <= 122
          || x >= 48 && x <= 57
          || x == 45
          || x == 95
          || x == 46
          || x == 126
   in if reserved
        then word8 x
        else percentEncodedByte x

percentEncodedByte :: Word8 -> Write
percentEncodedByte x =
  case divMod x 16 of
    (a, b) -> word8 37 <> asciiHexDigit a <> asciiHexDigit b

asciiHexDigit :: Word8 -> Write
asciiHexDigit x =
  word8 (if x < 10 then 48 + x else 55 + x)

domain :: Text -> Write
domain =
  mconcat . intersperse (asciiChar '.') . fmap domainSegment . Text.split (== '.')

domainSegment :: Text -> Write
domainSegment value =
  if Text.all (< '\x80') value
    then textUtf8 value
    else byteString "xn--" <> byteString (Punycode.encode value)

asciiChar :: Char -> Write
asciiChar = word8 . fromIntegral . ord
