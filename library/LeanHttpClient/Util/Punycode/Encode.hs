{-
This module is taken from the "punycode" package.

The following license covers this documentation, and the source code, except
where otherwise indicated.

Copyright 2012, Myles C. Maxfield. All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

\* Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.

\* Redistributions in binary form must reproduce the above copyright notice,
  this list of conditions and the following disclaimer in the documentation
  and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS "AS IS" AND ANY EXPRESS OR
IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
EVENT SHALL THE COPYRIGHT HOLDERS BE LIABLE FOR ANY DIRECT, INDIRECT,
INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA,
OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}
module LeanHttpClient.Util.Punycode.Encode (encode) where

import qualified Data.ByteString as BS
import Data.Char
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word
import LeanHttpClient.Prelude
import LeanHttpClient.Util.Punycode.Shared

data PunycodeState = PunycodeState
  { n :: Int,
    delta :: Int,
    bias :: Int,
    h :: Int
  }

-- | Encode a string into its ascii form
encode :: T.Text -> BS.ByteString
encode = execWriter . initialWriter

initialWriter :: (MonadWriter BS.ByteString m) => T.Text -> m ()
initialWriter input = do
  tell basics
  when (b > 0) $ tell $ BS.singleton $ fromIntegral $ ord '-'
  evalStateT (inner3 (map ord $ T.unpack input) b)
    $ PunycodeState
      { n = initial_n,
        delta = 0,
        bias = initial_bias,
        h = b
      }
  where
    basics = TE.encodeUtf8 $ T.filter isBasic input
    b = BS.length basics

inner3 :: (MonadState PunycodeState m, MonadWriter BS.ByteString m) => [Int] -> Int -> m ()
inner3 input b = do
  state <- get
  helper state
  where
    helper state
      | h' < length input = do
          put $ state {n = m, delta = delta'}
          mapM_ (inner2 b) input
          state' <- get
          put $ state' {delta = (delta state') + 1, n = (n state') + 1}
          inner3 input b
      | otherwise = return ()
      where
        m = minimum $ filter (>= n') input
        n' = n state
        h' = h state
        delta' = (delta state) + (m - n') * (h' + 1)

inner2 :: (MonadState PunycodeState m, MonadWriter BS.ByteString m) => Int -> Int -> m ()
inner2 b c = do
  state <- get
  helper state
  where
    helper state
      | c == n' = do
          q <- inner delta' base bias'
          tell $ BS.singleton $ baseToAscii q
          put $ state {bias = adapt delta' (h' + 1) (h' == b), delta = 0, h = (h state) + 1}
      | otherwise = put $ state {delta = delta'}
      where
        delta' = (delta state) + d
          where
            d
              | c < n' = 1
              | otherwise = 0
        n' = n state
        bias' = bias state
        h' = h state

inner :: (MonadWriter BS.ByteString m) => Int -> Int -> Int -> m Int
inner q k bias'
  | q < t = return q
  | otherwise = do
      tell $ BS.singleton $ baseToAscii $ t + ((q - t) `mod` (base - t))
      inner ((q - t) `div` (base - t)) (k + base) bias'
  where
    t
      | k <= bias' + tmin = tmin
      | k >= bias' + tmax = tmax
      | otherwise = k - bias'

baseToAscii :: Int -> Word8
baseToAscii i
  | i < 26 = fromIntegral $ i + (ord 'a')
  | otherwise = fromIntegral $ (i - 26) + (ord '0')
