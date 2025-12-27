{-# LANGUAGE BangPatterns #-}

module ModeS.Demodulator
  ( process
  ) where

import Data.Bits (shiftR)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BSU
import Data.Vector.Unboxed qualified as V
import Data.Word (Word16, Word8)

import ModeS.Types (Message (..), MessageLength (..))

-- | Constants
longMsgBits :: Int
longMsgBits = 112

shortMsgBits :: Int
shortMsgBits = 56

preambleUs :: Int
preambleUs = 8

-- | Convert raw I/Q samples into magnitude vector
computeMagnitudeVector :: BS.ByteString -> V.Vector Word16
computeMagnitudeVector bs = V.generate numSamples getMagnitude
 where
  numSamples = BS.length bs `div` 2
  getMagnitude idx =
    let i = fromIntegral (BSU.unsafeIndex bs (idx * 2)) - 127 :: Int
        q = fromIntegral (BSU.unsafeIndex bs (idx * 2 + 1)) - 127 :: Int
        magnitude = sqrt (fromIntegral (i * i + q * q) :: Double)
    in round (magnitude * 360)

-- | Check if magnitudes at position form a valid preamble
detectPreamble :: V.Vector Word16 -> Int -> Bool
detectPreamble mags pos
  | pos + 14 >= V.length mags = False
  | otherwise =
      let
        (!mag0, !mag1, !mag2, !mag3) = (mags V.! pos, mags V.! (pos + 1), mags V.! (pos + 2), mags V.! (pos + 3))
        (!mag4, !mag5, !mag6, !mag7) =
          (mags V.! (pos + 4), mags V.! (pos + 5), mags V.! (pos + 6), mags V.! (pos + 7))
        (!mag8, !mag9) = (mags V.! (pos + 8), mags V.! (pos + 9))

        !threshold = (fromIntegral (mag0 + mag2 + mag7 + mag9) :: Int) `div` 6

        patternCheck =
          mag0 > mag1
            && mag1 < mag2
            && mag2 > mag3
            && mag3 < mag0
            && mag4 < mag0
            && mag5 < mag0
            && mag6 < mag0
            && mag7 > mag8
            && mag8 < mag9
            && mag9 > mag6

        spikeCheck = fromIntegral mag4 < threshold && fromIntegral mag5 < threshold

        spaceCheck = all (< threshold) $ map (fromIntegral . (mags V.!)) [pos + 11 .. pos + 14]
      in
        patternCheck && spikeCheck && spaceCheck

-- | Determine message length from first byte (DF field)
determineMessageLength :: [Bool] -> Maybe MessageLength
determineMessageLength bits =
  if length bits < 8
    then Nothing
    else
      let
        df = bitsToWord8 (take 8 bits) `shiftR` 3
      in
        Just
          $ if df < 16
            then ShortMessage
            else LongMessage

-- | Demodulate initial bits to determine message type
demodulateInitialBits :: V.Vector Word16 -> Int -> Int -> Maybe [Bool]
demodulateInitialBits mags startPos numBits = go 0 []
 where
  minDelta = 256 :: Word16
  go i acc
    | i >= numBits = Just (reverse acc)
    | otherwise = do
        let pos = startPos + (i * 2)
        low <- mags V.!? pos
        high <- mags V.!? (pos + 1)
        let delta = abs (fromIntegral low - fromIntegral high)
        if low == high
          then Nothing
          else case acc of
            (x : _) | delta < minDelta -> go (i + 1) (x : acc)
            _ -> go (i + 1) ((low > high) : acc)

-- | Demodulate remaining bits after type determination
demodulateRemainingBits
  :: V.Vector Word16 -> Int -> Int -> [Bool] -> Maybe [Bool]
demodulateRemainingBits mags startPos totalBits initialBits = go (length initialBits) initialBits
 where
  minDelta = 256 :: Word16
  go i acc
    | i >= totalBits = Just acc
    | otherwise =
        let pos = startPos + (i * 2)
            !low = mags V.! pos
            !high = mags V.! (pos + 1)
            !delta = abs (fromIntegral low - fromIntegral high)
        in if low == high
             then Nothing
             else
               if i > 0 && delta < minDelta
                 then go (i + 1) (acc ++ [last acc])
                 else go (i + 1) (acc ++ [low > high])

-- | Demodulate bits from magnitude samples with length detection
demodulateMessage :: V.Vector Word16 -> Int -> Maybe Message
demodulateMessage mags startPos = do
  initialBits <- demodulateInitialBits mags startPos 8
  msgLenType <- determineMessageLength initialBits

  let requiredBits = case msgLenType of
        ShortMessage -> shortMsgBits
        LongMessage -> longMsgBits

  if startPos + (requiredBits * 2) > V.length mags
    then Nothing
    else do
      fullBits <- demodulateRemainingBits mags startPos requiredBits initialBits
      let avgDelta = calcAverageDelta mags startPos msgLenType
      if avgDelta < (10 * 255)
        then Nothing
        else
          Just
            Message
              { msgLength = msgLenType
              , msgBits = fullBits
              }

-- | Helper to calculate average delta across magnitude samples
calcAverageDelta :: V.Vector Word16 -> Int -> MessageLength -> Double
calcAverageDelta mags start msgLenType =
  let msglen = case msgLenType of
        ShortMessage -> shortMsgBits `div` 8
        LongMessage -> longMsgBits `div` 8
      !delta =
        sum
          [ abs
              ( fromIntegral (mags V.! (start + i))
                  - fromIntegral (mags V.! (start + i + 1))
                  :: Int
              )
          | i <- [0, 2 .. msglen * 8 * 2 - 1]
          ]
  in fromIntegral delta / fromIntegral (msglen * 4)

-- | Convert list of bits to Word8
bitsToWord8 :: [Bool] -> Word8
bitsToWord8 bits =
  let bitValues =
        zipWith
          (\b p -> if b then (2 :: Word8) ^ p else 0)
          (reverse bits)
          [(0 :: Word8) .. 7]
  in sum bitValues

-- | Find all valid messages in a magnitude vector
detectMessages :: V.Vector Word16 -> [Message]
detectMessages mags = go 0 []
 where
  go pos acc
    | pos + preambleUs * 2 + shortMsgBits * 2 >= V.length mags = reverse acc
    | otherwise =
        if detectPreamble mags pos
          then case demodulateMessage mags (pos + preambleUs * 2) of
            Just msg ->
              let skipLen = case msgLength msg of
                    ShortMessage -> shortMsgBits
                    LongMessage -> longMsgBits
              in go (pos + preambleUs * 2 + skipLen * 2) (msg : acc)
            Nothing -> go (pos + 1) acc
          else go (pos + 1) acc

-- | Main processing function
process :: BS.ByteString -> [Message]
process = detectMessages . computeMagnitudeVector
