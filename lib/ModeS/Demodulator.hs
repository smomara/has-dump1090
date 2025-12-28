{-# LANGUAGE BangPatterns #-}

module ModeS.Demodulator
  ( process
  , computeMagnitudeVector
  ) where

import Data.Bits (shiftR)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.Complex (Complex (..))
import Data.Complex qualified as Complex
import Data.Vector.Storable qualified as VS
import Data.Word (Word16, Word8)
import Foreign.ForeignPtr (castForeignPtr)

import ModeS.Types (Message (..), MessageLength (..), fromMessageLength)

-- | Constants
preambleSamples :: Int
preambleSamples = 8 * 2_000_000 * 1_000_000 -- assuming 2 MHz sample rate for now

totalMessageSamples :: Int
totalMessageSamples = fromMessageLength LongMessage + preambleSamples

-- | Convert raw I/Q samples into magnitude vector
computeMagnitudeVector :: BS.ByteString -> VS.Vector Word16
computeMagnitudeVector = VS.map computeMagnitude . toIQ

toIQ :: BS.ByteString -> VS.Vector (Complex Word8)
toIQ bs =
  let (fp, _, len) = BSI.toForeignPtr bs
  in VS.unsafeFromForeignPtr0 (castForeignPtr fp) (len `div` 2)

computeMagnitude :: Complex Word8 -> Word16
computeMagnitude =
  round
    . (* 360)
    . Complex.magnitude
    . fmap (subtract 127 . fromIntegral @Word8 @Double)

-- | Check if magnitudes at position form a valid preamble
detectPreamble :: VS.Vector Word16 -> Int -> Bool
detectPreamble mags pos
  | pos + 14 >= VS.length mags = False
  | otherwise =
      let
        (!mag0, !mag1, !mag2, !mag3) =
          (mags VS.! pos, mags VS.! (pos + 1), mags VS.! (pos + 2), mags VS.! (pos + 3))
        (!mag4, !mag5, !mag6, !mag7) =
          ( mags VS.! (pos + 4)
          , mags VS.! (pos + 5)
          , mags VS.! (pos + 6)
          , mags VS.! (pos + 7)
          )
        (!mag8, !mag9) = (mags VS.! (pos + 8), mags VS.! (pos + 9))

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

        spaceCheck = all (< threshold) $ map (fromIntegral . (mags VS.!)) [pos + 11 .. pos + 14]
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
demodulateInitialBits :: VS.Vector Word16 -> Int -> Int -> Maybe [Bool]
demodulateInitialBits mags startPos numBits = go 0 []
 where
  minDelta = 256 :: Word16
  go i acc
    | i >= numBits = Just (reverse acc)
    | otherwise = do
        let pos = startPos + (i * 2)
        low <- mags VS.!? pos
        high <- mags VS.!? (pos + 1)
        let delta = abs (fromIntegral low - fromIntegral high)
        if low == high
          then Nothing
          else case acc of
            (x : _) | delta < minDelta -> go (i + 1) (x : acc)
            _ -> go (i + 1) ((low > high) : acc)

-- | Demodulate remaining bits after type determination
demodulateRemainingBits
  :: VS.Vector Word16 -> Int -> Int -> [Bool] -> Maybe [Bool]
demodulateRemainingBits mags startPos totalBits initialBits = go (length initialBits) initialBits
 where
  minDelta = 256 :: Word16
  go i acc
    | i >= totalBits = Just acc
    | otherwise =
        let pos = startPos + (i * 2)
            !low = mags VS.! pos
            !high = mags VS.! (pos + 1)
            !delta = abs (fromIntegral low - fromIntegral high)
        in if low == high
             then Nothing
             else
               if i > 0 && delta < minDelta
                 then go (i + 1) (acc ++ [last acc])
                 else go (i + 1) (acc ++ [low > high])

-- | Demodulate bits from magnitude samples with length detection
demodulateMessage :: VS.Vector Word16 -> Int -> Maybe Message
demodulateMessage mags startPos = do
  initialBits <- demodulateInitialBits mags startPos 8
  msgLenType <- determineMessageLength initialBits

  let requiredBits = fromMessageLength msgLenType
  if startPos + (requiredBits * 2) > VS.length mags
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
calcAverageDelta :: VS.Vector Word16 -> Int -> MessageLength -> Double
calcAverageDelta mags start msgLenType =
  let msglen = fromMessageLength msgLenType `div` 8
      !delta =
        sum
          [ abs
              ( fromIntegral (mags VS.! (start + i))
                  - fromIntegral (mags VS.! (start + i + 1))
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
detectMessages :: VS.Vector Word16 -> [Message]
detectMessages mags = go 0 []
 where
  go pos acc
    | pos + totalMessageSamples >= VS.length mags = reverse acc
    | otherwise =
        if detectPreamble mags pos
          then case demodulateMessage mags (pos + preambleSamples * 2) of
            Just msg ->
              let skipLen = fromMessageLength (msgLength msg)
              in go (pos + preambleSamples * 2 + skipLen * 2) (msg : acc)
            Nothing -> go (pos + 1) acc
          else go (pos + 1) acc

-- | Main processing function
process :: BS.ByteString -> [Message]
process = detectMessages . computeMagnitudeVector
