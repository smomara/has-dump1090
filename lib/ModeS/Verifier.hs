{-# LANGUAGE RecordWildCards #-}

module ModeS.Verifier
  ( verify
  , newIcaoCache
  , IcaoCache (..)
  , icaoCacheTtl
  , icaoCacheLen
  ) where

import Data.Bits
import Data.Maybe (listToMaybe)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Vector.Unboxed qualified as V
import Data.Vector.Unboxed.Mutable qualified as MV
import Data.Word (Word32, Word8)

import ModeS.Types

-- | Cache configuration
icaoCacheLen :: Int
icaoCacheLen = 1024 -- Power of two required

icaoCacheTtl :: Int
icaoCacheTtl = 60 -- Time to live in seconds

-- | Cache data structure for ICAO addresses
data IcaoCache = IcaoCache
  { cacheAddrs :: V.Vector Word32
  -- ^ ICAO addresses
  , cacheTimes :: V.Vector Word32
  -- ^ Timestamps
  , cacheSize :: Int
  -- ^ Must be power of 2
  , cacheTtl :: Int
  -- ^ Time to live in seconds
  }
  deriving Show

-- | Create new empty cache
newIcaoCache :: Int -> Int -> IcaoCache
newIcaoCache size ttl =
  IcaoCache
    { cacheAddrs = V.replicate size 0
    , cacheTimes = V.replicate size 0
    , cacheSize = size
    , cacheTtl = ttl
    }

-- | CRC lookup table for Mode S
checksumTable :: V.Vector Word32
checksumTable =
  V.fromList
    [ 0x3935ea
    , 0x1c9af5
    , 0xf1b77e
    , 0x78dbbf
    , 0xc397db
    , 0x9e31e9
    , 0xb0e2f0
    , 0x587178
    , 0x2c38bc
    , 0x161c5e
    , 0x0b0e2f
    , 0xfa7d13
    , 0x82c48d
    , 0xbe9842
    , 0x5f4c21
    , 0xd05c14
    , 0x682e0a
    , 0x341705
    , 0xe5f186
    , 0x72f8c3
    , 0xc68665
    , 0x9cb936
    , 0x4e5c9b
    , 0xd8d449
    , 0x939020
    , 0x49c810
    , 0x24e408
    , 0x127204
    , 0x093902
    , 0x049c81
    , 0xfdb444
    , 0x7eda22
    , 0x3f6d11
    , 0xe04c8c
    , 0x702646
    , 0x381323
    , 0xe3f395
    , 0x8e03ce
    , 0x4701e7
    , 0xdc7af7
    , 0x91c77f
    , 0xb719bb
    , 0xa476d9
    , 0xadc168
    , 0x56e0b4
    , 0x2b705a
    , 0x15b82d
    , 0xf52612
    , 0x7a9309
    , 0xc2b380
    , 0x6159c0
    , 0x30ace0
    , 0x185670
    , 0x0c2b38
    , 0x06159c
    , 0x030ace
    , 0x018567
    , 0xff38b7
    , 0x80665f
    , 0xbfc92b
    , 0xa01e91
    , 0xaff54c
    , 0x57faa6
    , 0x2bfd53
    , 0xea04ad
    , 0x8af852
    , 0x457c29
    , 0xdd4410
    , 0x6ea208
    , 0x375104
    , 0x1ba882
    , 0x0dd441
    , 0xf91024
    , 0x7c8812
    , 0x3e4409
    , 0xe0d800
    , 0x706c00
    , 0x383600
    , 0x1c1b00
    , 0x0e0d80
    , 0x0706c0
    , 0x038360
    , 0x01c1b0
    , 0x00e0d8
    , 0x00706c
    , 0x003836
    , 0x001c1b
    , 0xfff409
    , 0x000000
    , 0x000000
    , 0x000000
    , 0x000000
    , 0x000000
    , 0x000000
    , 0x000000
    , 0x000000
    , 0x000000
    , 0x000000
    , 0x000000
    , 0x000000
    , 0x000000
    , 0x000000
    , 0x000000
    , 0x000000
    , 0x000000
    , 0x000000
    , 0x000000
    , 0x000000
    , 0x000000
    , 0x000000
    , 0x000000
    , 0x000000
    ]

-- | Convert bit array to bytes
bitsToBytes :: [Bool] -> [Word8]
bitsToBytes [] = []
bitsToBytes bits =
  let byte =
        foldr
          (\(bitVal, pos) acc -> if bitVal then setBit acc pos else acc)
          0
          (zip (take 8 bits) [7, 6 .. 0])
  in byte : bitsToBytes (drop 8 bits)

-- | Extract downlink format from first byte
getDownlinkFormat :: Word8 -> Maybe DownlinkFormat
getDownlinkFormat byte = case shiftR byte 3 of
  0 -> Just DFShortAirSurveillance
  4 -> Just DFSurveillanceAlt
  5 -> Just DFSurveillanceId
  11 -> Just DFAllCallReply
  16 -> Just DFLongAirAir
  17 -> Just DFExtendedSquitter
  20 -> Just DFCommAAltRequest
  21 -> Just DFCommAIdRequest
  24 -> Just DFCommCELM
  _ -> Nothing

-- | Get current time in seconds
getCurrentTimeSeconds :: IO Word32
getCurrentTimeSeconds = round <$> getPOSIXTime

-- Calculate checksum
calculateChecksum :: [Word8] -> MessageLength -> Word32
calculateChecksum bytes bitLength =
  let offset = case bitLength of
        LongMessage -> 0
        ShortMessage -> 56
      indices = [0 .. fromMessageLength bitLength - 1]
      folder :: Word32 -> Int -> Word32
      folder crc j =
        let byte = j `div` 8
            bitPos = j `mod` 8
            bitmask :: Word32
            bitmask = 1 `shiftL` (7 - bitPos)
            byteVal :: Word32
            byteVal = fromIntegral (bytes !! byte)
        in if byteVal .&. bitmask /= 0
             then crc `xor` checksumTable V.! (j + offset)
             else crc
  in foldl folder 0 indices

-- | Extract message CRC from last three bytes
extractMessageCRC :: [Word8] -> MessageLength -> Word32
extractMessageCRC bytes bitLength =
  let lastIndex = fromMessageLength bitLength `div` 8 - 1
  in (fromIntegral (bytes !! (lastIndex - 2)) `shiftL` 16)
       .|. (fromIntegral (bytes !! (lastIndex - 1)) `shiftL` 8)
       .|. fromIntegral (bytes !! lastIndex)

-- | Extract ICAO address from bytes 1-3
extractICAO :: [Word8] -> Word32
extractICAO bytes =
  (fromIntegral (bytes !! 1) `shiftL` 16)
    .|. (fromIntegral (bytes !! 2) `shiftL` 8)
    .|. fromIntegral (bytes !! 3)

-- | Add an ICAO address to the cache
addRecentlySeenIcaoAddr :: Word32 -> IcaoCache -> IO IcaoCache
addRecentlySeenIcaoAddr addr cache = do
  currentTime <- getCurrentTimeSeconds
  let idx = icaoCacheHash addr (cacheSize cache)
      newAddrs = V.modify (\v -> MV.write v idx addr) (cacheAddrs cache)
      newTimes = V.modify (\v -> MV.write v idx currentTime) (cacheTimes cache)
  return $ cache{cacheAddrs = newAddrs, cacheTimes = newTimes}

-- | Hash function for ICAO cache
icaoCacheHash :: Word32 -> Int -> Int
icaoCacheHash a size =
  let a1 = ((shiftR a 16 `xor` a) * 0x45d9f3b) .&. 0xffffffff
      a2 = ((shiftR a1 16 `xor` a1) * 0x45d9f3b) .&. 0xffffffff
      a3 = (shiftR a2 16 `xor` a2) .&. 0xffffffff
  in fromIntegral $ a3 .&. (fromIntegral size - 1)

-- | Try to fix single bit errors
fixSingleBitErrors :: [Word8] -> MessageLength -> Maybe (Int, [Word8])
fixSingleBitErrors msg ml = go 0
 where
  go j
    | j >= fromMessageLength ml = Nothing
    | otherwise =
        let byteIndex = j `div` 8
            bitIndex = 7 - (j `mod` 8)
            bitmask = (1 :: Word8) `shiftL` bitIndex
            modifiedMsg =
              take byteIndex msg
                ++ [(msg !! byteIndex) `xor` bitmask]
                ++ drop (byteIndex + 1) msg
            crc1 = extractMessageCRC modifiedMsg ml
            crc2 = calculateChecksum modifiedMsg ml
        in if crc1 == crc2
             then Just (j, modifiedMsg)
             else go (j + 1)

-- | Fix two bit errors (only for DF17)
fixTwoBitsErrors :: [Word8] -> MessageLength -> Maybe ([Int], [Word8])
fixTwoBitsErrors msg ml = go 0 0
 where
  go i j
    | i >= fromMessageLength ml = Nothing
    | j >= fromMessageLength ml = go (i + 1) (i + 2)
    | i == j = go i (j + 1)
    | otherwise =
        let (byteIndex1, bitIndex1) = (i `div` 8, 7 - (i `mod` 8))
            (byteIndex2, bitIndex2) = (j `div` 8, 7 - (j `mod` 8))
            bitmask1 = (1 :: Word8) `shiftL` bitIndex1
            bitmask2 = (1 :: Word8) `shiftL` bitIndex2

            modifiedMsg =
              if byteIndex1 == byteIndex2
                then
                  let combined = bitmask1 .|. bitmask2
                  in take byteIndex1 msg
                       ++ [(msg !! byteIndex1) `xor` combined]
                       ++ drop (byteIndex1 + 1) msg
                else
                  let msg1 =
                        take byteIndex1 msg
                          ++ [(msg !! byteIndex1) `xor` bitmask1]
                          ++ drop (byteIndex1 + 1) msg
                      msg2 =
                        take byteIndex2 msg1
                          ++ [(msg1 !! byteIndex2) `xor` bitmask2]
                          ++ drop (byteIndex2 + 1) msg1
                  in msg2

            crc1 = extractMessageCRC modifiedMsg ml
            crc2 = calculateChecksum modifiedMsg ml
        in if crc1 == crc2
             then Just ([i, j], modifiedMsg)
             else go i (j + 1)

-- \| Brute force ICAO address recovery

-- | Pure decoding function without cache operations
verifyPure :: Message -> Maybe VerifiedMessage
verifyPure Message{..} = do
  let bytes = bitsToBytes msgBits

  df <- getDownlinkFormat =<< listToMaybe bytes
  let computedCRC = calculateChecksum bytes msgLength
      messageCRC = extractMessageCRC bytes msgLength
      initialParity = computedCRC == messageCRC

      -- Try error correction for DF11/17 if needed
      (_, correctedBytes, correctedParity) =
        if not initialParity
          && (df == DFAllCallReply || df == DFExtendedSquitter)
          then case fixSingleBitErrors bytes msgLength of
            Just (bitPos, fixed) -> ([bitPos], fixed, True)
            Nothing ->
              if df == DFExtendedSquitter
                then case fixTwoBitsErrors bytes msgLength of
                  Just (bits', fixed) -> (bits', fixed, True)
                  Nothing -> ([], bytes, initialParity)
                else ([], bytes, initialParity)
          else ([], bytes, initialParity)
  return
    VerifiedMessage
      { verifiedDF = df
      , verifiedICAO = extractICAO correctedBytes
      , verifiedParity = if correctedParity then Valid else InvalidChecksum
      , verifiedPayload = correctedBytes
      }

-- | Check if address exists in cache
checkCache :: Word32 -> IcaoCache -> IO Bool
checkCache addr cache = do
  currentTime <- getCurrentTimeSeconds
  let idx = icaoCacheHash addr (cacheSize cache)
      storedAddr = cacheAddrs cache V.! idx
      storedTime = cacheTimes cache V.! idx
  return
    $ storedAddr == addr
      && currentTime - storedTime <= fromIntegral (cacheTtl cache)

-- | Update cache with new address
updateCache :: VerifiedMessage -> IcaoCache -> IO IcaoCache
updateCache msg cache =
  case verifiedDF msg of
    -- Only update cache for DF11 and DF17 with valid parity
    df
      | df `elem` [DFAllCallReply, DFExtendedSquitter]
          && verifiedParity msg == Valid ->
          addRecentlySeenIcaoAddr (verifiedICAO msg) cache
    _ -> return cache

-- | Try to recover ICAO address using cache
recoverAddress :: Message -> VerifiedMessage -> IcaoCache -> IO (Maybe Word32)
recoverAddress Message{msgLength} dm cache =
  if dfRequiresBruteForce (verifiedDF dm)
    then do
      let bytes = verifiedPayload dm
          lastByte = (fromMessageLength msgLength `div` 8) - 1
          crc = calculateChecksum bytes msgLength
          recoveredAddr =
            (fromIntegral (bytes !! (lastByte - 2)) `xor` ((crc `shiftR` 16) .&. 0xff))
              `shiftL` 16
              .|. (fromIntegral (bytes !! (lastByte - 1)) `xor` ((crc `shiftR` 8) .&. 0xff))
                `shiftL` 8
              .|. (fromIntegral (bytes !! lastByte) `xor` (crc .&. 0xff))

      isInCache <- checkCache recoveredAddr cache
      return $ if isInCache then Just recoveredAddr else Nothing
    else return Nothing
 where
  dfRequiresBruteForce :: DownlinkFormat -> Bool
  dfRequiresBruteForce df =
    df
      `elem` [ DFShortAirSurveillance
             , DFSurveillanceAlt
             , DFSurveillanceId
             , DFLongAirAir
             , DFCommAAltRequest
             , DFCommAIdRequest
             , DFCommCELM
             ]

-- | Main decode function that combines pure decoding with cache operations
verify :: Message -> IcaoCache -> IO (Maybe (VerifiedMessage, IcaoCache))
verify msg cache = case verifyPure msg of
  Nothing -> return Nothing
  Just verifiedMsg -> do
    -- Try to recover address if needed
    finalMsg <-
      if verifiedParity verifiedMsg == InvalidChecksum
        then do
          mAddr <- recoverAddress msg verifiedMsg cache
          return $ case mAddr of
            Just addr ->
              verifiedMsg
                { verifiedICAO = addr
                , verifiedParity = Valid
                }
            Nothing -> verifiedMsg
        else return verifiedMsg

    -- Update cache if needed
    newCache <- updateCache finalMsg cache
    return $ Just (finalMsg, newCache)
