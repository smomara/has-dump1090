module ModeS.Decoder
    ( decode
    ) where

import Data.Word (Word8, Word32)
import Data.Bits
import qualified Data.Vector.Unboxed as V
import ModeS.Types

-- Checksum table
checksumTable :: V.Vector Word32
checksumTable = V.fromList
    [ 0x3935ea, 0x1c9af5, 0xf1b77e, 0x78dbbf, 0xc397db, 0x9e31e9, 0xb0e2f0, 0x587178,
      0x2c38bc, 0x161c5e, 0x0b0e2f, 0xfa7d13, 0x82c48d, 0xbe9842, 0x5f4c21, 0xd05c14,
      0x682e0a, 0x341705, 0xe5f186, 0x72f8c3, 0xc68665, 0x9cb936, 0x4e5c9b, 0xd8d449,
      0x939020, 0x49c810, 0x24e408, 0x127204, 0x093902, 0x049c81, 0xfdb444, 0x7eda22,
      0x3f6d11, 0xe04c8c, 0x702646, 0x381323, 0xe3f395, 0x8e03ce, 0x4701e7, 0xdc7af7,
      0x91c77f, 0xb719bb, 0xa476d9, 0xadc168, 0x56e0b4, 0x2b705a, 0x15b82d, 0xf52612,
      0x7a9309, 0xc2b380, 0x6159c0, 0x30ace0, 0x185670, 0x0c2b38, 0x06159c, 0x030ace,
      0x018567, 0xff38b7, 0x80665f, 0xbfc92b, 0xa01e91, 0xaff54c, 0x57faa6, 0x2bfd53,
      0xea04ad, 0x8af852, 0x457c29, 0xdd4410, 0x6ea208, 0x375104, 0x1ba882, 0x0dd441,
      0xf91024, 0x7c8812, 0x3e4409, 0xe0d800, 0x706c00, 0x383600, 0x1c1b00, 0x0e0d80,
      0x0706c0, 0x038360, 0x01c1b0, 0x00e0d8, 0x00706c, 0x003836, 0x001c1b, 0xfff409,
      0x000000, 0x000000, 0x000000, 0x000000, 0x000000, 0x000000, 0x000000, 0x000000,
      0x000000, 0x000000, 0x000000, 0x000000, 0x000000, 0x000000, 0x000000, 0x000000,
      0x000000, 0x000000, 0x000000, 0x000000, 0x000000, 0x000000, 0x000000, 0x000000
    ]

-- Convert list of bits to bytes
bitsToBytes :: [Bool] -> [Word8]
bitsToBytes [] = []
bitsToBytes bits = 
    let byte = foldr (\(bit', pos) acc -> if bit' then setBit acc pos else acc) 
                     0 
                     (zip (take 8 bits) [7,6..0])
    in byte : bitsToBytes (drop 8 bits)

-- Get DF type from first byte
getDownlinkFormat :: Word8 -> Maybe DownlinkFormat
getDownlinkFormat byte = case shiftR byte 3 of
    0  -> Just DFShortAirAir
    4  -> Just DFSurveillanceAlt
    5  -> Just DFSurveillanceId
    11 -> Just DFAllCallReply
    16 -> Just DFLongAirAir
    17 -> Just DFExtendedSquitter
    20 -> Just DFCommAAltRequest
    21 -> Just DFCommAIdRequest
    24 -> Just DFCommCELM
    _  -> Nothing

-- Calculate checksum
calculateChecksum :: [Word8] -> Int -> Word32
calculateChecksum bytes numBits = 
    let offset = if numBits == 112 then 0 else (112 - 56)
        indices = [0..numBits-1]
        folder :: Word32 -> Int -> Word32
        folder crc j =
            let byte = j `div` 8
                bitIndex = j `mod` 8
                bitmask :: Word32
                bitmask = 1 `shiftL` (7 - bitIndex)
                byteVal :: Word32
                byteVal = fromIntegral (bytes !! byte)
            in if byteVal .&. bitmask /= 0
               then crc `xor` checksumTable V.! (j + offset)
               else crc
    in foldl folder 0 indices

-- Extract message CRC from the last three bytes
extractMessageCRC :: [Word8] -> Int -> Word32
extractMessageCRC bytes numBits =
    let lastIndex = numBits `div` 8 - 1
    in  (fromIntegral (bytes !! (lastIndex - 2)) `shiftL` 16) .|.
        (fromIntegral (bytes !! (lastIndex - 1)) `shiftL` 8) .|.
        fromIntegral (bytes !! lastIndex)

-- Extract ICAO address from bytes 1-3
extractICAO :: [Word8] -> Word32
extractICAO bytes =
    (fromIntegral (bytes !! 1) `shiftL` 16) .|.
    (fromIntegral (bytes !! 2) `shiftL` 8) .|.
    fromIntegral (bytes !! 3)

-- Try to fix single bit errors
fixSingleBitErrors :: [Word8] -> Int -> [Word8]
fixSingleBitErrors msg bits = go 0
  where
    go j
      | j >= bits = msg  -- No fix found, return original
      | otherwise = 
          let byteIndex = j `div` 8
              bitIndex = 7 - (j `mod` 8)
              bitmask = (1 :: Word8) `shiftL` bitIndex
              -- Create modified message with flipped bit
              modifiedMsg = take byteIndex msg ++ 
                          [(msg !! byteIndex) `xor` bitmask] ++ 
                          drop (byteIndex + 1) msg
              -- Calculate CRCs
              crc1 = extractMessageCRC modifiedMsg bits
              crc2 = calculateChecksum modifiedMsg bits
          in if crc1 == crc2
             then modifiedMsg  -- Fix found
             else go (j + 1)  -- Try next bit

-- Main decode function with debug prints
decode :: Message -> Maybe DecodedMessage
decode msg = 
    let bytes = bitsToBytes (msgBits msg)
    in do
        df <- getDownlinkFormat (head bytes)
        
        let computedCRC = calculateChecksum bytes numBits
            messageCRC = extractMessageCRC bytes numBits
            initialParity = computedCRC == messageCRC 
            numBits = case msgLength msg of
                ShortMessage -> 56
                LongMessage -> 112

            -- Attempt to fix errors if CRC fails and message type matches
            (finalBytes, finalParity) = 
                if not initialParity && 
                   (df == DFAllCallReply || df == DFExtendedSquitter)
                then
                    let fixedMsg = fixSingleBitErrors bytes numBits
                        newCRC = calculateChecksum fixedMsg numBits
                        newMsgCRC = extractMessageCRC fixedMsg numBits
                    in if newCRC == newMsgCRC
                       then (fixedMsg, True)
                       else (bytes, False)
                else (bytes, initialParity)

            parity = if finalParity 
                    then Valid 
                    else InvalidChecksum
            icao = extractICAO finalBytes

        return $ DecodedMessage
            { decodedDF = df
            , decodedICAO = icao
            , decodedParity = parity
            , decodedPayload = finalBytes
            }
