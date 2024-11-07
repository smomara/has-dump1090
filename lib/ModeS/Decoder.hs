module ModeS.Decoder where

import ModeS.Types
import Data.Word
import Data.Bits

-- | Main decoder function for Mode S messages
decode :: VerifiedMessage -> DecodedMessage
decode msg = DecodedMessage
    { msgFormat = verifiedDF msg
    , msgCommon = decodeCommonFields msg
    , msgSpecific = Nothing -- TODO
    }

-- | Decode the common fields from a verified Mode S message
decodeCommonFields :: VerifiedMessage -> CommonFields
decodeCommonFields msg@VerifiedMessage{verifiedICAO = icao} = CommonFields
    { icaoAddress = icao
    , altitude = decodeAltitude msg
    , identity = decodeIdentity msg
    }

-- | Decode altitude if this message type contains it
decodeAltitude :: VerifiedMessage -> Maybe Altitude
decodeAltitude msg@VerifiedMessage{verifiedDF = df, verifiedPayload = payload} =
    case df of
        -- DF 0, 4, 16, 20 contain 13-bit altitude
        df | df `elem` [DFShortAirSurveillance, DFSurveillanceAlt, 
                       DFLongAirAir, DFCommAAltRequest] ->
            Just $ decodeAC13Field payload

        -- For DF17, only decode altitude for airborne position messages (ME type 9-18)
        DFExtendedSquitter -> 
            let meType = payload !! 4 `shiftR` 3
            in if meType >= 9 && meType <= 18
               then Just $ decodeAC12Field payload
               else Nothing

        _ -> Nothing

-- | Decode 13-bit AC altitude field (in DF 0,4,16,20)
decodeAC13Field :: [Word8] -> Altitude
decodeAC13Field payload = 
    let mBit = testBit (payload !! 3) 6  -- M bit at bit 6
        qBit = testBit (payload !! 3) 4  -- Q bit at bit 4
    in if not mBit && qBit 
       then -- Standard altitude coding
            let n = ((fromIntegral (payload !! 2) .&. 0x1F) `shiftL` 6) .|.
                    ((fromIntegral (payload !! 3) .&. 0x80) `shiftR` 2) .|.
                    ((fromIntegral (payload !! 3) .&. 0x20) `shiftR` 1) .|.
                    (fromIntegral (payload !! 3) .&. 0x0F)
            in Altitude 
                { altValue = n * 25 - 1000  -- Each unit = 25ft, -1000ft offset
                , altUnit = Feet
                }
       else -- Not implemented: Metric altitude or special coding
            Altitude 
                { altValue = 0
                , altUnit = Feet
                }

-- | Decode 12-bit AC altitude field (in DF17)
decodeAC12Field :: [Word8] -> Altitude  
decodeAC12Field payload =
    let qBit = testBit (payload !! 5) 0   -- Q bit is LSB of byte 5
    in if qBit
       then -- Standard altitude coding
            let n = ((fromIntegral (payload !! 5) `shiftR` 1) `shiftL` 4) .|.
                    ((fromIntegral (payload !! 6) .&. 0xF0) `shiftR` 4)
            in Altitude
                { altValue = n * 25 - 1000  -- Each unit = 25ft, -1000ft offset
                , altUnit = Feet
                }
       else -- Not implemented: Special coding
            Altitude 
                { altValue = 0
                , altUnit = Feet 
                }

-- | Decode identity/squawk code if present (in DF 4,5,20,21)
decodeIdentity :: VerifiedMessage -> Maybe Int
decodeIdentity msg@VerifiedMessage{verifiedDF = df, verifiedPayload = payload}
    | df `elem` [DFSurveillanceAlt, DFSurveillanceId, 
                 DFCommAAltRequest, DFCommAIdRequest] =
        Just $ let a = ((fromIntegral (payload !! 3) .&. 0x80) `shiftR` 5) .|.
                       ((fromIntegral (payload !! 2) .&. 0x02) `shiftR` 0) .|. 
                       ((fromIntegral (payload !! 2) .&. 0x08) `shiftR` 3)
                   b = ((fromIntegral (payload !! 3) .&. 0x02) `shiftL` 1) .|.
                       ((fromIntegral (payload !! 3) .&. 0x08) `shiftR` 2) .|.
                       ((fromIntegral (payload !! 3) .&. 0x20) `shiftR` 5)
                   c = ((fromIntegral (payload !! 2) .&. 0x01) `shiftL` 2) .|.
                       ((fromIntegral (payload !! 2) .&. 0x04) `shiftR` 1) .|.
                       ((fromIntegral (payload !! 2) .&. 0x10) `shiftR` 4)
                   d = ((fromIntegral (payload !! 3) .&. 0x01) `shiftL` 2) .|.
                       ((fromIntegral (payload !! 3) .&. 0x04) `shiftR` 1) .|.
                       ((fromIntegral (payload !! 3) .&. 0x10) `shiftR` 4)
               in a * 1000 + b * 100 + c * 10 + d
    | otherwise = Nothing
