module ModeS.Decoder where

import ModeS.Types
import Data.Word
import Data.Bits
import Data.Char (chr)

-- | Main decoder function for Mode S messages
decode :: VerifiedMessage -> DecodedMessage
decode msg = DecodedMessage
    { msgFormat = verifiedDF msg
    , msgCommon = decodeCommonFields msg
    , msgSpecific = decodeSpecificFields msg
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

-- | Decode format-specific fields based on message type
decodeSpecificFields :: VerifiedMessage -> Maybe MessageSpecific
decodeSpecificFields msg@VerifiedMessage{verifiedDF = df, verifiedPayload = payload} = 
    case df of
        DFExtendedSquitter ->
            let meType = payload !! 4 `shiftR` 3
            in if meType >= 1 && meType <= 4
               then Just $ DF17Fields
                    { esType = AircraftID
                    , esData = ESAircraftID $ decodeAircraftIdentification payload
                    }
               else Nothing  -- Handle other DF17 types later
        _ -> Nothing  -- Handle other DFs later

-- | Determine aircraft category from TC and CA values
decodeCategory :: Word8 -> Word8 -> AircraftCategory
decodeCategory tc ca = case (tc, ca) of
    (2, 1) -> EmergencyVehicle
    (2, 2) -> ServiceVehicle
    (2, 3) -> ServiceVehicle
    (2, _) -> GroundObstruction
    (3, 1) -> Glider
    (3, 2) -> LighterThanAir
    (3, 3) -> Parachutist
    (3, 4) -> Ultralight
    (3, 6) -> UAV
    (3, 7) -> SpaceVehicle
    (4, 1) -> LightAircraft
    (4, 2) -> MediumAircraft1
    (4, 3) -> MediumAircraft2
    (4, 4) -> HighVortex
    (4, 5) -> HeavyAircraft
    (4, 6) -> HighPerformance
    (4, 7) -> Rotorcraft
    (_, 0) -> NoCategory
    _      -> ReservedAircraft

-- | Extract the ME field from payload for DF17
getMEField :: [Word8] -> [Word8]
getMEField payload = take 7 $ drop 4 payload

-- | Decode aircraft identification from ME field bytes
decodeAircraftIdentification :: [Word8] -> AircraftIdentification
decodeAircraftIdentification payload = 
    let meField = getMEField payload
        -- Get TC and CA from first byte of ME field
        tc = meField !! 0 `shiftR` 3         -- First 5 bits (TC)
        ca = meField !! 0 .&. 0x07           -- Last 3 bits (CA)
        
        -- Extract 6-bit characters starting from second byte
        -- Each character is 6 bits, aligned to byte boundaries
        c1 = fromIntegral $ (meField !! 1 `shiftR` 2) .&. 0x3F
        c2 = fromIntegral $ ((meField !! 1 .&. 0x03) `shiftL` 4) .|.
                           ((meField !! 2 `shiftR` 4) .&. 0x0F)
        c3 = fromIntegral $ ((meField !! 2 .&. 0x0F) `shiftL` 2) .|.
                           ((meField !! 3 `shiftR` 6) .&. 0x03)
        c4 = fromIntegral $ meField !! 3 .&. 0x3F
        c5 = fromIntegral $ (meField !! 4 `shiftR` 2) .&. 0x3F
        c6 = fromIntegral $ ((meField !! 4 .&. 0x03) `shiftL` 4) .|.
                           ((meField !! 5 `shiftR` 4) .&. 0x0F)
        c7 = fromIntegral $ ((meField !! 5 .&. 0x0F) `shiftL` 2) .|.
                           ((meField !! 6 `shiftR` 6) .&. 0x03)
        c8 = fromIntegral $ meField !! 6 .&. 0x3F

        -- Convert to string using exact mapping:
        -- A-Z : 1-26
        -- 0-9 : 48-57  
        -- Space: 32
        decodeChar :: Word8 -> Char
        decodeChar n
            | n >= 1 && n <= 26  = chr (fromIntegral n + 64)    -- A-Z: 1-26 maps to 65-90
            | n >= 48 && n <= 57 = chr (fromIntegral n)         -- 0-9: 48-57 maps directly
            | n == 32  = ' '                                     -- Space
            | otherwise = ' '

        callsign = reverse . dropWhile (== ' ') . reverse $ 
                  map decodeChar [c1, c2, c3, c4, c5, c6, c7, c8]
    in AircraftIdentification
        { aircraftCategory = decodeCategory tc ca
        , flightNumber = callsign
        }
