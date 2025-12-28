module ModeS.Decoder where

import Data.Bits
import Data.Char (chr)
import Data.Word
import Data.Maybe

import ModeS.Types

-- | Main decoder function for Mode S messages
decode :: VerifiedMessage -> DecodedMessage
decode msg =
  DecodedMessage
    { msgFormat = verifiedDF msg
    , msgCommon = decodeCommonFields msg
    , msgSpecific = decodeSpecificFields msg
    }

-- | Decode the common fields from a verified Mode S message
decodeCommonFields :: VerifiedMessage -> CommonFields
decodeCommonFields VerifiedMessage{verifiedICAO = icao} =
  CommonFields
    { icaoAddress = icao
    }

-- | Decode altitude if this message type contains it
decodeAltitude :: VerifiedMessage -> Maybe Altitude
decodeAltitude VerifiedMessage{verifiedDF = dfType, verifiedPayload = payload} =
  case dfType of
    -- DF 0, 4, 16, 20 contain 13-bit altitude
    df
      | df
          `elem` [ DFShortAirSurveillance
                 , DFSurveillanceAlt
                 , DFLongAirAir
                 , DFCommAAltRequest
                 ] ->
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
  let mBit = testBit (payload !! 3) 6 -- M bit at bit 6
      qBit = testBit (payload !! 3) 4 -- Q bit at bit 4
  in if not mBit && qBit
       then -- Standard altitude coding
         let n =
               ((fromIntegral (payload !! 2) .&. 0x1F) `shiftL` 6)
                 .|. ((fromIntegral (payload !! 3) .&. 0x80) `shiftR` 2)
                 .|. ((fromIntegral (payload !! 3) .&. 0x20) `shiftR` 1)
                 .|. (fromIntegral (payload !! 3) .&. 0x0F)
         in Altitude
              { altValue = n * 25 - 1000 -- Each unit = 25ft, -1000ft offset
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
  let qBit = testBit (payload !! 5) 0 -- Q bit is LSB of byte 5
  in if qBit
       then -- Standard altitude coding
         let n =
               ((fromIntegral (payload !! 5) `shiftR` 1) `shiftL` 4)
                 .|. ((fromIntegral (payload !! 6) .&. 0xF0) `shiftR` 4)
         in Altitude
              { altValue = n * 25 - 1000 -- Each unit = 25ft, -1000ft offset
              , altUnit = Feet
              }
       else -- Not implemented: Special coding
         Altitude
           { altValue = 0
           , altUnit = Feet
           }

-- | Helper to decode utility message (UM) field
decodeUM :: [Word8] -> Int
decodeUM payload =
  ((fromIntegral (payload !! 1) .&. 0x07) `shiftL` 3)
    .|. ((fromIntegral (payload !! 2) `shiftR` 5) .&. 0x07)

-- | Decode identity/squawk code if present (in DF 4,5,20,21)
decodeIdentityField :: [Word8] -> Int
decodeIdentityField payload =
  let a =
        ((fromIntegral (payload !! 3) .&. 0x80) `shiftR` 5)
          .|. ((fromIntegral (payload !! 2) .&. 0x02) `shiftR` 0)
          .|. ((fromIntegral (payload !! 2) .&. 0x08) `shiftR` 3)
      b =
        ((fromIntegral (payload !! 3) .&. 0x02) `shiftL` 1)
          .|. ((fromIntegral (payload !! 3) .&. 0x08) `shiftR` 2)
          .|. ((fromIntegral (payload !! 3) .&. 0x20) `shiftR` 5)
      c =
        ((fromIntegral (payload !! 2) .&. 0x01) `shiftL` 2)
          .|. ((fromIntegral (payload !! 2) .&. 0x04) `shiftR` 1)
          .|. ((fromIntegral (payload !! 2) .&. 0x10) `shiftR` 4)
      d =
        ((fromIntegral (payload !! 3) .&. 0x01) `shiftL` 2)
          .|. ((fromIntegral (payload !! 3) .&. 0x04) `shiftR` 1)
          .|. ((fromIntegral (payload !! 3) .&. 0x10) `shiftR` 4)
  in a * 1000 + b * 100 + c * 10 + d

-- | Decode format-specific fields based on message type
decodeSpecificFields :: VerifiedMessage -> Maybe MessageSpecific
decodeSpecificFields msg@VerifiedMessage{verifiedDF = df, verifiedPayload = payload} =
  case df of
    DFAllCallReply -> (fmap DF11Fields . fromCA) . (.&. 0x07) =<< listToMaybe payload
    DFSurveillanceAlt -> decodeDF45Fields msg
    DFSurveillanceId -> decodeDF45Fields msg
    DFCommAAltRequest -> decodeDF45Fields msg
    DFCommAIdRequest -> decodeDF45Fields msg
    DFExtendedSquitter ->
      let meType = payload !! 4 `shiftR` 3
      in case meType of
           t
             | t >= 1 && t <= 4 ->
                 Just
                   $ DF17Fields
                     { esType = AircraftID
                     , esData = ESAircraftID $ decodeAircraftIdentification payload
                     }
           t
             | t >= 5 && t <= 8 ->
                 Just
                   $ DF17Fields
                     { esType = SurfacePos
                     , esData = ESSurfacePos $ decodeSurfacePosition payload
                     }
           t
             | t >= 9 && t <= 18 ->
                 Just
                   $ DF17Fields
                     { esType = AirbornePos
                     , esData =
                         ESAirbornePos
                           (decodeAirbornePosition payload)
                           (decodeAC12Field payload)
                     }
           19 -> case decodeAirborneVelocity payload of
             Just vel ->
               Just
                 $ DF17Fields
                   { esType = AirborneVel
                   , esData = ESAirborneVel vel
                   }
             Nothing -> Nothing
           _ -> Nothing -- TODO Handle other types of DF17
    _ -> Nothing -- TODO Handle other DFs later

-- | Decode fields specific to DF4,5,20,21
decodeDF45Fields :: VerifiedMessage -> Maybe MessageSpecific
decodeDF45Fields VerifiedMessage{verifiedDF = dfType, verifiedPayload = payload} =
  let fs = payload !! 0 .&. 0x07 -- Flight status
      dr = (payload !! 1 `shiftR` 3) .&. 0x1F -- Downlink request
      um = decodeUM payload -- Utility message
      status = fromMaybe NormalAirborne $ fromFS fs
  in case dfType of
       -- DF4/20: Altitude reply
       df
         | df == DFSurveillanceAlt || df == DFCommAAltRequest ->
             Just
               $ DF420Fields
                 { flightStatus = status
                 , downlinkRequest = fromIntegral dr
                 , utilityMsg = um
                 , altitude = decodeAC13Field payload
                 }
       -- DF5/21: Identity reply
       df
         | df == DFSurveillanceId || df == DFCommAIdRequest ->
             Just
               $ DF521Fields
                 { flightStatus = status
                 , downlinkRequest = fromIntegral dr
                 , utilityMsg = um
                 , identity = decodeIdentityField payload
                 }
       _ -> Nothing

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
  _ -> ReservedAircraft

-- | Extract the ME field from payload for DF17
getMEField :: [Word8] -> [Word8]
getMEField payload = take 7 $ drop 4 payload

-- | Decode aircraft identification from ME field bytes
decodeAircraftIdentification :: [Word8] -> AircraftIdentification
decodeAircraftIdentification payload =
  let meField = getMEField payload
      -- Get TC and CA from first byte of ME field
      tc = meField !! 0 `shiftR` 3 -- First 5 bits (TC)
      ca = meField !! 0 .&. 0x07 -- Last 3 bits (CA)

      -- Extract 6-bit characters starting from second byte
      -- Each character is 6 bits, aligned to byte boundaries
      c1 = fromIntegral $ (meField !! 1 `shiftR` 2) .&. 0x3F
      c2 =
        fromIntegral
          $ ((meField !! 1 .&. 0x03) `shiftL` 4)
            .|. ((meField !! 2 `shiftR` 4) .&. 0x0F)
      c3 =
        fromIntegral
          $ ((meField !! 2 .&. 0x0F) `shiftL` 2)
            .|. ((meField !! 3 `shiftR` 6) .&. 0x03)
      c4 = fromIntegral $ meField !! 3 .&. 0x3F
      c5 = fromIntegral $ (meField !! 4 `shiftR` 2) .&. 0x3F
      c6 =
        fromIntegral
          $ ((meField !! 4 .&. 0x03) `shiftL` 4)
            .|. ((meField !! 5 `shiftR` 4) .&. 0x0F)
      c7 =
        fromIntegral
          $ ((meField !! 5 .&. 0x0F) `shiftL` 2)
            .|. ((meField !! 6 `shiftR` 6) .&. 0x03)
      c8 = fromIntegral $ meField !! 6 .&. 0x3F

      -- Convert to string using exact mapping:
      -- A-Z : 1-26
      -- 0-9 : 48-57
      -- Space: 32
      decodeChar :: Word8 -> Char
      decodeChar n
        | n >= 1 && n <= 26 = chr (fromIntegral n + 64) -- A-Z: 1-26 maps to 65-90
        | n >= 48 && n <= 57 = chr (fromIntegral n) -- 0-9: 48-57 maps directly
        | n == 32 = ' ' -- Space
        | otherwise = ' '

      callsign =
        reverse . dropWhile (== ' ') . reverse
          $ map decodeChar [c1, c2, c3, c4, c5, c6, c7, c8]
  in AircraftIdentification
       { aircraftCategory = decodeCategory tc ca
       , flightNumber = callsign
       }

-- | Extract raw latitude and longitude from ME field bytes
decodeCPRCoordinates :: [Word8] -> RawCPRCoordinates
decodeCPRCoordinates payload =
  let
    -- Extract 17 bits each for lat/lon directly matching C implementation
    rawLat :: Word32
    rawLat =
      ((fromIntegral (payload !! 6) .&. 0x03) `shiftL` 15)
        .|. (fromIntegral (payload !! 7) `shiftL` 7)
        .|. (fromIntegral (payload !! 8) `shiftR` 1)

    rawLon :: Word32
    rawLon =
      ((fromIntegral (payload !! 8) .&. 0x01) `shiftL` 16)
        .|. (fromIntegral (payload !! 9) `shiftL` 8)
        .|. fromIntegral (payload !! 10)
  in
    RawCPRCoordinates
      { rawLatitude = fromIntegral rawLat
      , rawLongitude = fromIntegral rawLon
      }

-- | Create base Position from ME field bytes
decodePosition :: [Word8] -> Position
decodePosition payload =
  let coords = decodeCPRCoordinates payload
      meField = getMEField payload
      -- Extract odd/even flag (F) and UTC flag (T)
      f = testBit (meField !! 2) 2 -- F bit (odd/even) at bit 2
      t = testBit (meField !! 2) 3 -- T bit (UTC sync) at bit 3
  in Position
       { posCoordinates = coords
       , posOddFormat = f -- True = Odd frame, False = Even frame
       , posUTCSync = t -- UTC timing status
       }

-- | Decode airborne position (now just uses base position)
decodeAirbornePosition :: [Word8] -> AirbornePosition
decodeAirbornePosition = decodePosition

-- | Decode surface movement field
decodeSurfaceMovement :: Word8 -> Word8 -> SurfaceMovement
decodeSurfaceMovement movByte trkByte =
  let mov :: Int
      mov = fromIntegral $ movByte .&. 0x7F

      speed :: Double
      speed = case mov of
        0 -> 0 -- Not available
        1 -> 0 -- Stopped
        n | n <= 8 -> fromIntegral (n - 1) * 0.125 -- 0.125 kt steps
        n | n <= 12 -> 1 + fromIntegral (n - 9) * 0.25 -- 0.25 kt steps
        n | n <= 38 -> 2 + fromIntegral (n - 13) * 0.5 -- 0.5 kt steps
        n | n <= 93 -> 15 + fromIntegral (n - 39) -- 1 kt steps
        n | n <= 108 -> 70 + fromIntegral (n - 94) * 2 -- 2 kt steps
        n | n <= 123 -> 100 + fromIntegral (n - 109) * 5 -- 5 kt steps
        124 -> 175 -- >= 175 kt
        _ -> 0 -- Reserved
      trkValid = testBit trkByte 7 -- Status bit for track
      trkRaw = fromIntegral $ trkByte .&. 0x7F :: Float
      track = trkRaw * (360.0 / 128.0) -- Convert to degrees
  in SurfaceMovement
       { surfaceSpeed = round speed
       , surfaceTrack = track
       , surfaceTrackValid = trkValid
       }

-- | Decode surface position from ME field bytes
decodeSurfacePosition :: [Word8] -> SurfacePosition
decodeSurfacePosition payload =
  let meField = getMEField payload

      -- Get Movement from bits 6-12 (7 bits)
      movValue =
        ((fromIntegral (meField !! 0) .&. 0x07) `shiftL` 4)
          .|. ((fromIntegral (meField !! 1) `shiftR` 4) .&. 0x0F)

      -- Get ground track value from bits 14-20 (7 bits)
      trackByte =
        ((fromIntegral (meField !! 1) .&. 0x0F) `shiftL` 4)
          .|. ((fromIntegral (meField !! 2) `shiftR` 4) .&. 0x0F)

      mov = decodeSurfaceMovement movValue trackByte

      -- Get base position
      pos = decodePosition payload
  in SurfacePosition
       { surfacePosition = pos
       , surfaceMovement = mov
       }

-- | Decode airborne velocity from ME field bytes
decodeAirborneVelocity :: [Word8] -> Maybe Velocity
decodeAirborneVelocity msg =
  let meType = msg !! 4 `shiftR` 3
      subType = msg !! 4 .&. 0x07
  in if meType == 19 && subType >= 1 && subType <= 4
       then case subType of
         1 -> Just $ decodeGroundVelocity msg
         2 -> Just $ decodeGroundVelocity msg
         3 -> Just $ decodeAirVelocity msg
         4 -> Just $ decodeAirVelocity msg
         _ -> Nothing
       else Nothing

-- | Decode ground velocity (subtype 1-2)
decodeGroundVelocity :: [Word8] -> Velocity
decodeGroundVelocity msg =
  let
    -- Extract EW/NS velocities
    ewDir = testBit (msg !! 5) 2
    ewVel =
      ((fromIntegral (msg !! 5 :: Word8) .&. 0x03 :: Int) `shiftL` 8)
        .|. fromIntegral (msg !! 6 :: Word8)
        :: Int
    nsDir = testBit (msg !! 7) 7
    nsVel =
      ((fromIntegral (msg !! 7 :: Word8) .&. 0x7F :: Int) `shiftL` 3)
        .|. ((fromIntegral (msg !! 8 :: Word8) .&. 0xE0 :: Int) `shiftR` 5)
        :: Int

    -- Apply directions
    ewVelSigned = fromIntegral (if ewDir then -ewVel else ewVel) :: Float
    nsVelSigned = fromIntegral (if nsDir then -nsVel else nsVel) :: Float

    -- Calculate speed and track
    speed = round $ sqrt (fromIntegral $ nsVel * nsVel + ewVel * ewVel :: Float)
    heading =
      if speed > 0
        then
          let h = atan2 ewVelSigned nsVelSigned * 360.0 / (2.0 * pi)
          in if h < 0 then h + 360 else h
        else 0

    -- Extract vertical rate
    vr =
      ((fromIntegral (msg !! 8 :: Word8) .&. 0x07 :: Int) `shiftL` 6)
        .|. ((fromIntegral (msg !! 9 :: Word8) .&. 0xFC :: Int) `shiftR` 2)
    vrSign = testBit (msg !! 8) 3
    vrValue = (if vrSign then -1 else 1) * (vr - 1) * 64
  in
    GroundVelocity speed heading vrValue

-- | Decode air velocity (subtype 3-4)
decodeAirVelocity :: [Word8] -> Velocity
decodeAirVelocity msg =
  let headingValid = testBit (msg !! 5) 2
      headingRaw =
        ((fromIntegral (msg !! 5 :: Word8) .&. 0x03 :: Int) `shiftL` 5)
          .|. ((fromIntegral (msg !! 6 :: Word8) `shiftR` 3) .&. 0x1F :: Int)
      heading = (360.0 / 128.0) * fromIntegral headingRaw
  in AirVelocity heading headingValid
