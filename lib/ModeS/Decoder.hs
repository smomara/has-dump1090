{-# LANGUAGE RecordWildCards #-}

module ModeS.Decoder
    ( decode
    ) where

import ModeS.Types
import Data.Bits
import Data.Word
import Data.Maybe (fromMaybe)

-- Main decoder function
decode :: VerifiedMessage -> DecodedMessage
decode VerifiedMessage{..} = 
    let baseMsg = DecodedMessage
            { decodedDF = verifiedDF
            , decodedICAO = verifiedICAO
            , decodedCapability = fromMaybe Level1 $ fromCA (verifiedPayload !! 0 .&. 0x7)
            , decodedFlightStatus = Nothing
            , decodedIdentity = Nothing
            , decodedAltitude = Nothing
            , decodedGroundBit = Nothing
            , decodedExtSquitter = Nothing
            }
    in case verifiedDF of
        DFShortAirSurveillance -> 
            baseMsg { decodedAltitude = decodeAltitude verifiedPayload }
            
        DFSurveillanceAlt ->
            baseMsg 
                { decodedAltitude = decodeAltitude verifiedPayload
                , decodedFlightStatus = Just $ decodeFlightStatus (verifiedPayload !! 0)
                }
            
        DFSurveillanceId ->
            baseMsg
                { decodedFlightStatus = Just $ decodeFlightStatus (verifiedPayload !! 0)
                , decodedIdentity = Just $ decodeIdentity verifiedPayload
                }
            
        DFExtendedSquitter ->
            baseMsg 
                { decodedExtSquitter = Just $ decodeExtendedSquitter verifiedPayload
                }
            
        _ -> baseMsg

-- Decode altitude field (13-bit encoding)
decodeAltitude :: [Word8] -> Maybe Int
decodeAltitude payload =
    let mBit = testBit (payload !! 3) 6
        qBit = testBit (payload !! 3) 4
    in if not mBit && qBit
       then 
           let n = ((fromIntegral (payload !! 2) .&. 31) `shiftL` 6) .|.
                   ((fromIntegral (payload !! 3) .&. 0x80) `shiftR` 2) .|.
                   ((fromIntegral (payload !! 3) .&. 0x20) `shiftR` 1) .|.
                   (fromIntegral (payload !! 3) .&. 15)
           in Just $ n * 25 - 1000
       else Nothing

-- Decode Mode A identity/squawk code
decodeIdentity :: [Word8] -> Int
decodeIdentity payload =
    let a = ((fromIntegral (payload !! 3) .&. 0x80) `shiftR` 5) .|.
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

-- Decode flight status field
decodeFlightStatus :: Word8 -> FlightStatus
decodeFlightStatus w = case w .&. 0x7 of
    0 -> NormalAirborne
    1 -> NormalGround
    2 -> AlertAirborne
    3 -> AlertGround
    4 -> AlertSPI
    5 -> SPIOnly
    6 -> StatusReserved1
    7 -> StatusReserved2
    _ -> NormalAirborne

-- Decode extended squitter messages (DF17)
decodeExtendedSquitter :: [Word8] -> ExtendedSquitterType
decodeExtendedSquitter payload =
    let meType = payload !! 4 `shiftR` 3
        meSub = payload !! 4 .&. 0x7
    in case meType of
        1 -> decodeAircraftIdentification AircraftD payload
        2 -> decodeAircraftIdentification AircraftC payload
        3 -> decodeAircraftIdentification AircraftB payload
        4 -> decodeAircraftIdentification AircraftA payload
        
        5  -> decodeSurfacePosition payload
        6  -> decodeSurfacePosition payload
        7  -> decodeSurfacePosition payload
        8  -> decodeSurfacePosition payload
        
        9  -> decodeAirbornePosition payload
        10 -> decodeAirbornePosition payload
        11 -> decodeAirbornePosition payload
        12 -> decodeAirbornePosition payload
        13 -> decodeAirbornePosition payload
        14 -> decodeAirbornePosition payload
        15 -> decodeAirbornePosition payload
        16 -> decodeAirbornePosition payload
        17 -> decodeAirbornePosition payload
        18 -> decodeAirbornePosition payload
        
        19 | meSub >= 1 && meSub <= 4 -> decodeAirborneVelocity payload meSub
        
        28 | meSub == 1 -> decodeOperationalStatus payload
        
        _ -> ESUnknownType meType meSub

-- Decode aircraft identification message (placeholder)
decodeAircraftIdentification :: AircraftType -> [Word8] -> ExtendedSquitterType
decodeAircraftIdentification acType _ = ESAircraftIdentification acType "UNKNOWN"

-- Decode surface position message (placeholder)
decodeSurfacePosition :: [Word8] -> ExtendedSquitterType
decodeSurfacePosition _ =
    let pos = Position 0 0 0 BarometricAlt False
    in ESSurfacePosition pos 0 0

-- Decode airborne position message (placeholder) 
decodeAirbornePosition :: [Word8] -> ExtendedSquitterType
decodeAirbornePosition _ =
    let pos = Position 0 0 0 BarometricAlt False
    in ESAirbornePosition pos Nothing

-- Decode airborne velocity message (placeholder)
decodeAirborneVelocity :: [Word8] -> Word8 -> ExtendedSquitterType
decodeAirborneVelocity _ _ =
    ESAirborneVelocity GroundSpeedSubsonic 0 0 0 True

-- Decode aircraft operational status (placeholder)
decodeOperationalStatus :: [Word8] -> ExtendedSquitterType
decodeOperationalStatus _ = ESOperationalStatus NoEmergency []
