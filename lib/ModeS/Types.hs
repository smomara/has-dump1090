{-# LANGUAGE LambdaCase #-}

module ModeS.Types
    ( MessageLength(..)
    , Message(..)
    , VerifiedMessage(..)
    , DecodedMessage(..)
    , DownlinkFormat(..)
    , ParityStatus(..)
    , TransponderCapability(..)
    , ExtendedSquitterType(..)
    , AircraftType(..)
    , AltitudeType(..)
    , VelocityType(..)
    , Position(..)
    , FlightStatus(..)
    , AircraftStatus(..)
    , fromCA
    ) where

import Data.Word
import Data.Int

data MessageLength = ShortMessage | LongMessage
    deriving (Show, Eq)

-- Raw demodulated message
data Message = Message
    { msgLength :: !MessageLength
    , msgBits :: [Bool]
    } deriving (Show, Eq)

data ParityStatus = Valid | CorrectedError | InvalidChecksum
    deriving (Show, Eq)

-- Represents the different downlink format types
data DownlinkFormat
    = DFShortAirSurveillance    -- DF 0
    | DFSurveillanceAlt         -- DF 4
    | DFSurveillanceId          -- DF 5
    | DFAllCallReply            -- DF 11
    | DFLongAirAir              -- DF 16
    | DFExtendedSquitter        -- DF 17
    | DFCommAAltRequest         -- DF 20
    | DFCommAIdRequest          -- DF 21
    | DFCommCELM                -- DF 24
    deriving (Show, Eq)

-- Validated message
data VerifiedMessage = VerifiedMessage
    { verifiedDF :: !DownlinkFormat
    , verifiedICAO :: !Word32      -- 24-bit ICAO address
    , verifiedParity :: !ParityStatus
    , verifiedPayload :: ![Word8]  -- Decoded payload bytes
    } deriving (Show, Eq)

-- Aircraft Types for DF17
data AircraftType
    = Reserved
    | SurfaceEmergencyVehicle
    | SurfaceServiceVehicle
    | GroundObstruction
    | AircraftD  -- Set D
    | AircraftC  -- Set C
    | AircraftB  -- Set B
    | AircraftA  -- Set A
    deriving (Show, Eq)

-- Altitude types
data AltitudeType
    = BarometricAlt
    | GeometricAlt
    deriving (Show, Eq)

-- Velocity types
data VelocityType
    = GroundSpeedSubsonic  -- Subsonic (<0.5 Mach)
    | GroundSpeedSupersonic -- Supersonic (>0.5 Mach)
    | AirspeedSubsonic
    | AirspeedSupersonic
    deriving (Show, Eq)

-- Position information
data Position = Position
    { latitude :: !Double        -- Latitude in degrees
    , longitude :: !Double       -- Longitude in degrees
    , altitude :: !Int          -- Altitude in feet
    , altType :: !AltitudeType  -- Type of altitude measurement
    , isValid :: !Bool          -- Position validity flag
    } deriving (Show, Eq)

-- Flight status
data FlightStatus
    = NormalAirborne
    | NormalGround
    | AlertAirborne
    | AlertGround
    | AlertSPI           -- Special Position Identification
    | SPIOnly
    | StatusReserved1
    | StatusReserved2
    deriving (Show, Eq)

-- Aircraft emergency/priority status
data AircraftStatus
    = NoEmergency
    | GeneralEmergency
    | MedicalEmergency
    | MinimumFuel
    | NoCommunications
    | UnlawfulInterference
    | DownedAircraft
    deriving (Show, Eq)

-- Extended squitter message types (DF17)
data ExtendedSquitterType
    = ESAircraftIdentification
        { aircraftType :: !AircraftType
        , flightNumber :: !String
        }
    | ESSurfacePosition
        { position :: !Position
        , groundSpeed :: !Int     -- in knots
        , groundTrack :: !Int     -- in degrees
        }
    | ESAirbornePosition
        { position :: !Position
        , posVerticalRate :: !(Maybe Int) -- feet/minute
        }
    | ESAirborneVelocity
        { velocityType :: !VelocityType
        , speed :: !Int           -- in knots
        , velVerticalRate :: !Int    -- in feet/minute
        , heading :: !Int         -- in degrees
        , speedType :: !Bool      -- True = Ground Speed, False = Airspeed
        }
    | ESOperationalStatus
        { aircraftStatus :: !AircraftStatus
        , capabilities :: ![String]
        }
    | ESUnknownType
        { meType :: !Word8
        , meSubtype :: !Word8
        }
    deriving (Show, Eq)

-- Transponder capability
data TransponderCapability
    = Level1               -- CA = 0
    | Level2              -- CA = 1
    | Level3              -- CA = 2
    | Level4              -- CA = 3
    | Level2PlusGround    -- CA = 4
    | Level2PlusAirborne  -- CA = 5
    | Level2PlusAny       -- CA = 6
    | Level7              -- CA = 7
    deriving (Show, Eq)

fromCA :: Word8 -> Maybe TransponderCapability
fromCA = \case
    0 -> Just Level1
    1 -> Just Level2
    2 -> Just Level3
    3 -> Just Level4
    4 -> Just Level2PlusGround
    5 -> Just Level2PlusAirborne
    6 -> Just Level2PlusAny
    7 -> Just Level7
    _ -> Nothing

-- Fully decoded message
data DecodedMessage = DecodedMessage
    { decodedDF :: !DownlinkFormat
    , decodedICAO :: !Word32
    , decodedCapability :: !TransponderCapability
    -- Common fields
    , decodedFlightStatus :: !(Maybe FlightStatus)
    , decodedIdentity :: !(Maybe Int)        -- Squawk code
    , decodedAltitude :: !(Maybe Int)        -- in feet
    , decodedGroundBit :: !(Maybe Bool)      -- True = on ground
    -- Extended Squitter specific
    , decodedExtSquitter :: !(Maybe ExtendedSquitterType)
    } deriving (Show, Eq)
