{-# LANGUAGE LambdaCase #-}

module ModeS.Types where

import Data.Word
import Data.Int

-- | Basic message length enum
data MessageLength = ShortMessage | LongMessage
    deriving (Show, Eq)

-- | Raw demodulated message before any verification
data Message = Message
    { msgLength :: !MessageLength
    , msgBits :: [Bool]
    } deriving (Show, Eq)

-- | Message parity/CRC status
data ParityStatus 
    = Valid 
    | InvalidChecksum
    deriving (Show, Eq)

-- | Different downlink format types
data DownlinkFormat
    = DFShortAirSurveillance   -- DF 0
    | DFSurveillanceAlt        -- DF 4  
    | DFSurveillanceId         -- DF 5
    | DFAllCallReply           -- DF 11
    | DFLongAirAir             -- DF 16  
    | DFExtendedSquitter       -- DF 17
    | DFCommAAltRequest        -- DF 20
    | DFCommAIdRequest         -- DF 21
    | DFCommCELM               -- DF 24
    deriving (Show, Eq)

-- | Message with verified CRC and extracted ICAO
data VerifiedMessage = VerifiedMessage
    { verifiedDF :: !DownlinkFormat     -- Downlink Format
    , verifiedICAO :: !Word32           -- 24-bit ICAO address
    , verifiedParity :: !ParityStatus   -- CRC/Parity check result
    , verifiedPayload :: ![Word8]       -- Raw payload bytes
    } deriving (Show, Eq)

-- | Aircraft altitude units
data AltitudeUnit = Feet | Meters
    deriving (Show, Eq)

-- | Altitude with units
data Altitude = Altitude
    { altValue :: !Int
    , altUnit :: !AltitudeUnit
    } deriving (Show, Eq)

-- | Geographic coordinates
data Coordinates = Coordinates
    { latitude :: !Double
    , longitude :: !Double
    } deriving (Show, Eq)

-- | Raw CPR coordinates before decoding
data RawCPRCoordinates = RawCPRCoordinates
    { rawLatitude :: !Int  
    , rawLongitude :: !Int
    } deriving (Show, Eq)

-- | Aircraft velocity components
data Velocity = Velocity
    { groundSpeed :: !Int -- In knots
    , track :: !Int      -- In degrees
    , verticalRate :: !Int -- In feet/minute
    } deriving (Show, Eq) 

-- | Vertical rate info
data VerticalRate = VerticalRate
    { vertRateSource :: !Bool -- True = Barometric, False = Geometric
    , vertRateSign :: !Bool   -- True = Up, False = Down
    , vertRateValue :: !Int   -- Actual rate value
    } deriving (Show, Eq)

-- | Flight status for DF4,5,20,21 
data FlightStatus
    = NormalAirborne
    | NormalGround  
    | AlertAirborne
    | AlertGround
    | AlertSPI      -- Alert & Special Position Identification
    | SPIOnly       -- Special Position Identification only
    | Reserved6     -- Value 6 not assigned
    | Reserved7     -- Value 7 not assigned
    deriving (Show, Eq)

-- | Transponder capability levels
data TransponderCapability
    = Level1              -- CA = 0 (Surveillance Only) 
    | Level2              -- CA = 1 (DF0,4,5,11)
    | Level3              -- CA = 2 (DF0,4,5,11,20,21)
    | Level4              -- CA = 3 (DF0,4,5,11,20,21,24)
    | Level2PlusGround    -- CA = 4 (DF0,4,5,11,20,21,24,code7 - ground)
    | Level2PlusAirborne  -- CA = 5 (DF0,4,5,11,20,21,24,code7 - airborne)
    | Level2PlusAny       -- CA = 6 (DF0,4,5,11,20,21,24,code7)
    | Level7              -- CA = 7 (Unknown)
    deriving (Show, Eq)

-- | Extended squitter message types
data ExtendedSquitterType
    = AircraftID        -- ME Type 1-4  
    | SurfacePos        -- ME Type 5-8
    | AirbornePos       -- ME Type 9-18
    | AirborneVel       -- ME Type 19
    | Reserved          -- Other types
    deriving (Show, Eq)

-- | Aircraft identification info from ME Type 1-4
data AircraftIdentification = AircraftIdentification
    { aircraftType :: !Int      -- 0-3 corresponding to type A-D
    , flightNumber :: !String   -- 8 char flight number
    } deriving (Show, Eq)

-- | Common fields shared between downlink formats  
data CommonFields = CommonFields
    { icaoAddress :: !Word32
    , altitude :: Maybe Altitude
    , identity :: Maybe Int  -- 13-bit squawk code
    } deriving (Show, Eq)

-- | Extended squitter specific fields
data ExtendedSquitterData
    = ESAircraftID !AircraftIdentification
    | ESAirbornePos !RawCPRCoordinates !Altitude
    | ESAirborneVel !Velocity
    | ESSurfacePos !RawCPRCoordinates
    deriving (Show, Eq)

-- | Format-specific fields 
data MessageSpecific
    = DF11Fields
        { capability :: !TransponderCapability
        }
    | DF17Fields  
        { esType :: !ExtendedSquitterType
        , esData :: !ExtendedSquitterData
        }
    | DF45Fields
        { flightStatus :: !FlightStatus
        , downlinkRequest :: !Int
        }
    deriving (Show, Eq)

-- | Final decoded message with all fields interpreted
data DecodedMessage = DecodedMessage
    { msgFormat :: !DownlinkFormat
    , msgCommon :: !CommonFields
    , msgSpecific :: Maybe MessageSpecific
    } deriving (Show, Eq)

-- Helper functions

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

fromFS :: Word8 -> Maybe FlightStatus  
fromFS = \case
    0 -> Just NormalAirborne
    1 -> Just NormalGround
    2 -> Just AlertAirborne 
    3 -> Just AlertGround
    4 -> Just AlertSPI
    5 -> Just SPIOnly
    6 -> Just Reserved6
    7 -> Just Reserved7
    _ -> Nothing
