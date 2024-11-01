module ModeS.Types
    ( MessageLength(..)
    , Message(..)
    , DecodedMessage(..)
    , DownlinkFormat(..)
    , ParityStatus(..)
    ) where

import Data.Word (Word8, Word32)

data MessageLength = ShortMessage | LongMessage
    deriving (Show, Eq)

-- Raw demodulated message
data Message = Message
    { msgLength :: !MessageLength
    , msgBits :: [Bool]
    } deriving (Show, Eq)

-- Represents the different downlink format types
data DownlinkFormat
    = DFShortAirAir        -- DF 0
    | DFSurveillanceAlt    -- DF 4
    | DFSurveillanceId     -- DF 5
    | DFAllCallReply       -- DF 11
    | DFLongAirAir         -- DF 16
    | DFExtendedSquitter   -- DF 17
    | DFCommAAltRequest    -- DF 20
    | DFCommAIdRequest     -- DF 21
    | DFCommCELM           -- DF 24
    deriving (Show, Eq)

data ParityStatus = Valid | CorrectedError | InvalidChecksum
    deriving (Show, Eq)

-- Decoded and validated message
data DecodedMessage = DecodedMessage
    { decodedDF :: !DownlinkFormat
    , decodedICAO :: !Word32      -- 24-bit ICAO address
    , decodedParity :: !ParityStatus
    , decodedPayload :: ![Word8]  -- Decoded payload bytes
    } deriving (Show, Eq)
