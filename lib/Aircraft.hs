{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Aircraft 
    ( Aircraft(..)
    , AircraftState(..)
    , updateAircraft
    , pruneOldAircraft
    , newAircraftState
    ) where

import qualified Data.Map.Strict as Map
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import Data.Word (Word32)
import Data.Maybe (fromMaybe)
import qualified ModeS.Types as Mode

-- | Main state for tracking all aircraft
data AircraftState = AircraftState 
    { aircraft :: Map.Map Word32 Aircraft  -- ^ Map from ICAO to Aircraft
    , lastPrune :: POSIXTime               -- ^ Last time old aircraft were pruned
    }

-- | Information about a single aircraft
data Aircraft = Aircraft
    { icaoAddress :: !Word32               -- ^ Unique ICAO address
    , callsign :: !(Maybe String)          -- ^ Flight number/callsign
    , category :: !(Maybe Mode.AircraftCategory) -- ^ Aircraft category
    , position :: !(Maybe Mode.Position)    -- ^ Last known position
    , altitudeFt :: !(Maybe Int)           -- ^ Last known altitude in feet
    , groundSpeed :: !(Maybe Int)          -- ^ Ground speed in knots
    , track :: !(Maybe Float)              -- ^ Track angle in degrees
    , verticalRate :: !(Maybe Int)         -- ^ Vertical rate in ft/min
    , lastSeen :: !POSIXTime               -- ^ Time of last message
    } deriving (Show)

-- | Create a new empty aircraft state
newAircraftState :: IO AircraftState
newAircraftState = do
    now <- getPOSIXTime
    return $ AircraftState
        { aircraft = Map.empty
        , lastPrune = now
        }

-- | Create a new aircraft entry
newAircraft :: Word32 -> POSIXTime -> Aircraft
newAircraft icao time = Aircraft
    { icaoAddress = icao
    , callsign = Nothing
    , category = Nothing
    , position = Nothing
    , altitudeFt = Nothing
    , groundSpeed = Nothing
    , track = Nothing
    , verticalRate = Nothing
    , lastSeen = time
    }

-- | Update aircraft state with a new decoded message
updateAircraft :: AircraftState -> Mode.DecodedMessage -> IO AircraftState
updateAircraft state msg = do
    now <- getPOSIXTime
    let Mode.CommonFields{icaoAddress} = Mode.msgCommon msg
    
    -- Get existing or create new aircraft
    let existing = Map.lookup icaoAddress (aircraft state)
    let base = fromMaybe (newAircraft icaoAddress now) existing
    
    -- Update aircraft with new message data
    let updated = updateAircraftFields base msg now
    
    -- Insert updated aircraft into state
    return $ state { aircraft = Map.insert icaoAddress updated (aircraft state) }

-- | Update specific fields based on message type
updateAircraftFields :: Aircraft -> Mode.DecodedMessage -> POSIXTime -> Aircraft
updateAircraftFields ac msg now = ac 
    { lastSeen = now  -- Always update last seen time
    , callsign = newCallsign
    , category = newCategory
    , position = newPosition
    , altitudeFt = newAltitude
    , groundSpeed = newGroundSpeed
    , track = newTrack
    , verticalRate = newVerticalRate
    }
  where
    -- Extract possible updates from message
    (newCallsign, newCategory, newPosition, newAltitude, newGroundSpeed, newTrack, newVerticalRate) = 
        case Mode.msgSpecific msg of
            -- Aircraft ID (callsign)
            Just (Mode.DF17Fields{Mode.esType = Mode.AircraftID, Mode.esData = Mode.ESAircraftID ident}) ->
                ( Just $ Mode.flightNumber ident
                , Just $ Mode.aircraftCategory ident
                , position ac
                , altitudeFt ac
                , groundSpeed ac
                , track ac
                , verticalRate ac
                )
                
            -- Airborne Position
            Just (Mode.DF17Fields{Mode.esType = Mode.AirbornePos, Mode.esData = Mode.ESAirbornePos pos alt}) ->
                ( callsign ac
                , category ac
                , Just pos
                , Just $ Mode.altValue alt
                , groundSpeed ac
                , track ac
                , verticalRate ac
                )
                
            -- Surface Position
            Just (Mode.DF17Fields{Mode.esType = Mode.SurfacePos, Mode.esData = Mode.ESSurfacePos Mode.SurfacePosition{surfacePosition, surfaceMovement}}) ->
                ( callsign ac
                , category ac
                , Just surfacePosition
                , Just 0  -- On ground
                , Just $ Mode.surfaceSpeed surfaceMovement
                , if Mode.surfaceTrackValid surfaceMovement 
                  then Just $ Mode.surfaceTrack surfaceMovement
                  else track ac
                , Just 0  -- No vertical rate on ground
                )
                
            -- Airborne Velocity
            Just (Mode.DF17Fields{Mode.esType = Mode.AirborneVel, Mode.esData = Mode.ESAirborneVel vel}) ->
                case vel of
                    Mode.GroundVelocity{Mode.velSpeed, Mode.velTrack, Mode.velVRate} ->
                        ( callsign ac
                        , category ac
                        , position ac
                        , altitudeFt ac
                        , Just velSpeed
                        , Just velTrack
                        , Just velVRate
                        )
                    Mode.AirVelocity{} ->  -- We don't track air velocity for now
                        ( callsign ac
                        , category ac
                        , position ac
                        , altitudeFt ac
                        , groundSpeed ac
                        , track ac
                        , verticalRate ac
                        )
                        
            -- Altitude only messages
            Just (Mode.DF420Fields{Mode.altitude = alt}) ->
                ( callsign ac
                , category ac
                , position ac
                , Just $ Mode.altValue alt
                , groundSpeed ac
                , track ac
                , verticalRate ac
                )
                
            -- No updates for other message types
            _ -> ( callsign ac
                , category ac
                , position ac
                , altitudeFt ac
                , groundSpeed ac
                , track ac
                , verticalRate ac
                )

-- | Remove aircraft not seen for more than 60 seconds
pruneOldAircraft :: AircraftState -> IO AircraftState
pruneOldAircraft state = do
    now <- getPOSIXTime
    -- Only prune every 10 seconds
    if now - lastPrune state < 10
        then return state
        else do
            let pruned = Map.filter (\ac -> now - lastSeen ac <= 60) (aircraft state)
            return $ state { aircraft = pruned, lastPrune = now }
