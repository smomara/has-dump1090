{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar)
import Control.Monad (foldM)
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Numeric (showHex)
import Text.Printf (printf)

import Aircraft
import ModeS
import Rtl

-- for this config, we have 262,144 / 2 = 131,072 samples per buffer
-- "   "    "       "  "    131,072 / 2,000,000 = 65.54 ms per buffer
-- With 15 buffers our total buffer is 15 * 65.54ms = 983ms, nearly 1 second
config :: DeviceConfig
config =
  DeviceConfig
    { sampleRate = 2_000_000 -- 2 MHz
    , centerFreq = 1_090_000_000 -- 1090 MHz
    , bufferSize = 262_144
    , bufferNum = Just 15
    , deviceIdx = 0
    }

-- | Process samples from the RTL-SDR
processRTLSDRSamples
  :: BS.ByteString -> MVar (IcaoCache, AircraftState) -> IO ()
processRTLSDRSamples samples stateMVar =
  modifyMVar_ stateMVar $ \(cache, aircraftState) -> do
    -- Process the samples through Mode S decoder
    (messages, newCache) <- processModeSData samples cache

    -- Update aircraft state for each message
    updatedAircraftState <- foldM updateAircraft aircraftState messages

    -- Periodically prune old aircraft
    finalState <- pruneOldAircraft updatedAircraftState

    -- Print current aircraft status
    printAircraftStatus finalState

    return (newCache, finalState)

-- | Print current status of all tracked aircraft
printAircraftStatus :: AircraftState -> IO ()
printAircraftStatus AircraftState{aircraft} = do
  putStrLn "\ESC[2J\ESC[H" -- Clear screen and move to top
  putStrLn $ "Tracking " ++ show (Map.size aircraft) ++ " aircraft:"
  putStrLn $ replicate 80 '-'
  putStrLn
    $ printf
      "%-8s | %-8s | %-20s | %-10s | %-10s | %-10s | %-15s"
      "HEX"
      "CAllSIGN"
      "POSITION"
      "ALTITUDE"
      "SPEED"
      "TRACK"
      "VERT RATE"
  putStrLn $ replicate 80 '-'
  mapM_ printAircraft $ Map.elems aircraft
  putStrLn $ replicate 80 '-'

-- | Print information about a single aircraft
printAircraft :: Aircraft -> IO ()
printAircraft Aircraft{..} = do
  let hex = showHex icaoAddress ""
      ident = maybe "unknown" id callsign
      pos = maybe "unknown" formatPos position
      alt = maybe "unknown" (\a -> show a ++ " ft") altitudeFt
      spd = maybe "unknown" (\s -> show s ++ " kt") groundSpeed
      hdg = maybe "unknown" (\t -> show t ++ "°") track
      vr = maybe "unknown" (\v -> show v ++ " ft/min") verticalRate

  putStrLn
    $ printf
      "%-8s | %-8s | %-20s | %-10s | %-10s | %-10s | %-15s"
      hex
      ident
      pos
      alt
      spd
      hdg
      vr
 where
  formatPos pos = printf "%.4f, %.4f" (latitude pos) (longitude pos)

main :: IO ()
main = do
  putStrLn "Starting Mode S aircraft tracker..."
  -- Create initial states
  let initialCache = newIcaoCache icaoCacheLen icaoCacheTtl
  initialAircraft <- newAircraftState

  -- Create MVar to hold state
  stateMVar <- newMVar (initialCache, initialAircraft)

  -- Start async reading with callback
  runConfig config $ \samples -> processRTLSDRSamples samples stateMVar
