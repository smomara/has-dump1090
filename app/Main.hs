{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import qualified Data.Map.Strict as Map
import qualified Data.ByteString as BS
import Data.Word (Word32)
import Numeric (showHex)
import Control.Monad (forever, void, when)
import System.Exit (exitFailure)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.C.Types (CUChar)
import Control.Monad (foldM)
import Text.Printf (printf)
import Control.Concurrent.MVar
import RTLSDR
import ModeS
import Aircraft

-- | RTL-SDR configuration constants
sampleRate :: Word32
sampleRate = 2000000  -- 2 MHz sample rate for Mode S

centerFreq :: Word32
centerFreq = 1090000000  -- 1090 MHz (Mode S frequency)

bufferSize :: Int
bufferSize = 16 * 16384  -- Same as previous code

gainMode :: Bool
gainMode = False  -- Auto gain

-- | Initialize the RTL-SDR device
initRTLSDR :: IO RTLSDR
initRTLSDR = do
    -- Get device count
    count <- getDeviceCount
    when (count == 0) $ do
        putStrLn "No RTL-SDR devices found"
        exitFailure

    -- Try to open first device
    open 0 >>= \case
        Nothing -> do
            putStrLn "Failed to open RTL-SDR device"
            exitFailure
        Just dev -> do
            -- Configure device
            void $ setSampleRate dev sampleRate
            void $ setCenterFreq dev centerFreq
            void $ setTunerGainMode dev gainMode
            
            -- Print device info
            name <- getDeviceName 0
            gains <- getTunerGains dev
            putStrLn $ "Opened RTL-SDR device: " ++ name
            putStrLn $ "Available gains: " ++ show gains
            putStrLn $ "Sample rate: " ++ show sampleRate
            putStrLn $ "Center frequency: " ++ show centerFreq
            
            -- Reset buffer before starting
            void $ resetBuffer dev
            return dev

-- | Process samples from the RTL-SDR
processRTLSDRSamples :: Ptr CUChar -> Int -> MVar (IcaoCache, AircraftState) -> IO ()
processRTLSDRSamples ptr len stateMVar = do
    -- Convert samples to ByteString
    samples <- BS.packCStringLen (castPtr ptr, len)
    
    -- Update state atomically
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
    putStrLn "\ESC[2J\ESC[H"  -- Clear screen and move to top
    putStrLn $ "Tracking " ++ show (Map.size aircraft) ++ " aircraft:"
    putStrLn $ replicate 80 '-'
    putStrLn $ printf "%-8s | %-8s | %-20s | %-10s | %-10s | %-10s | %-15s"
        "HEX" "CAllSIGN" "POSITION" "ALTITUDE" "SPEED" "TRACK" "VERT RATE"
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
        
    putStrLn $ printf "%-8s | %-8s | %-20s | %-10s | %-10s | %-10s | %-15s"
        hex ident pos alt spd hdg vr
  where
    formatPos pos = printf "%.4f, %.4f" 
        (fromIntegral $ rawLatitude $ posCoordinates pos :: Double)
        (fromIntegral $ rawLongitude $ posCoordinates pos :: Double)

main :: IO ()
main = do
    putStrLn "Starting Mode S aircraft tracker..."
    
    -- Initialize RTL-SDR
    dev <- initRTLSDR
    
    -- Create initial states
    let initialCache = newIcaoCache icaoCacheLen icaoCacheTtl
    initialAircraft <- newAircraftState
    
    -- Create MVar to hold state
    stateMVar <- newMVar (initialCache, initialAircraft)
    
    -- Start async reading with callback
    void $ readAsync dev 0 (fromIntegral bufferSize) $ \ptr len -> 
        processRTLSDRSamples ptr len stateMVar
    
    -- Wait forever (until Ctrl-C)
    forever $ return ()
