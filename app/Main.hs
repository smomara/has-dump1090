{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import qualified Data.ByteString as BS
import Data.Word (Word8, Word32)
import Numeric (showHex)
import Control.Monad (forever, void, when)
import System.Exit (exitFailure)
import Foreign.Marshal.Array (peekArray)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.C.Types (CUChar)
import RTLSDR
import ModeS

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
processRTLSDRSamples :: Ptr CUChar -> Int -> IcaoCache -> IO IcaoCache
processRTLSDRSamples ptr len cache = do
    -- Convert samples to ByteString
    samples <- BS.packCStringLen (castPtr ptr, len)
    
    -- Process the samples through Mode S decoder
    (messages, newCache) <- processModeSData samples cache
    
    -- Print decoded messages
    mapM_ (putStrLn . formatDecodedMessage) messages
    
    return newCache

-- | Format a decoded message
formatDecodedMessage :: DecodedMessage -> String
formatDecodedMessage DecodedMessage{..} =
    let CommonFields{..} = msgCommon
        basicInfo = ["DF=" ++ show msgFormat, 
                    "ICAO=" ++ showHex icaoAddress ""]
        specificInfo = maybe [] formatSpecific msgSpecific
    in "[RTL-SDR] {" ++ unwords (basicInfo ++ specificInfo) ++ "}"
    where
        formatSpecific :: MessageSpecific -> [String]
        formatSpecific = \case
            DF11Fields{..} ->
                ["CA=" ++ show capability]
                
            DF17Fields{..} ->
                ["Type=" ++ show esType] ++ formatESData esData
                
            DF420Fields{..} ->
                ["Status=" ++ show flightStatus,
                 "DR=" ++ show downlinkRequest,
                 "UM=" ++ show utilityMsg,
                 "Alt=" ++ show (altValue altitude) ++ 
                    case altUnit altitude of
                        Feet -> "ft"
                        Meters -> "m"]
                        
            DF521Fields{..} ->
                ["Status=" ++ show flightStatus,
                 "DR=" ++ show downlinkRequest,
                 "UM=" ++ show utilityMsg,
                 "Squawk=" ++ show identity]

        formatESData :: ExtendedSquitterData -> [String]
        formatESData = \case
            ESAircraftID ident ->
                ["Flight=" ++ flightNumber ident,
                 "Category=" ++ show (aircraftCategory ident)]
                 
            ESAirbornePos pos alt ->
                formatPosition pos ++
                ["Alt=" ++ show (altValue alt) ++ 
                    case altUnit alt of
                        Feet -> "ft"
                        Meters -> "m"]
                        
            ESSurfacePos SurfacePosition{..} ->
                formatPosition surfacePosition ++
                formatSurfaceMovement surfaceMovement
                
            ESAirborneVel vel -> case vel of
                GroundVelocity {..} ->
                    ["Speed=" ++ show velSpeed ++ "kt",
                     "Track=" ++ show velTrack ++ "°",
                     "VRate=" ++ show velVRate ++ "ft/min"]
                AirVelocity {..} ->
                    ["Track=" ++ if velValid
                                then show velHeading ++ "°"
                                else "invalid"]

        formatPosition :: Position -> [String]
        formatPosition pos =
            ["Lat=" ++ show (fromIntegral $ rawLatitude $ posCoordinates pos),
             "Lon=" ++ show (fromIntegral $ rawLongitude $ posCoordinates pos),
             "OddFormat=" ++ show (posOddFormat pos),
             "UTC=" ++ show (posUTCSync pos)]

        formatSurfaceMovement :: SurfaceMovement -> [String]
        formatSurfaceMovement mov =
            ["Speed=" ++ show (surfaceSpeed mov) ++ "kt",
             "Track=" ++ if surfaceTrackValid mov 
                        then show (surfaceTrack mov) ++ "°"
                        else "invalid"]

main :: IO ()
main = do
    putStrLn "Starting Mode S receiver..."
    
    -- Initialize RTL-SDR
    dev <- initRTLSDR
    
    -- Create initial ICAO cache
    let initialCache = newIcaoCache icaoCacheLen icaoCacheTtl
    
    -- Start async reading with callback
    void $ readAsync dev 0 (fromIntegral bufferSize) $ \ptr len -> do
        void $ processRTLSDRSamples ptr len initialCache
    
    -- Wait forever (until Ctrl-C)
    forever $ return ()
