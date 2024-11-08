{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import qualified Data.ByteString as BS
import Data.Word (Word8)
import Numeric (showHex, readHex)
import Control.Monad (foldM)
import Data.Bits
import Data.Char (digitToInt)
import ModeS

-- | Constants matching the test case
dataLen :: Int
dataLen = 16 * 16384  -- 256k

preambleUs :: Int
preambleUs = 8

longMsgBits :: Int
longMsgBits = 112

shortMsgBits :: Int
shortMsgBits = 56

fullLen :: Int
fullLen = preambleUs + longMsgBits

-- | Convert bytes to hex string
bytesToHex :: [Word8] -> String
bytesToHex = concatMap (\b -> let s = showHex b "" in if length s == 1 then '0':s else s)

-- | Convert hex string to bytes
hexToBytes :: String -> [Word8]
hexToBytes [] = []
hexToBytes [_] = [] -- Ignore odd length
hexToBytes (h1:h2:rest) = 
    let byte = fromIntegral $ digitToInt h1 * 16 + digitToInt h2
    in byte : hexToBytes rest

-- | Process a test message in hex format
processTestMessage :: String -> IO ()
processTestMessage hexMsg = do
    let bytes = hexToBytes $ filter (/= ' ') hexMsg
        msgBits = concatMap (\b -> [testBit b i | i <- reverse [0..7]]) bytes
        msgLen = if length bytes > 7 
                then LongMessage
                else ShortMessage
        msg = Message msgLen msgBits
        cache = newIcaoCache icaoCacheLen icaoCacheTtl
    
    verifiedMsg <- verify msg cache
    case verifiedMsg of
        Just (vm, _) -> 
            if verifiedParity vm == Valid
            then putStrLn $ "Test message: " ++ formatVerifiedMessage vm
            else putStrLn "Invalid checksum"
        Nothing -> putStrLn "Failed to verify message"

-- | Format a decoded message
formatDecodedMessage :: DecodedMessage -> String
formatDecodedMessage DecodedMessage{..} =
    let CommonFields{..} = msgCommon
        
        basicInfo = ["DF=" ++ show msgFormat, 
                    "ICAO=" ++ showHex icaoAddress ""]
            
        specificInfo = maybe [] formatSpecific msgSpecific
        
    in "{" ++ unwords (basicInfo ++ specificInfo) ++ "}"
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

-- | Format a valid verified message with decoded info
formatVerifiedMessage :: VerifiedMessage -> String
formatVerifiedMessage vm =
    let dfType = show (verifiedDF vm)
        icaoHex = showHex (verifiedICAO vm) ""
        payload = bytesToHex (verifiedPayload vm)
        decoded = formatDecodedMessage (decode vm)
    in payload ++ " [" ++ dfType ++ " ICAO:" ++ icaoHex ++ "] " ++ decoded

-- | Split ByteString into chunks of specified size
chunksOf :: Int -> BS.ByteString -> [BS.ByteString]
chunksOf size bs
    | BS.length bs < size = []
    | otherwise = 
        let (chunk, rest) = BS.splitAt size bs
        in if BS.length chunk == size
           then chunk : chunksOf size rest
           else []

-- | Process a single message and convert to output string if valid
messageToString :: Message -> IcaoCache -> IO (Maybe String, IcaoCache)
messageToString msg cache = do
    verifiedMsg <- verify msg cache
    case verifiedMsg of
        Just (dm, newCache) -> 
            if verifiedParity dm == Valid
            then return (Just (formatVerifiedMessage dm), newCache)
            else return (Nothing, newCache)
        Nothing -> return (Nothing, cache)

-- | Process a chunk of ByteString data
processChunk :: BS.ByteString -> IcaoCache -> IO ([String], IcaoCache)
processChunk chunk cache = do
    let messages = process chunk
    foldM processMessage ([], cache) messages
  where
    processMessage (outputs, currentCache) msg = do
        (mOutput, newCache) <- messageToString msg currentCache
        case mOutput of
            Just output -> return (outputs ++ [output], newCache)
            Nothing -> return (outputs, newCache)

main :: IO ()
main = do
    -- Process test messages
    let testMessages = [ "8D4840D6202CC371C32CE0576098"
                       , "8C4841753A9A153237AEF0F275BE"
                       , "8D485020994409940838175B284F"  -- message a, sub-type 1
                       , "8DA05F219B06B6AF189400CBC33F"] -- message b, sub-type 2
    putStrLn "Processing test messages:"
    mapM_ processTestMessage testMessages
    putStrLn ""
    
    -- Process binary file
    putStrLn "Processing binary file:"
    -- Create initial ICAO cache
    let initialCache = newIcaoCache icaoCacheLen icaoCacheTtl
    
    -- Read the binary file
    contents <- BS.readFile "fixture.bin"
    
    -- Process the file in chunks
    let chunks = chunksOf dataLen contents
    
    -- Process all chunks while maintaining cache state
    (validMessages, _) <- foldM processChunks ([], initialCache) chunks
    
    -- Print all valid messages
    mapM_ putStrLn validMessages
  where
    processChunks (outputs, cache) chunk = do
        (newOutputs, newCache) <- processChunk chunk cache
        return (outputs ++ newOutputs, newCache)
