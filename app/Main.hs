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
        
        altitudeInfo = maybe [] 
            (\alt -> ["Alt=" ++ show (altValue alt) ++ 
                     case altUnit alt of 
                         Feet -> "ft"
                         Meters -> "m"]) 
            altitude
            
        identityInfo = maybe [] 
            (\id -> ["Squawk=" ++ show id]) 
            identity
            
        specificInfo = maybe [] formatSpecific msgSpecific
        
    in "{" ++ unwords (basicInfo ++ altitudeInfo ++ identityInfo ++ specificInfo) ++ "}"
    where
        formatSpecific :: MessageSpecific -> [String]
        formatSpecific = \case
            DF11Fields{..} ->
                ["CA=" ++ show capability]
                
            DF17Fields{..} ->
                ["Type=" ++ show esType] ++ formatESData esData
                
            DF45Fields{..} ->
                ["Status=" ++ show flightStatus,
                 "DR=" ++ show downlinkRequest]

        formatESData :: ExtendedSquitterData -> [String]
        formatESData = \case
            ESAircraftID ident ->
                ["Flight=" ++ flightNumber ident,
                 "Category=" ++ show (aircraftCategory ident)]

            ESAirbornePos pos alt ->
               ["Lat=" ++ show (fromIntegral $ rawLatitude $ coordinates pos),
                 "Lon=" ++ show (fromIntegral $ rawLongitude $ coordinates pos),
                 "Alt=" ++ show (altValue alt) ++ 
                    case altUnit alt of
                        Feet -> "ft"
                        Meters -> "m",
                 "OddFormat=" ++ show (isOddFormat pos),
                 "UTC=" ++ show (isUTCSync pos)]

            ESAirborneVel vel ->
                ["Speed=" ++ show (groundSpeed vel) ++ "kt",
                 "Track=" ++ show (track vel) ++ "°",
                 "VRate=" ++ show (verticalRate vel) ++ "ft/min"]
                 
            ESSurfacePos cpr ->
                ["Lat=" ++ show (fromIntegral $ rawLatitude cpr),
                 "Lon=" ++ show (fromIntegral $ rawLongitude cpr)]

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
    let testMessages = ["8D4840D6202CC371C32CE0576098"]
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
