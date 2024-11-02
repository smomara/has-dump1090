{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Data.ByteString as BS
import Data.Word (Word8)
import Numeric (showHex)
import Control.Monad (foldM)
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

-- | Format a decoded message
formatDecodedMessage :: DecodedMessage -> String
formatDecodedMessage dm = 
    let basicInfo = [ "DF=" ++ show (decodedDF dm)
                   , "ICAO=" ++ showHex (decodedICAO dm) ""
                   , "CA=" ++ show (decodedCapability dm)
                   ]
        
        flightStatus = maybe [] (\fs -> ["Status=" ++ show fs]) (decodedFlightStatus dm)
        identity = maybe [] (\id -> ["Squawk=" ++ show id]) (decodedIdentity dm)
        altitudeInfo = maybe [] (\alt -> ["Alt=" ++ show alt ++ "ft"]) (decodedAltitude dm)
        ground = maybe [] (\g -> ["Ground=" ++ show g]) (decodedGroundBit dm)
        
        esInfo = case decodedExtSquitter dm of
            Just esType -> case esType of
                ESAircraftIdentification{..} -> 
                    ["Flight=" ++ flightNumber, "Type=" ++ show aircraftType]
                    
                ESSurfacePosition{..} -> 
                    [ "Lat=" ++ show (latitude position)
                    , "Lon=" ++ show (longitude position)
                    , "Speed=" ++ show groundSpeed ++ "kt"
                    , "Track=" ++ show groundTrack ++ "°"
                    ]
                    
                ESAirbornePosition{..} -> 
                    [ "Lat=" ++ show (latitude position)
                    , "Lon=" ++ show (longitude position)
                    , "PosAlt=" ++ show (altitude position) ++ "ft"
                    ] ++ maybe [] (\vr -> ["VRate=" ++ show vr ++ "ft/min"]) posVerticalRate
                    
                ESAirborneVelocity{..} ->
                    [ "Type=" ++ show velocityType
                    , "Speed=" ++ show speed ++ "kt"
                    , "VRate=" ++ show velVerticalRate ++ "ft/min"
                    , "Heading=" ++ show heading ++ "°"
                    , "SpeedType=" ++ (if speedType then "Ground" else "Air")
                    ]
                    
                ESOperationalStatus{..} ->
                    ["Status=" ++ show aircraftStatus, "Capabilities=" ++ show capabilities]
                    
                ESUnknownType{..} ->
                    ["ME=" ++ show meType, "Subtype=" ++ show meSubtype]
                    
            Nothing -> []
            
    in "{" ++ unwords (basicInfo ++ flightStatus ++ identity ++ altitudeInfo ++ ground ++ esInfo) ++ "}"

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
