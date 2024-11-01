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

-- | Format a valid decoded message
formatDecodedMessage :: DecodedMessage -> String
formatDecodedMessage dm =
    let dfType = show (decodedDF dm)
        icaoHex = showHex (decodedICAO dm) ""
        payload = bytesToHex (decodedPayload dm)
    in payload ++ " [" ++ dfType ++ " ICAO:" ++ icaoHex ++ "]"

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
    decodedMsg <- decode msg cache
    case decodedMsg of
        Just (dm, newCache) -> 
            if decodedParity dm == Valid
            then return (Just (formatDecodedMessage dm), newCache)
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
