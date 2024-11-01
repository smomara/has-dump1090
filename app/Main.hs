module Main where
import qualified Data.ByteString as BS
import qualified Data.Vector.Unboxed as V
import Data.Word (Word8)
import Data.Bits (shiftR, (.&.))
import Numeric (showHex)
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

-- | Convert a list of bits to a hex string
bitsToHex :: [Bool] -> String
bitsToHex bits =
    let bytes = chunksOf 8 bits
        byteToInt :: [Bool] -> Int
        byteToInt byte = sum $ zipWith (\b p -> if b then 2^p else 0) 
                                      (reverse byte) 
                                      [0..7]
        toHexStr n = if n < 16 
                    then '0' : showHex n "" 
                    else showHex n ""
    in concatMap (toHexStr . byteToInt) bytes
  where
    chunksOf _ [] = []
    chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- | Convert bytes to hex string
bytesToHex :: [Word8] -> String
bytesToHex = concatMap (\b -> let s = showHex b "" in if length s == 1 then '0':s else s)

-- | Process a single message and convert to hex string
messageToHex :: Message -> String
messageToHex msg =
    let firstByte = take 8 (msgBits msg)
        -- Extract just the DF (first 5 bits) from the first byte
        df :: Word8
        df = fromIntegral $ (byteToInt firstByte `shiftR` 3) 
        msgLen = getMsgLenBits df `div` 8
        bits = take (msgLen * 8) (msgBits msg)
    in bitsToHex bits
  where
    byteToInt :: [Bool] -> Int
    byteToInt byte = sum $ zipWith (\b p -> if b then 2^p else 0) 
                                  (reverse byte) 
                                  [0..7]

-- | Calculate message length in bits based on message type
getMsgLenBits :: Word8 -> Int
getMsgLenBits msgtype = 
    if (msgtype .&. 0x10) /= 0 
    then longMsgBits  -- DF >= 16 (long message)
    else shortMsgBits -- DF <= 15 (short message)

-- | Split ByteString into chunks of specified size
chunksOf :: Int -> BS.ByteString -> [BS.ByteString]
chunksOf size bs
    | BS.length bs < size = []
    | otherwise = 
        let (chunk, rest) = BS.splitAt size bs
        in if BS.length chunk == size
           then chunk : chunksOf size rest
           else []

-- | Process a single message and convert to output string
messageToString :: Message -> String
messageToString msg =
    let originalHex = messageToHex msg
        decodedMsg = decode msg
        msgDetails = case decodedMsg of
            Just dm -> 
                let dfType = show (decodedDF dm)
                    icaoHex = showHex (decodedICAO dm) ""
                    crcStatus = case decodedParity dm of
                        Valid -> "CRC OK"
                        InvalidChecksum -> "CRC FAIL"
                        CorrectedError -> "CRC CORRECTED"
                    finalHex = bytesToHex (decodedPayload dm)
                    details = if originalHex /= finalHex
                             then " (Corrected: " ++ finalHex ++ ")"
                             else ""
                in originalHex ++ " [" ++ dfType ++ " ICAO:" ++ icaoHex ++ 
                   "] " ++ crcStatus ++ details
            Nothing -> originalHex ++ " INVALID DF"
    in msgDetails

-- | Process a chunk of ByteString data
processChunk :: BS.ByteString -> [String]
processChunk chunk =
    let messages = process chunk
    in map messageToString messages

main :: IO ()
main = do
    -- Read the binary file
    contents <- BS.readFile "fixture.bin"
    
    -- Process the file in chunks
    let chunks = chunksOf dataLen contents
        messages = concatMap processChunk chunks
    
    -- Print all messages
    mapM_ putStrLn messages
