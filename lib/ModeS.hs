module ModeS
  ( module ModeS.Demodulator
  , module ModeS.Verifier
  , module ModeS.Decoder
  , module ModeS.Types
  , processModeSData
  ) where

import Control.Monad (foldM)
import qualified Data.ByteString as BS

import ModeS.Decoder
import ModeS.Demodulator
import ModeS.Types
import ModeS.Verifier

{- | Process Mode S data, handling demodulation, verification, and decoding
Returns a list of successfully decoded messages and the updated ICAO cache
-}
processModeSData
  :: BS.ByteString -> IcaoCache -> IO ([DecodedMessage], IcaoCache)
processModeSData input cache = do
  -- Demod raw data into messages
  let messages = process input

  -- Process each message through verification and decoding
  foldM processMessage ([], cache) messages
 where
  processMessage (decodedMsgs, currentCache) msg = do
    -- Verify the message and update cache
    verificationResult <- verify msg currentCache
    case verificationResult of
      Just (verifiedMsg, newCache) ->
        -- Only include messages with valid parity
        if verifiedParity verifiedMsg == Valid
          then return (decodedMsgs ++ [decode verifiedMsg], newCache)
          else return (decodedMsgs, newCache)
      Nothing ->
        return (decodedMsgs, currentCache)
