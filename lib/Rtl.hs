{-# LANGUAGE RecordWildCards #-}

module Rtl
  ( Device
  , withDevice
  , DeviceConfig (..)
  , totalBuffer
  , runConfig
  ) where

import Control.Exception (bracket)
import Control.Monad (forever, when)
import Data.ByteString (ByteString, packCStringLen)
import Data.Maybe (fromMaybe)
import Data.Word (Word32)
import Foreign.Ptr (castPtr)
import RTLSDR (RTLSDR)
import RTLSDR qualified as RTLSDR

newtype Device = Device RTLSDR

data DeviceConfig
  = DeviceConfig
  { sampleRate :: Word32
  , centerFreq :: Word32
  , bufferSize :: Word32
  {- ^ byte size of asynchronous buffers
  must be multiple of 512
  -}
  , bufferNum :: Maybe Word32
  {- ^ number of asynchronous buffers
  Nothing means default (15)
  -}
  , deviceIdx :: Word32
  }
  deriving (Eq, Show)

-- RTL-SDR samples are Complex Word8, so samples per buffer are `bufferSize / 2`
-- and time per buffer = samples per buffer / samples per second (`sampleRate`)
-- and total buffer = time per buffer * number of buffers
totalBuffer :: Fractional a => DeviceConfig -> a
totalBuffer DeviceConfig{..} =
  let samplesPerBuffer = fromIntegral bufferSize / 2
      secondsPerBuffer = samplesPerBuffer / fromIntegral sampleRate
  in secondsPerBuffer * maybe 15 fromIntegral bufferNum

assertDeviceId :: Integral a => a -> IO ()
assertDeviceId n = do
  count <- RTLSDR.getDeviceCount
  when (count <= fromIntegral n) (fail "device id not found")

makeDevice :: Word32 -> IO Device
makeDevice n = do
  assertDeviceId n
  md <- RTLSDR.open n
  case md of
    Nothing -> fail "failed to open device"
    Just d -> Device d <$ RTLSDR.resetBuffer d

closeDevice :: Device -> IO ()
closeDevice (Device d) = RTLSDR.close d

withDevice :: Word32 -> (Device -> IO a) -> IO a
withDevice n = bracket (makeDevice n) closeDevice

runConfig :: DeviceConfig -> (ByteString -> IO ()) -> IO a
runConfig DeviceConfig{..} f = withDevice deviceIdx $ \(Device dev) -> do
  _ <- RTLSDR.setSampleRate dev sampleRate
  _ <- RTLSDR.setCenterFreq dev centerFreq
  _ <- RTLSDR.setTunerGainMode dev False

  _ <- RTLSDR.readAsync dev (fromMaybe 0 bufferNum) bufferSize
    $ \ptr len -> f =<< packCStringLen (castPtr ptr, len)

  forever $ pure ()
