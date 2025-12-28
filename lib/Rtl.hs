{-# LANGUAGE RecordWildCards #-}

module Rtl
  ( Device
  , withDevice
  , DeviceConfig (..)
  , totalBuffer
  , runConfig
  , IQ (unIQ)
  ) where

import Control.Exception (bracket)
import Control.Monad (forever, when)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.Complex (Complex)
import Data.Maybe (fromMaybe)
import Data.Vector.Storable qualified as Storable
import Data.Word (Word32, Word8)
import Foreign.ForeignPtr (castForeignPtr)
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

newtype IQ = IQ {unIQ :: Storable.Vector (Complex Word8)}

runConfig :: DeviceConfig -> (IQ -> IO ()) -> IO a
runConfig DeviceConfig{..} f = withDevice deviceIdx $ \(Device dev) -> do
  _ <- RTLSDR.setSampleRate dev sampleRate
  _ <- RTLSDR.setCenterFreq dev centerFreq
  _ <- RTLSDR.setTunerGainMode dev False

  _ <- RTLSDR.readAsync dev (fromMaybe 0 bufferNum) bufferSize
    $ \ptr len ->
      f =<< IQ . toIQ <$> BS.packCStringLen (castPtr ptr, len)

  forever $ pure ()

toIQ :: BS.ByteString -> Storable.Vector (Complex Word8)
toIQ bs =
  let (fp, _, len) = BSI.toForeignPtr bs
  in Storable.unsafeFromForeignPtr0 (castForeignPtr fp) (len `div` 2)
