-- vim: fdm=marker
module System.Hardware.EyeOne (
  -- * Useful constants
  spectrumSize
, minWaveLength
, maxWaveLength
  -- * Types
, module System.Hardware.EyeOne.Types
  -- * Interface
, connected
, keyPressed
, triggerMeasurement
, getNumberOfAvailableSamples
, getSpectrum
, calibrate
, setOption
, getOption
, lastError
, extendedErrorInformation
  -- * Setters
, setMeasurementMode
, setIlluminationKey
, setColorSpaceKey
, setObserverKey
, setWhiteBaseKey
) where

import Foreign.C
import Foreign.C.String
import Foreign.Ptr
import Foreign.Marshal.Array (peekArray, mallocArray)
import Control.Monad

import System.Hardware.EyeOne.Types
import System.Hardware.EyeOne.C

-- | Number of samples that will appear in a spectral measurement.
spectrumSize :: Int
spectrumSize = 36

-- | Minimum wavelength (first sample) that will be returned.
minWaveLength :: Int
minWaveLength = 380
-- | Maximum wavelength (last sample) that will be returned.
maxWaveLength :: Int
maxWaveLength = 730


-- * Conversion helpers

-- | Transform a error code as returned from the c functions into an 'EyeOneErrorType'.
fromError :: CInt -> EyeOneErrorType
fromError e = case e of
    0  -> NoError
    1  -> DeviceNotReady
    2  -> DeviceNotConnected
    3  -> DeviceNotCalibrated
    4  -> KeyNotPressed
    5  -> NoSubstrateWhite
    6  -> WrongMeasureMode
    7  -> StripRecognitionFailed
    8  -> NoDataAvailable
    9  -> Exception
    10 -> InvalidArgument
    11 -> UnknownError
    12 -> WrongDeviceType
    _  -> UnlistedError (fromIntegral e)

-- | Monadic version of 'fromError'.
fromErrorM :: IO CInt -> IO EyeOneErrorType
fromErrorM = liftM fromError

connected :: IO Bool
connected = do
    connStatus <- fromErrorM c_I1_IsConnected
    return $ connStatus == NoError

keyPressed :: IO Bool
keyPressed = do
    keyStatus <- fromErrorM c_I1_KeyPressed
    return $ keyStatus /= KeyNotPressed

triggerMeasurement :: IO EyeOneErrorType
triggerMeasurement = fromErrorM c_I1_TriggerMeasurement

getNumberOfAvailableSamples :: IO Integer
getNumberOfAvailableSamples = liftM fromIntegral c_I1_GetNumberOfAvailableSamples

getSpectrum :: Int -> IO (Either EyeOneErrorType [Float])
getSpectrum sampleIndex = do
    itemsPtr <- makeArray spectrumSize
    returnCode <- fromErrorM $ c_I1_GetSpectrum itemsPtr (fromIntegral sampleIndex)
    case returnCode of
        NoError -> return . Right . map realToFrac =<< peekArray spectrumSize itemsPtr
        code    -> return . Left $ code

-- | Create a new array with specified size as a (Ptr CFloat).
makeArray :: Int -> IO (Ptr CFloat)
makeArray = mallocArray

calibrate :: IO EyeOneErrorType
calibrate = fromErrorM c_I1_Calibrate

setOption :: (ToOption a, ToValue b) => a -> b -> IO EyeOneErrorType
setOption opt val =
    withCString (toOption opt) $ \optStr ->
        withCString (toValue val) $ \valStr -> do
            fromErrorM (c_I1_SetOption optStr valStr)

getOption :: (ToOption a) => a -> IO String
getOption opt = do
    withCString (toOption opt) $ \optStr -> do
        res <- c_I1_GetOption optStr
        peekCString res

lastError :: IO String
lastError = getOption LastError

extendedErrorInformation :: IO String
extendedErrorInformation = getOption ExtendedErrorInformation

setMeasurementMode :: EyeOneMeasurementMode -> IO EyeOneErrorType
setMeasurementMode = setOption MeasurementMode

setIlluminationKey :: EyeOneIlluminationKey -> IO EyeOneErrorType
setIlluminationKey = setOption IlluminationKey

setColorSpaceKey :: EyeOneColorSpaceKey -> IO EyeOneErrorType
setColorSpaceKey = setOption ColorSpaceKey

setObserverKey :: EyeOneObserverKey -> IO EyeOneErrorType
setObserverKey = setOption ObserverKey

setWhiteBaseKey :: EyeOneWhiteBaseKey -> IO EyeOneErrorType
setWhiteBaseKey = setOption WhiteBaseKey


