-- vim: fdm=marker
{-# LANGUAGE ForeignFunctionInterface #-}
module System.Hardware.EyeOne (
  -- * Useful constants
  spectrumSize
, minWaveLength
, maxWaveLength
  -- * Option classes
, ToOption
, ToValue
  -- * EyeOne Error types
, EyeOneErrorType(..)
  -- * EyeOne options
, EyeOneOption(..)
, EyeOneMeasurementMode(..)
, EyeOneIlluminationKey(..)
, EyeOneObserverKey(..)
, EyeOneWhiteBaseKey(..)
, EyeOneDensityStandardKey(..)
, EyeOneDensityFilterModeKey(..)
, EyeOneColorSpaceKey(..)
  -- * Interface
, connected
, keyPressed
, triggerMeasurement
, calibrate
, setOption
, getOption
, lastError
, extendedErrorInformation
  -- * Setters
, setMeasurementMode
) where

import Foreign.C
import Foreign.C.String
import Foreign.Ptr
import Foreign.Marshal.Array
import Control.Monad

-- | Number of samples that will appear in a spectral measurement.
spectrumSize :: Int
spectrumSize = 36

-- | Minimum wavelength (first sample) that will be returned.
minWaveLength :: Int
minWaveLength = 380
-- | Maximum wavelength (last sample) that will be returned.
maxWaveLength :: Int
maxWaveLength = 730

-- | Class to turn a passed item into an EyeOne option string
-- which can be used in functions like 'setOption' and 'getOption' (as the first parameter).
class ToOption a where
    toOption :: a -> String

-- | Class to turn a passed item into an EyeOne value string
-- which can be used in 'setOption' (as the second parameter).
class ToValue a where
    toValue :: a -> String

-- | Error types that the device can return.
data EyeOneErrorType =
      NoError
    | DeviceNotReady
    | DeviceNotConnected
    | DeviceNotCalibrated
    | KeyNotPressed
    | NoSubstrateWhite
    | WrongMeasureMode
    | StripRecognitionFailed
    | NoDataAvailable
    | Exception
    | InvalidArgument
    | UnknownError
    | WrongDeviceType
    | UnlistedError Int
    deriving (Show, Eq)

-- EyeOne options {{{

-- | General options for the device.
data EyeOneOption =
      Version
    | SerialNumber
    | Connection
    | IsKeyPressed
    | Recognition
    | LastCalibrationTime
    | LastCalibrationCounter
    | LastError
    | ExtendedErrorInformation
    | AvailableSamples
    | AvailableMeasurementModes
    | Beep
    | LastAutoDensityFilter
    | AdaptiveMode
    | MeasurementMode
    | IlluminationKey
    | ObserverKey
    | WhiteBaseKey
    | DensityStandardKey
    | DensityFilterModeKey
    | ColorSpaceKey
    deriving (Show, Enum, Eq)

instance ToOption EyeOneOption where
    toOption IlluminationKey      = "Colorimetric.Illumination"
    toOption ObserverKey          = "Colorimetric.Observer"
    toOption WhiteBaseKey         = "Colorimetric.WhiteBase"
    toOption DensityStandardKey   = "Colorimetric.DensityStandard"
    toOption DensityFilterModeKey = "Colorimetric.DensityFilterMode"
    toOption ColorSpaceKey        = "ColorSpaceDescription.Type"
    -- Can use 'show' as long as the data type has the same name as the resulting string.
    toOption opt = show opt

-- | Possible measurement modes of the device.
data EyeOneMeasurementMode =
      SingleEmission
    | SingleReflectance
    | SingleAmbientLight
    | ScanningReflectance
    | ScanningAmbientLight
    deriving (Show, Enum, Eq)

instance ToValue EyeOneMeasurementMode where
    -- Can use 'show' as long as the data type has the same name as the resulting string.
    toValue = show

-- | Possible illumination modes of the device.
data EyeOneIlluminationKey =
      A
    | B
    | C
    | D50
    | D55
    | D65
    | D75
    | F2
    | F7
    | F11
    | Emission
    deriving (Show, Enum, Eq)

instance ToValue EyeOneIlluminationKey where
    -- Can use 'show' as long as the data type has the same name as the resulting string.
    toValue = show

data EyeOneObserverKey =
      TwoDegree
    | TenDegree
    deriving (Show, Enum, Eq)

instance ToValue EyeOneObserverKey where
    -- Can use 'show' as long as the data type has the same name as the resulting string.
    toValue = show

data EyeOneWhiteBaseKey =
      Absolute
    | Paper
    | Automatic
    deriving (Show, Enum, Eq)

instance ToValue EyeOneWhiteBaseKey where
    -- Can use 'show' as long as the data type has the same name as the resulting string.
    toValue = show
      
data EyeOneDensityStandardKey =
      DIN
    | DINNB
    | ANSIA
    | ANSIE
    | ANSII
    | ANSIT
    | SPI
    deriving (Show, Enum, Eq)

instance ToValue EyeOneDensityStandardKey where
    -- Can use 'show' as long as the data type has the same name as the resulting string.
    toValue = show

data EyeOneDensityFilterModeKey =
      Black
    | Cyan
    | Magenta
    | Yellow
    | Max
    | Auto
    deriving (Show, Enum, Eq)

instance ToValue EyeOneDensityFilterModeKey where
    -- Can use 'show' as long as the data type has the same name as the resulting string.
    toValue = show

data EyeOneColorSpaceKey =
      CIELab
    | CIELCh
    | CIELuv
    | CIELChuv
    | CIEuvY1960
    | CIEuvY1976
    | CIEXYZ
    | CIExyY
    | HunterLab
    | RxRyRz
    | LABmg
    | LCHmg
    | RGB
    deriving (Show, Enum, Eq)

instance ToValue EyeOneColorSpaceKey where
    -- Can use 'show' as long as the data type has the same name as the resulting string.
    toValue = show

-- END options }}}

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

-- * Imports

foreign import ccall "EyeOne.h I1_IsConnected"          c_I1_IsConnected :: IO CInt
foreign import ccall "EyeOne.h I1_KeyPressed"           c_I1_KeyPressed :: IO CInt
foreign import ccall "EyeOne.h I1_TriggerMeasurement"   c_I1_TriggerMeasurement :: IO CInt
foreign import ccall "EyeOne.h I1_Calibrate"            c_I1_Calibrate :: IO CInt
foreign import ccall "EyeOne.h I1_SetOption"            c_I1_SetOption :: CString -> CString -> IO CInt
foreign import ccall "EyeOne.h I1_GetOption"            c_I1_GetOption :: CString -> IO CString
foreign import ccall "EyeOne.h I1_GetNumberOfAvailableSamples" c_I1_GetNumberOfAvailableSamples :: IO CLong
foreign import ccall "EyeOne.h I1_GetSpectrum"          c_I1_GetSpectrum :: Ptr CFloat -> CLong -> IO CInt


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

getSpectrum sampleIndex = do
    itemsPtr <- makeArray
    returnCode <- c_I1_GetSpectrum itemsPtr sampleIndex
    samples <- peekArray spectrumSize itemsPtr
    return samples
    where makeArray :: IO (Ptr CFloat)
          makeArray = mallocArray spectrumSize
    -- c_I1_GetSpectrum

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


