module System.Hardware.EyeOne.Types (
  -- * Option classes
  ToOption(..)
, ToValue(..)
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
) where

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
