{-# LANGUAGE ForeignFunctionInterface #-}
module System.Hardware.EyeOne.C (
  c_I1_IsConnected
, c_I1_KeyPressed
, c_I1_TriggerMeasurement
, c_I1_Calibrate
, c_I1_SetOption
, c_I1_GetOption
, c_I1_GetNumberOfAvailableSamples
, c_I1_GetSpectrum
, c_I1_GetTriStimulus
) where

import Foreign.C
import Foreign.Ptr

foreign import ccall "EyeOne.h I1_IsConnected"          c_I1_IsConnected                        :: IO CInt
foreign import ccall "EyeOne.h I1_KeyPressed"           c_I1_KeyPressed                         :: IO CInt
foreign import ccall "EyeOne.h I1_TriggerMeasurement"   c_I1_TriggerMeasurement                 :: IO CInt
foreign import ccall "EyeOne.h I1_Calibrate"            c_I1_Calibrate                          :: IO CInt
foreign import ccall "EyeOne.h I1_SetOption"            c_I1_SetOption                          :: CString -> CString -> IO CInt
foreign import ccall "EyeOne.h I1_GetOption"            c_I1_GetOption                          :: CString -> IO CString
foreign import ccall "EyeOne.h I1_GetNumberOfAvailableSamples" c_I1_GetNumberOfAvailableSamples :: IO CLong
foreign import ccall "EyeOne.h I1_GetSpectrum"          c_I1_GetSpectrum                        :: Ptr CFloat -> CLong -> IO CInt
foreign import ccall "EyeOne.h I1_GetTriStimulus"       c_I1_GetTriStimulus                     :: Ptr CFloat -> CLong -> IO CInt

