{-# LANGUAGE ForeignFunctionInterface #-}

module XGBoostFFI where

import Control.Monad (liftM)
import Foreign
import Foreign.C

#include <xgboost/c_api.h>

peekFloatConv :: (Storable a, RealFloat a, RealFloat b) 
              => Ptr a -> IO b
peekFloatConv  = liftM realToFrac . peek

data DMatrix
{#pointer *DMatrixHandle as DMatrixPtr foreign -> DMatrix #}
{#fun XGBGetLastError as getLastError {}  -> `String' #}
--foreign import ccall unsafe "xgboost_wrapper.h XGDMatrixCreateFromMat"
--  xgboostMatrixCreateFromMat :: FloatArray -> CULong -> CULong -> CFloat -> (Ptr DMatrixHandle) -> IO CInt

{#fun XGDMatrixCreateFromMat as dmatrixCreateFromMat
   { castPtr `Float'
   , `Int'
   , `Int'
   , `Float'
   , alloca- `DMatrixPtr' peek*}
   -> `Int' #}
-- {#fun XGDMatrixCreateFromFile as createFromFile {} -> `Int' #}
-- getLastError :: IO String
-- getLastError = {#call unsafe XGBGetLastError #}
-- dmatrixCreateFromFile = {#call XGDMatrixCreateFromFile #}
-- dmatrixCreateFromDataIter = {#call XGDMatrixCreateFromDataIter #}
-- dmatrixCreateFromMat = {#call XGDMatrixCreateFromMat #}
-- dmatrixFree = {#call XGDMatrixFree #}
-- dmatrixSaveBinary = {#call XGDMatrixSaveBinary #}
