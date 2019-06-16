{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

module XGBoostFFI where

import Control.Monad (liftM)
import Foreign
import Foreign.C

#include <xgboost/c_api.h>

peekPtr :: Ptr a -> IO (Ptr b)
peekPtr = peek . castPtr

peekIntConv :: (Storable a, Integral a, Integral b) => Ptr a -> IO b
peekIntConv = liftM fromIntegral . peek
peekFloatConv :: (Storable a, RealFloat a, RealFloat b) => Ptr a -> IO b
peekFloatConv  = liftM realToFrac . peek

type FloatPtr = Ptr Float
type DMatrix = Ptr Word8
{#pointer *DMatrixHandle as DMatrixPtr foreign -> DMatrix #}

{#fun XGBGetLastError as getLastError {}  -> `String' #}

{#fun XGDMatrixCreateFromMat as cDMatrixCreateFromMat
   { castPtr `Ptr Float'
   , `Int'
   , `Int'
   , `Float'
   , alloca- `Ptr Word8' peekPtr* }
   -> `Int' #}

dmCreateFromMat :: [Float] -> Int -> Int -> Float -> IO (Int, DMatrix)
dmCreateFromMat xs nrow ncol missing = do
 withArray xs $ \xsPtr -> cDMatrixCreateFromMat xsPtr nrow ncol missing

{#fun XGDMatrixSetFloatInfo as cDMatrixSetFloatInfo
   { castPtr `Ptr Word8'
   , `String'
   , castPtr `Ptr Float'
   , `Int'}
   -> `Int' #}

dmSetFloatInfo :: DMatrix -> String -> [Float] -> IO Int
dmSetFloatInfo dm field xs =
  withArray xs $ \xsPtr -> cDMatrixSetFloatInfo dm field xsPtr (length xs)

{#fun XGDMatrixGetFloatInfo as cDMatrixGetFloatInfo
   { castPtr `DMatrix'
   , `String'
   , alloca- `Int' peekIntConv*
   , alloca- `Ptr Float' peekPtr* }
   -> `Int' #}

dmGetFloatInfo :: DMatrix -> String -> IO (Int, [Float])
dmGetFloatInfo dm field = do
  (ret, outLen, outDPtr) <- cDMatrixGetFloatInfo dm field
  (ret,) <$> peekArray outLen outDPtr

repl :: IO ()
repl = do
  (_, dm) <- dmCreateFromMat [1.0, 2.0, 3.0, 4.0] 2 2 2.0
  dmSetFloatInfo dm "label" [5.0, 6.0]
  (_, info) <- dmGetFloatInfo dm "label"
  putStrLn (show info)
 
