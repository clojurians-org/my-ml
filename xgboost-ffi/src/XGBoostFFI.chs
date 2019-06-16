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

type DMatrix = Ptr Word8
type Booster = Ptr Word8
{#pointer *DMatrixHandle as DMatrixPtr foreign -> DMatrix #}
{#pointer *BoosterHandle as BoosterPtr foreign -> Booster #}

{#fun XGBGetLastError as cGetLastError {}  -> `String' #}
getLastError :: IO String
getLastError = cGetLastError

{#fun XGDMatrixCreateFromMat as cDMatrixCreateFromMat
   { castPtr `Ptr Float'
   , `Int'
   , `Int'
   , `Float'
   , alloca- `Ptr DMatrix' peekPtr* }
   -> `Int' #}
dmCreateFromMat :: [Float] -> Int -> Int -> Float -> IO (Int, Ptr DMatrix)
dmCreateFromMat xs nrow ncol missing = do
 withArray xs $ \xsPtr -> cDMatrixCreateFromMat xsPtr nrow ncol missing


{#fun XGDMatrixCreateFromFile as cDMatrixCreateFromFile
   { `String'
   , `Int'
   , alloca- `Ptr DMatrix' peekPtr* }
   -> `Int' #}
dmCreateFromFile :: FilePath -> Int -> IO (Int, Ptr DMatrix)
dmCreateFromFile = cDMatrixCreateFromFile


{#fun XGDMatrixFree as cDMatrixFree { castPtr `Ptr DMatrix' }  -> `Int' #}
dmFree :: Ptr DMatrix -> IO Int
dmFree = cDMatrixFree

{#fun XGDMatrixSetFloatInfo as cDMatrixSetFloatInfo
   { castPtr `Ptr DMatrix'
   , `String'
   , castPtr `Ptr Float'
   , `Int'}
   -> `Int' #}
dmSetFloatInfo :: Ptr DMatrix -> String -> [Float] -> IO Int
dmSetFloatInfo dm field xs =
  withArray xs $ \xsPtr -> cDMatrixSetFloatInfo dm field xsPtr (length xs)

{#fun XGDMatrixGetFloatInfo as cDMatrixGetFloatInfo
   { castPtr `Ptr DMatrix'
   , `String'
   , alloca- `Int' peekIntConv*
   , alloca- `Ptr Float' peekPtr* }
   -> `Int' #}
dmGetFloatInfo :: Ptr DMatrix -> String -> IO (Int, [Float])
dmGetFloatInfo dm field = do
  (ret, outLen, outDPtr) <- cDMatrixGetFloatInfo dm field
  (ret,) <$> peekArray outLen outDPtr

{#fun XGBoosterCreate as cBoosterCreate
   { castPtr `Ptr DMatrix'
   , `Int'
   , alloca- `Ptr Booster' peekPtr* }
   -> `Int' #}
boosterCreate :: Ptr DMatrix -> Int -> IO (Int, Ptr Booster)
boosterCreate = cBoosterCreate

repl :: IO ()
repl = do
  (_, dm) <- dmCreateFromMat [1.0, 2.0, 3.0, 4.0] 2 2 2.0
  dmSetFloatInfo dm "label" [5.0, 6.0]
  (_, info) <- dmGetFloatInfo dm "label"
  putStrLn (show info)
  dmFree dm

  (_, dTrain) <- dmCreateFromFile "data/agaricus.txt.train" 0
  (_, dTest) <- dmCreateFromFile "data/agaricus.txt.test" 0

  (_, cBoosterCreate) <- boosterCreate dTrain 1
  dmFree dTrain
  dmFree dTest
  putStrLn "finished"
