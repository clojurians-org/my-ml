{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Prelude hiding ()
import Data.Function ((&))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Error.Class (MonadError)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Text (Text)
import Data.Time (UTCTime(..), Day, parseTimeM, defaultTimeLocale, toGregorian)
import Data.Csv (FromNamedRecord, defaultDecodeOptions)
import qualified Data.Csv as CSV
import qualified Data.HashMap.Lazy as M

import Labels ((:=)(..), get, cons, project)
import Labels.CSV () -- instance

import Data.Conduit (
    ConduitT, ZipSink(..), toConsumer
  , runConduit, runConduitRes, runConduitPure
  , (.|), yield, getZipSink
  )
import Data.Conduit.Combinators (sourceFile, sourceFileBS, sinkFile)
import qualified Data.Conduit.Combinators as C

import Data.Csv.Conduit (fromNamedCsvLiftError)

import Data.Map.Lens

instance CSV.FromField Day where
  parseField = parseTimeM True defaultTimeLocale "%F" .BS8.unpack
instance CSV.FromField UTCTime where
  parseField = parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" . BS8.unpack

main :: IO ()
main = undefined

-- tableTypes "User" "data/JDATA_A/jdata_user_basic_info.csv"
-- tableTypes "Item" "data/JDATA_A/jdata_sku_basic_info.csv"
-- tableTypes "ItemTrans" "data/JDATA_A/jdata_user_action.csv"
-- tableTypes "Order" "data/JDATA_A/jdata_user_order.csv"
-- tableTypes "OrderScore" "data/JDATA_A/jdata_user_comment_score.csv"

repl :: IO ()
repl = do
  user <- runConduitRes $
                  sourceFile "data/JDATA_A/jdata_user_basic_info.csv"
               .| removeBOM .| toCSV
               .| C.map (id @( "user_id" := Int
                             , "age" := Int
                             , "sex" := Int
                             , "user_lv_cd" := Int))
               .| C.sinkList
  order <- runConduitRes $
                  sourceFile "data/JDATA_A/jdata_user_order.csv"
               .| removeBOM .| toCSV
               .| C.map (id @( "user_id" := Int
                             , "sku_id" := Int
                             , "o_id" := Int
                             , "o_date" := Day
                             , "o_area" := Int
                             , "o_sku_num" := Int))
               .| C.map (addYMD #o_date id)
               .| C.take 3
               .| C.sinkList

  let filename = "data/JDATA_A/jdata_user_comment_score.csv"
  
  orderComment <- runConduitRes $
                  sourceFile "test.csv"
               .| removeBOM .| toCSV
               .| C.map (id @( "user_id" := Int
                             , "comment_create_tm" := UTCTime
                             , "o_id" := Int
                             , "score_level" := Int))
               .| C.map (copyWith #comment_create_tm #c_date id)
               .| C.map (addYMD #c_date utctDay)
               .| C.map project
               .| C.map (id @( "user_id" := Int
                             , "c_date" := UTCTime
                             , "year" := Integer
                             , "month" := Int
                             , "day" := Int
                             , "o_id" := Int
                             , "score_level" := Int))
               .|  ((toConsumer . getZipSink) ((,)
                       <$> ZipSink (C.foldl (flip (:)) [])
                       <*> ZipSink (groupBy #score_level (+) (const 1)) ) >>= yield)
               .| C.take 3
               .| C.sinkList

  mapM_ print (take 3 orderComment)
  putStrLn "finished"
  where
    removeBOM :: Monad m => ConduitT BS.ByteString BS.ByteString m ()
    removeBOM = (C.take 1 .| C.map (BS.drop 3)) >> C.map id
    toCSV :: (FromNamedRecord a, MonadError IOError m) => ConduitT BS.ByteString a m ()
    toCSV = fromNamedCsvLiftError (userError . show) defaultDecodeOptions
    addYMD name f r = do
      let (year, month, day) = toGregorian (f (get name r))
      r & cons (#year := year) & cons (#month := month) & cons (#day := day)
    copyWith oname nname f r = cons (nname := f (get oname r)) r
    groupBy name f f0 = C.foldl (\m r -> M.insertWith f (get name r) (f0 r) m)  M.empty

-- fromList [(0,[9,6,3]),(1,[10,7,4,1]),(2,[8,5,2])]
-- (9, 9), (6, 9), (3, 9), (10, 10), (7, 10), (4, 10), (1, 10)...
repl2 = runConduit $ C.yieldMany [0..9]
--        .| C.iterM print
        .| ((toConsumer . getZipSink) ((,,)
             <$> ZipSink (C.foldl (flip (:)) [])
             <*> ZipSink C.sum
             <*> ZipSink C.length
             ) >>= yield)
        .| C.concatMap (\(xs, sum, len) -> map (,sum,len) xs)
        .| C.mapM_ print

repl3 = do
  let a = [["a", "aa", "aaa"], ["a", "bb", "bbb"], ["a", "aa", "ccc"], ["b", "bb", "bbb"]]
  let b = M.fromList [ ("a", M.fromList [ ("aa", M.fromList [("aaa", 1)])
                                        , ("bb", M.fromList [("bbb", 1)])
                                        , ("bb", M.fromList [("ccc", 1)]) ])
                     , ("b", M.fromList [ ("bb", M.fromList [("bbb", 1)]) ]) ]
            :: M.HashMap String (M.HashMap String (M.HashMap String Int))
  putStrLn "finished"

  -- M.empty