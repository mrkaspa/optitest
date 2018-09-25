{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, DuplicateRecordFields, RecordWildCards #-}

module Generators where

import Model
import Test.QuickCheck
import Data.Text as T

nonEmptyList gen = do
   x <- gen 
   xs <- listOf gen
   return (x:xs)

arbitraryNonEmptyText :: Gen T.Text
arbitraryNonEmptyText = T.pack <$> nonEmptyList (elements $ (' ':['A'..'Z'])++['a'..'z'])

arbitraryText :: Gen T.Text
arbitraryText = T.pack <$> arbitrary

instance Arbitrary Task where
   arbitrary = do
      task_type <- arbitraryNonEmptyText
      address <- arbitraryNonEmptyText
      address_extra <- arbitraryText
      description <- arbitraryText
      client <- arbitraryNonEmptyText
      phone <- arbitraryText
      num_doc <- arbitraryText
      email <- arbitraryText
      meta <- arbitraryText
      weight <- choose(1,100)
      volume <- choose(1,100)
      lat <- choose (4.647157,4.657636)
      lon <- choose (-74.050104,-74.077820)
      let city = "Bogota"
          dep_state = "Cundinamarca"
          country = "Colombia"
      return $ Task {..} 


-- minTaskSize mus be minimum 3
defaultMinTaskSize = 3
defaultMaxTaskSize = 100 

instance Arbitrary OptimizationData where
   arbitrary = sizedOptimizationData defaultMinTaskSize defaultMaxTaskSize

sizedOptimizationData :: Int -> Int -> Gen OptimizationData
sizedOptimizationData minSize maxSize = do
    vehicle_id <- (1+) <$> arbitrarySizedNatural 
    max_weight <- choose (1,100)
    max_vol <- choose (1,100::Int)
    let measure_type = "time"
        departure = "now"
        calculation_type = "naive"
        algorithm = "ants"
    max_measure <- choose(4*60,8*60)
    download_time <- choose(10,20)
    routes <- arrayGen minSize maxSize  
    return $ OptimizationData {..}

arrayGen :: Arbitrary a => Int -> Int -> Gen [a]
arrayGen minSize maxSize = (++) <$> vector minSize <*> resize diffSize arbitrary
    where diffSize = max (maxSize - minSize) minSize
