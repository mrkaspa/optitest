{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, DuplicateRecordFields, RecordWildCards #-}

module Generators where

import Model
import Test.QuickCheck
import Data.Text as T

nonEmptyList gen = do
   x <- gen 
   xs <- listOf gen
   return (x:xs)

arbitraryNonEmptyTextNumber :: Gen T.Text
arbitraryNonEmptyTextNumber = T.pack <$> nonEmptyList (elements ['0'..'9'])


arbitraryNonEmptyText :: Gen T.Text
arbitraryNonEmptyText = T.pack <$> nonEmptyList (elements $ (' ':['A'..'Z'])++['a'..'z'])

arbitraryText :: Gen T.Text
arbitraryText = T.pack <$> listOf (elements $ (' ':['A'..'Z'])++['a'..'z'])

instance Arbitrary Task where
   arbitrary = do
      let task_type = "u"
          address = "address"
          address_extra = ""
          description = ""
          client = "cliente"
          phone = "1234567"
          num_doc = ""
          email = ""
          meta = ""
      weight <- choose(1,5000)
      volume <- choose(1,1000000)
      lat <- choose (4.647157,4.657636)
      lon <- choose (-74.050104,-74.077820)
      let city = "Bogota"
          dep_state = "Cundinamarca"
          country = "Colombia"
      return $ Task {..} 


-- minTaskSize must be a minimum of 3
defaultMinTaskSize :: Int
defaultMinTaskSize = 3

defaultMaxTaskSize :: Int
defaultMaxTaskSize = 100

defaultNumberOfTests :: Int
defaultNumberOfTests = 100

instance Arbitrary OptimizationData where
   arbitrary = sizedOptimizationData defaultMinTaskSize defaultMaxTaskSize

sizedOptimizationData :: Int -> Int -> Gen OptimizationData
sizedOptimizationData minSize maxSize = do
    vehicle_id <- (1+) <$> arbitrarySizedNatural 
    -- max_weight <- choose (1000,100000)
    -- max_vol <- choose (1000,100000::Int)
    (max_weight,max_vol) <- elements [(4500,45000000),(5000,50000000),(10000,100000000)]
    let measure_type = "time"
        departure = "now"
        calculation_type = "naive"
        algorithm = "ants"
    max_measure <- (*60) <$> choose(1,12)
    download_time <- choose(10,20)
    routes <- fixRoutes <$> arrayGen minSize maxSize 
    return $ OptimizationData {..}
  where fixRoutes [] = []
        fixRoutes (task:tasks) = task{task_type = "l"} : tasks

arrayGen :: Arbitrary a => Int -> Int -> Gen [a]
arrayGen = arrayFromGen arbitrary

arrayFromGen :: Gen a -> Int -> Int -> Gen [a]
arrayFromGen gen minSize maxSize = (++) <$> vectorOf minSize gen <*> resize diffSize (listOf gen)
    where diffSize = max (maxSize - minSize) minSize

