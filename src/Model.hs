{-# LANGUAGE OverloadedStrings, DeriveGeneric, ScopedTypeVariables, DuplicateRecordFields, RecordWildCards #-}

module Model where

import Data.Aeson
import GHC.Generics
import qualified Data.Text as T
import Control.Monad

data Parameters 
    = Parameters 
    { host :: String
    , minTaskSize :: Int
    , maxTaskSize :: Int
    , numberOfTests :: Int
    }
    deriving (Show, Eq)


type Latitude = Double
type Longitude = Double

type Token = T.Text

data Task
    = Task
    { task_type:: T.Text
    , address:: T.Text
    , address_extra:: T.Text
    , description:: T.Text
    , city:: T.Text
    , dep_state:: T.Text
    , country:: T.Text
    , client:: T.Text
    , phone:: T.Text
    , num_doc:: T.Text
    , email:: T.Text
    , meta:: T.Text
    , weight:: Int
    , volume:: Int
    , lat:: Latitude
    , lon:: Longitude
    }
   deriving (Show, Generic, Eq)

data OptimizationData
    = OptimizationData
    { vehicle_id :: Int
    , max_weight :: Int
    , max_vol :: Int
    , measure_type :: T.Text
    , departure :: T.Text
    , calculation_type :: T.Text
    , max_measure :: Int
    , download_time :: Int
    , algorithm :: T.Text
    , routes :: [Task]  
    } 
    deriving (Show, Generic, Eq)

data OptimizationResponseData
    = OptimizationResponseData
    { reference :: T.Text
    , code :: T.Text
    , description :: T.Text
    }
    deriving (Show, Generic, Eq)

data OptimizationResponse a
    = OptimizationResponse 
    { code :: T.Text
    , message :: T.Text
    , data_ :: a
    }
    deriving (Show, Generic, Eq)

data AuthenticationData
    = AuthenticationData
    { email :: T.Text
    , password :: T.Text
    }
    deriving (Show, Generic, Eq)

data AuthenticationResponse
    = AuthenticationResponse 
    { token :: Token }
    deriving (Show, Generic, Eq)

instance FromJSON OptimizationResponseData
instance ToJSON OptimizationResponseData

instance FromJSON a => FromJSON (OptimizationResponse a) where
    parseJSON (Object v) = OptimizationResponse <$>
                            v .: "code" <*>
                            v .: "message" <*>
                            v .: "data"
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _          = mzero
instance ToJSON a => ToJSON (OptimizationResponse a) where
      toJSON OptimizationResponse{..} =
             object ["code" .= code, "message" .= message, "data" .= data_]


instance FromJSON AuthenticationData
instance ToJSON AuthenticationData

instance FromJSON AuthenticationResponse
instance ToJSON AuthenticationResponse

instance FromJSON Task
instance ToJSON Task

instance ToJSON OptimizationData
instance FromJSON OptimizationData


