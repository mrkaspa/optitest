{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, RecordWildCards, DuplicateRecordFields #-}

module Test
    ( testOptimizations
    ) where

import System.IO (stderr, hPutStrLn)
import Network.HTTP.Types.Header
import Network.Wreq
import Control.Lens
import Data.Aeson.Lens
import Data.Monoid
import qualified Data.Text as T
import Data.Text.Encoding as E
import Data.Aeson
import Model
import Generators
import Test.QuickCheck

optimization0 = OptimizationData
    { vehicle_id =  1
    , max_weight =  15
    , max_vol =  1
    , measure_type =  "time"
    , departure =  "now"
    , calculation_type =  "naive"
    , max_measure =  480
    , download_time =  20
    , algorithm =  "genetic"
    , routes = [task0, task1, task2]
    }

task0 = Task 
      { task_type =  "l"
      , address =  "Iglesia el señor de los milagros"
      , address_extra =  ""
      , description =  ""
      , city =  "Espinal"
      , dep_state =  "Sur espinal"
      , country =  "Colombia"
      , client =  "Iglesia"
      , phone =  ""
      , num_doc =  ""
      , email =  ""
      , meta =  ""
      , weight =  4
      , volume =  1
      , lat =  4.142657
      , lon = -74.881513
      }

task1 = Task
      { task_type =  "u"
      , address =  "Sitio 1"
      , address_extra =  ""
      , description =  ""
      , city =  "Espinal"
      , dep_state =  "Sur espinal"
      , country =  "Colombia"
      , client =  "Iglesia"
      , phone =  ""
      , num_doc =  ""
      , email =  ""
      , meta =  ""
      , weight =  4
      , volume =  1
      , lat =  4.143529
      , lon =  -74.877261
      }

task2 = Task
      { task_type =  "u"
      , address =  "Sitio 2"
      , address_extra =  ""
      , description =  ""
      , city =  "Espinal"
      , dep_state =  "Sur espinal"
      , country =  "Colombia"
      , client =  "Iglesia"
      , phone =  ""
      , num_doc =  ""
      , email =  ""
      , meta =  ""
      , weight =  4
      , volume =  1
      , lat =  4.149289
      , lon =  -74.882963
      }


authenticationData
    = AuthenticationData
    { email = "admin@liftit.co"
    , password = "liftit-2018"
    }

host :: String
host = "http://localhost:3000"

requestToken :: AuthenticationData -> IO Token
requestToken authenticationData = do 
    response <- post (host <> "/authenticate") $ toJSON authenticationData
    return $ response ^. responseBody . key "token" . _String

requestOptimization :: Token -> OptimizationData -> IO (OptimizationResponse OptimizationResponseData)
requestOptimization newToken optimizationData = do
    let options = defaults & header hAuthorization .~ ["Bearer " <> E.encodeUtf8 newToken]
    response <- asJSON =<< 
                postWith options (host <> "/optimizer") (toJSON optimizationData)
    return (response ^. responseBody)  

testOptimizations :: IO ()
testOptimizations = do 
    newToken <- requestToken authenticationData
    -- response <- requestOptimization newToken optimization0 
    mapM_ (\_ -> generate arbitrary >>= requestOptimization newToken) [1..100]

