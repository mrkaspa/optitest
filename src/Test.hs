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

authenticationData :: AuthenticationData
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

