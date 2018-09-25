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
    { email = "optiLiftiAdmin@liftit.co"
    , password = "liftit_2018"
    }

requestToken :: Parameters -> AuthenticationData -> IO Token
requestToken Parameters {..} authenticationData = do 
    response <- post (host <> "/authenticate") $ toJSON authenticationData
    return $ response ^. responseBody . key "token" . _String

requestOptimization :: Token -> Parameters -> OptimizationData -> IO (OptimizationResponse OptimizationResponseData)
requestOptimization newToken Parameters{..} optimizationData = do
    let options = defaults & header hAuthorization .~ ["Bearer " <> E.encodeUtf8 newToken]
    response <- asJSON =<< 
                postWith options (host <> "/optimizer") (toJSON optimizationData)
    return (response ^. responseBody)  

testOptimizations :: Parameters -> IO ()
testOptimizations parameters@Parameters{..} = do 
    newToken <- requestToken parameters authenticationData
    let test = generate (sizedOptimizationData minTaskSize maxTaskSize) 
               >>= requestOptimization newToken parameters
    mapM_ (\_ -> test) [1..numberOfTests]

