{-# LANGUAGE RecordWildCards #-}

module Main where

import Test
import Options.Applicative
import Data.Semigroup ((<>))
import Model (Parameters(..))
import Generators (defaultMinTaskSize, defaultMaxTaskSize)

theUrl :: Parser String 
theUrl = strOption
    ( long "url"
    <> short 'u'
    <> metavar "URL"
    <> value "http://localhost:3000"
    <> help "Url to send the generated requests" )

theMinSize :: Parser Int
theMinSize = option auto
    ( long "min-task-size"
    <> short 'm'
    <> metavar "MIN_TASK_SIZE"
    <> value defaultMinTaskSize
    <> help "Minimum route size (must be bigger than 3)" )

theMaxSize :: Parser Int
theMaxSize = option auto
    ( long "max-task-size"
    <> short 'M'
    <> metavar "MAX_TASK_SIZE"
    <> value defaultMaxTaskSize
    <> help "Maximum route size (must be equal or bigger than minsize)" )


parseParameters :: Parser Parameters
parseParameters = Parameters <$> theUrl <*> theMinSize <*> theMaxSize

commandLine :: ParserInfo Parameters
commandLine = info (parseParameters <**> helper) idm

validateParameters :: Parameters -> IO ()
validateParameters Parameters{..} | minTaskSize > maxTaskSize = error "The maximum task size must be equal or greather than the minimum task size" 
                                  | minTaskSize < 3 = error "The minimun task size is 3"
                                  | otherwise = return ()

main :: IO ()
main = do 
    parameters@Parameters{..} <- execParser commandLine
    validateParameters parameters
    testOptimizations parameters
