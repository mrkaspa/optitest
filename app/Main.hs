{-# LANGUAGE RecordWildCards #-}

module Main where

import Test
import Options.Applicative
import Data.Semigroup ((<>))
import Model (Parameters(..))
import Generators (defaultMinTaskSize, defaultMaxTaskSize, defaultNumberOfTests)

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

theTestCases :: Parser Int
theTestCases = option auto
    ( long "tests"
    <> short 't'
    <> metavar "TESTS"
    <> value defaultNumberOfTests
    <> help "Number of tests" )

parseParameters :: Parser Parameters
parseParameters = Parameters <$> theUrl <*> theMinSize <*> theMaxSize <*> theTestCases

commandLine :: ParserInfo Parameters
commandLine = info (parseParameters <**> helper) idm

validateParameters :: Parameters -> IO ()
validateParameters Parameters{..} | minTaskSize > maxTaskSize = fail "The maximum task size must be equal or greather than the minimum task size" 
                                  | minTaskSize < 3 = fail "The minimun task size is 3"
                                  | numberOfTests < 1 = fail "Invalid number of tests"
                                  | otherwise = return ()

main :: IO ()
main = do 
    parameters <- execParser commandLine
    validateParameters parameters
    testOptimizations parameters
