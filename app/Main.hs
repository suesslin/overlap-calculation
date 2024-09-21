{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where    

import System.IO
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import qualified Data.ByteString.Lazy as B
import Data.List (isSuffixOf)
import System.Directory
import Data.Either (isRight, fromRight)

data LayoutType = Baseline | RV | RH deriving (Show, Eq)
instance FromJSON LayoutType where
    parseJSON (String s) = case s of 
        "baseline"   -> pure Baseline
        "horizontal" -> pure RV
        "vertical"   -> pure RH
        _            -> fail $ "Invalid layout type: " ++ T.unpack s
    parseJSON _ = fail "Couldn't parse LayoutType"

data TracesDisplayingCondition = WithoutTraces | WithTraces deriving (Show, Eq)
instance FromJSON TracesDisplayingCondition where
    parseJSON (String s) = case s of
        "with_traces"    -> pure WithTraces
        "without_traces" -> pure WithoutTraces
        _                -> fail $ "Invalid trace displaying condition: " ++ T.unpack s
    parseJSON _ = fail "Couldn't parse TracesDisplayingCondition"
type Dimension = (Int, Int)

-- No FromJSON instance implementation needed because aeson handles tuples
newtype Coordinate = Coordinate (Int, Int)
  deriving (Show)

instance FromJSON Coordinate where
    parseJSON (Object v) =
        Coordinate <$> ((,) <$> v .: "x" <*> v .: "y")
    parseJSON _ = fail "Couldn't parse Coordinate"

data TouchPoint = TouchPoint
    { coordinate  :: Coordinate
    , timestamp   :: Int
    , currentPage :: Text
    , triggerId   :: Text
    } deriving (Show)

instance FromJSON TouchPoint where
    parseJSON (Object v) = 
        TouchPoint <$> v .: "coordinates"
                   <*> v .: "timestamp"
                   <*> v .: "currentPage"
                   <*> v .: "triggerId"
    parseJSON _ = fail "Couldn't parse TouchPoint"

data Item = Item
    { title  :: Text
    , amount :: Int
    , entity :: Double
    } deriving (Show)

instance FromJSON Item where
    parseJSON (Object v) = 
        Item <$> v .: "title"
             <*> v .: "amount"
             <*> v .: "entityPrice"
    parseJSON invalid = typeMismatch "Object" invalid

data Condition = Condition
    { layoutType                :: LayoutType
    , tracesDisplayingCondition :: TracesDisplayingCondition
    } deriving (Show)

instance FromJSON Condition where
    parseJSON (Object v) =
        Condition <$> v .: "layoutType"
                  <*> v .: "tracesDisplayingCondition"
    parseJSON _ = fail "Couldn't parse Condition"

data Interview = Interview
 { id           :: Text
 , condition    :: Condition
 , touchPoints  :: [TouchPoint]
 , taskNr       :: Int
 , cart         :: [Item]
} deriving (Show)

instance FromJSON Interview where
    parseJSON (Object v) = 
        Interview <$> v .: "interviewId"
                  <*> v .: "conditions"
                  <*> v .: "touchPoints"
                  <*> v .: "taskNr"
                  <*> v .: "cart"
    parseJSON _ = fail "Couldn't parse Interview"

main :: IO ()
main = do
    jsonData <- B.readFile "data/interview-article69_with_traces-traces_rearrangement-baseline_task-1.json"
    
    files <- listDirectory "data"
    let jsonFiles = fmap ((++) "data/") $ filter (isSuffixOf ".json") files

    let maybeInterview = eitherDecode jsonData :: Either String Interview
    maybeInterviews <- mapM decodeInterview $ jsonFiles
    
    let interviews = [i | Right i <- maybeInterviews]
    print interviews
    
    case maybeInterview of
        Right interview -> print interview
        Left err        -> putStrLn $ "There's an error: " ++ err

decodeInterview :: FilePath -> IO (Either String Interview)
decodeInterview path = do
    return . eitherDecode =<< B.readFile path 