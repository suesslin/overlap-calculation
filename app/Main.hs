{-# LANGUAGE OverloadedStrings #-}
module Main where    

import System.IO
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson


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

type Coordinate = (Int, Int)

data TouchPoint = TouchPoint
    { coordinate  :: Coordinate
    , timestamp   :: Int
    , currentPage :: Text
    , triggerId   :: Text
    } deriving (Show)

instance FromJSON TouchPoint where
    parseJSON (Object v) = 
        TouchPoint <$> v .: "coordinate"
                   <*> v .: "timestamp"
                   <*> v .: "currentPage"
                   <*> v .: "triggerId"
    parseJSON _ = fail "Couldn't parse TouchPoint"

data Cart = Cart
    { title  :: Text
    , amount :: Int
    , entity :: Double
    } deriving (Show)

instance FromJSON Cart where
    parseJSON (Object v) = 
        Cart <$> v .: "title"
             <*> v .: "amount"
             <*> v .: "entity"
    parseJSON _ = fail "Couldn't parse Cart"


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
 , cart         :: Cart
} deriving (Show)

-- .: stems from aeson package, takes an object and a key and returns a parser
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
    doReadFile "data/interview-article69_with_traces-traces_rearrangement-baseline_task-1.json"


doReadFile :: FilePath -> IO ()
doReadFile path = do
    handle <- openFile path ReadMode
    contents <- hGetContents handle
    putStrLn contents
