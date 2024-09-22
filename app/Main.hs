{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Main where    

import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import qualified Data.ByteString.Lazy as B
import Data.List (isSuffixOf)
import System.Directory

import Data.QuadTree (makeTree, setLocation, fuseTree, tile)

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
    files <- listDirectory "data"
    let jsonFiles = fmap ((++) "data/") $ filter (isSuffixOf ".json") files

    maybeInterviews <- mapM decodeInterview $ jsonFiles
    
    let interviews = [i | Right i <- maybeInterviews]

    let filterLayout = Baseline
    let filterTracesCondition = WithoutTraces

    -- All touch points for one given condition
    let interviewsOfOneCondition = filter (\i -> layoutType (condition i) == filterLayout && tracesDisplayingCondition (condition i) == filterTracesCondition) interviews
   
    let allTouchPointCoords = concatMap (map coordinate . touchPoints) interviewsOfOneCondition

    -- mapM_ putStrLn $ map (\(Coordinate coord) -> show coord) allTouchPointCoords

    let tree = foldl (\t (Coordinate coord) -> setLocation coord True t) (makeTree (1920,1080) False) allTouchPointCoords
    let fusedTree = fuseTree tree

    mapM_ print $ tile fusedTree

decodeInterview :: FilePath -> IO (Either String Interview)
decodeInterview path = do
    return . eitherDecode =<< B.readFile path 
