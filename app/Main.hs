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

import Data.QuadTree 

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

    -- All touch points for one given condition
    let interviewsOfOneCondition filterLayout filterTracesCondition = filter (\i -> layoutType (condition i) == filterLayout && tracesDisplayingCondition (condition i) == filterTracesCondition) interviews
    let touchPointsForLayout filterLayout filterTracesCondition = concatMap (map coordinate . touchPoints) (interviewsOfOneCondition filterLayout filterTracesCondition)

    -- Touch points for layouts
    let tpsBnoTr = touchPointsForLayout Baseline WithoutTraces
    let tpsRhNoTr = touchPointsForLayout RH WithoutTraces
    let tpsRvNoTr = touchPointsForLayout RV WithoutTraces
    
    -- Set of touch points for layouts without traces
    let tpsBnoTr = concatMap (map coordinate . touchPoints) interviewsForCondition where interviewsForCondition = interviewsOfOneCondition Baseline WithoutTraces
    
    -- Set of touch points for static UI
    -- Copied thrice to ensure equal amount of users as for the three layouts that simulate the adaptive UI
    let tpsStaticNoTr = concat $ replicate 3 tpsBnoTr

    -- Set of touch points for adaptive UI (without traces)
    -- Simulated by taking all tps over all uis (without traces)
    let tpsAdaptiveNoTr = tpsBnoTr ++ tpsRhNoTr ++ tpsRvNoTr

    -- Create tree 
    let tree tps = foldl (\t (Coordinate coord) -> setLocation coord True t) (makeTree (1920,1080) False) tps

    let staticTree = tree tpsStaticNoTr
    let adaptiveTree = tree tpsAdaptiveNoTr


    -- Breaks down into tiles
    -- Tiles are of type (a, Region), or (a, (Int, Int, Int, Int))
    -- where the region states x_min, y_min, x_max, y_max
    -- Meaning: The smaller the tile, the more points are accumulated in this small area
    let tiles tr = tile tr

    -- Calculation idea:
    -- Subregions become smaller 
    -- (smaller distance between xMax and and xMin and between yMax and yMin)
    -- if touchpoints are concentrated in this area.
    -- This concentration can be used a simplified indicator that there may be 
    -- more overlap (Though it is an approximation, not perfectly the same as overlap).
    -- We may therefore calculate the average area for each tile in the trees for each touch point data set.
    -- Area: (xMax-xMin) * (yMax-yMin) 
    --       regionArea exists in QuadTree library
    let averageArea tls = fromIntegral (sum (map (regionArea . snd) tls)) / fromIntegral (length tls)
    
    print . averageArea . tiles $ staticTree
    print . averageArea . tiles $ adaptiveTree

decodeInterview :: FilePath -> IO (Either String Interview)
decodeInterview path = do
    return . eitherDecode =<< B.readFile path 

-- regionDepth :: Region -> Int
-- regionDepth (xMin, yMin, xMax, yMax) 
--     = .
--         where width = xMax - xMin
--               height = yMax - yMin