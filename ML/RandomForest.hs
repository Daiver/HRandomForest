module ML.RandomForest where

import ML.Common
import ML.DecisionTree

import System.Random
import qualified Data.Map as Map
import qualified Data.List as List

data RandomForest = RandomForest [DecisionTree]
    deriving Show

instance Classifier RandomForest where
    predict (RandomForest trees) sample = fst . maximumWith snd . counts . map (flip predict sample) $ trees
    fit samples = RandomForest []

trainRandomForest gen samples = RandomForest trees
    where 
        (seeds, _) = randomSeq gen 20
        ans = map (flip randomSamplingWithRepeats samples . mkStdGen) seeds
        numberOfFeats = length . snd . head $ samples
        trees = map (\x -> buildForestNode (take (numberOfFeats `div` 2) . fst $ shuffle [0..length (snd $ samples !! 0) - 1] (snd x)) (fst x)) ans

buildForestNode featIdxs samples
    | bestGain > 0 = let (r, l) = divide samples featIdx thr in Node featIdx thr (buildNode l) (buildNode r)
    | otherwise = Leaf . counts . map fst $ samples
    where 
        (bestGain, featIdx, thr) = findBestThrBruteForce featIdxs samples

