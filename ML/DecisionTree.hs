module ML.DecisionTree where

import System.Random
import qualified Data.List as List
import qualified Data.Map as Map
import Control.Monad
import Control.Arrow
import Debug.Trace

import ML.Common

data DecisionTree = Node Int Float (DecisionTree) (DecisionTree) | Leaf [(Int, Int)]
    deriving Show

instance Classifier DecisionTree where
    predict tree feats = predictDecTree tree feats
    fit samples = buildNode samples

commonGain :: ([Int] -> Float) -> [Int] -> [Int] -> [Int] -> Float
commonGain h s s1 s2 = h s - (l1/l) * h s1 - (l2/l) * h s2
    where 
        l  = fromIntegral $ length s
        l1 = fromIntegral $ length s1
        l2 = fromIntegral $ length s2

gini :: [Float] -> Float
gini fr = 1 - (sum . map (**2) $ fr)

divide samples featIdx thr = List.partition ((>= thr) . item featIdx . snd) samples

findBestThrBruteForce featIndexes samples = List.maximumBy (\(x,_,_) (y,_,_) -> compare x y) genProps
    where
        countOfFeats = length . snd $ (samples !! 0 )
        h = gini . freqs . map snd . counts
        labels = map fst samples
        initGain = h labels
        pairs = [(i, (snd s) !! i) | i <- featIndexes, s <- samples]
        computeGainOfDivide i thr = uncurry (commonGain h labels) . mapTuple (map fst) $ divide samples i thr
        genProps = map (\(i, thr) -> (computeGainOfDivide i thr, i, thr)) pairs

buildNode :: [(Int, [Float])] -> DecisionTree
buildNode samples
    | bestGain > 0 = let (r, l) = divide samples featIdx thr in Node featIdx thr (buildNode l) (buildNode r)
    | otherwise = Leaf . counts . map fst $ samples
    where 
        (bestGain, featIdx, thr) = findBestThrIterative [0..length (snd $ samples !! 0) - 1] samples
        --(bestGain, featIdx, thr) = findBestThrBruteForce [0..length (snd $ samples !! 0) - 1] samples

predictDecTree :: DecisionTree  -> [Float] -> Int
predictDecTree (Leaf vals) _ = fst . List.maximumBy (\x y -> compare (snd x) (snd y)) $ vals
predictDecTree (Node featIdx thr l r) feats
    | feats !! featIdx >= thr = predict r feats
    | otherwise               = predict l feats



{-New-}
updateElem :: (a -> a) -> [a] -> Int -> [a]
updateElem f li i = 
    let(bef, aft) = splitAt i li in concat $ [bef, [f $ head aft], tail aft]

findBestThrIterative :: [Int] -> [(Int, [Float])] -> (Float, Int, Float)
findBestThrIterative featIndexes samples 
    | length initWr > 1 = (bestGain, bestFeat, bestThr)
    | otherwise = (0,0,0)
    where 
        (_, _, bestThr, bestFeat, bestGain) = foldl (foldlCore labels dt initGain) 
              (initWl, initWr, dt !! 0 !! 0, 0, initGain) $ indexesAndFeats
        labels = map fst samples
        dt     = map snd samples
        initWr = map snd $ counts labels
        initWl = replicate (length initWr) 0
        initGain = giniFromCounts . map snd . counts $ labels
        countOfFeats = length $ dt !! 0
        indexesAndFeats = concat $ map 
                            (\f ->map ((,) f) $ List.sortBy 
                                (\a b -> compare (dt!!a!!f) (dt!!b!!f)) 
                                [0.. length dt - 1]) 
                            featIndexes


foldlCore labels dt initGain state@(wl, wr, bestThr, bestFeat, bestGain) (idx, feat)
    | gain > bestGain = (wl', wr', thr, feat, gain)
    | otherwise = (wl', wr', bestThr, bestFeat, bestGain)
    where
        l   = fromIntegral $ length labels
        l1  = fromIntegral $ sum wl
        l2  = fromIntegral $ sum wr
        label = trace (show $ labels !! idx) $ labels !! idx
        thr = dt !! idx !! feat
        wl' = updateElem (+1) wl label
        wr' = updateElem (\x -> x - 1) wr label
        gain = initGain - giniFromCounts wl' * l1/l + giniFromCounts wr' * (l2/l)

giniFromCounts :: [Int] -> Float
giniFromCounts counts = 
    1 - (fromIntegral (sum $ map (^2) counts) / fromIntegral (length counts ^ 2))


