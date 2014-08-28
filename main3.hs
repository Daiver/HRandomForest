import System.Random
import System.Environment
import qualified Data.List as List
import qualified Data.Map as Map
import Control.Monad
import Control.Arrow

import ML.Common
import ML.DecisionTree
import ML.RandomForest

main = do
    args <- getArgs
    let reader = if (args !! 0 == "f") then reasSamplesClassIsLead else reasSamplesClassIsLast
    rawText <- readFile $ args !! 1
    let trainDataSet = reader $ rawText
    print "reading train"
    rawText2 <- readFile $ args !! 2
    let testDataSet = reader $ rawText2 
    --
    --let tree = buildNode trainDataSet
    let tree = trainRandomForest (mkStdGen 2) trainDataSet
    --print tree
    --mapM_ (\sample -> print $ show (fst sample) ++ show (predictDecTree tree (snd sample))) testDataSet
    print $ (show $ testClassifier tree testDataSet) ++ "/" ++ (show $ length testDataSet)
