import Control.Monad
import Control.Applicative
import Control.Arrow
import Data.Word
import Data.Binary.Get
import qualified Data.ByteString.Lazy as BStringLazy
import qualified Data.List as List

import System.Random

import ML.RandomForest

readListOfWord8N n = do
    replicate (fromIntegral n) $ getWord8 

readLabels :: Get (Word32, [Word8])
readLabels = do
    magic <- getWord32be
    count <- getWord32be
    list  <- sequence $ readListOfWord8N count
    return (count, list)

readImages = do
    magic <- getWord32be
    count <- getWord32be
    
    rows <- getWord32be
    cols <- getWord32be
    list <- sequence $ replicate (fromIntegral count) (sequence $ readListOfWord8N (rows * cols))
    return (count, (rows, cols), list)

makeSamples :: (Integral a) => [a] -> [[a]] -> [(Int, [Float])]
makeSamples labels images = map ( fromIntegral *** (map fromIntegral)) $ zip labels images

tmpWork samples = map f [0 .. length (head dt) - 1] 
    where
        n = length samples
        dt = map snd samples
        f feat = List.sortBy (\a b -> compare (dt!!a!!feat) (dt!!b!!feat)) [0 .. n - 1]

main = do
    let labelsFileName = "/home/daiver/Downloads/t10k-labels-idx1-ubyte"
    content <- BStringLazy.readFile labelsFileName
    let(countOfLabels, labels) = runGet (readLabels) content
    print $ (countOfLabels, length labels)
    let imagesFileName = "/home/daiver/Downloads/t10k-images-idx3-ubyte"
    content <- BStringLazy.readFile imagesFileName
    let (s, (r, c), images) = runGet (readImages) content
    print $ (s, r, c, length images, length . head $ images) --}

    let samples = makeSamples labels images
    print "Start train"
    print $ length . head $ tmpWork samples
    --let tree = trainRandomForest (mkStdGen 2) $ take 20 samples
    --print tree
    print "End"
