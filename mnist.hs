import Control.Monad
import Data.Word
import Data.Binary.Get
import qualified Data.ByteString.Lazy as BStringLazy

listOfWord8 = do
    empty <- isEmpty
    if empty
        then return []
        else do 
            v <- getWord8
            rest <- listOfWord8 
            return (v : rest)

readListOfWord8N n = do
    return . replicate (fromIntegral n) $ getWord8 

readLabels = do
    magic <- getWord32be
    count <- getWord32be
    --list  <- listOfWord8 
    list <- readListOfWord8N count
    return (count, list)

readImages = do
    magic <- getWord32be
    count <- getWord32be
    
    rows <- getWord32be
    cols <- getWord32be
    list <- replicateM (fromIntegral count) (readListOfWord8N (rows * cols))
    return (count, (rows, cols), list)

main = do
    let labelsFileName = "/home/daiver/Downloads/t10k-labels-idx1-ubyte"
    content <- BStringLazy.readFile labelsFileName
    let(countOfLabels, labels) = runGet (readLabels) content
    print $ (countOfLabels, length labels)
    let imagesFileName = "/home/daiver/Downloads/t10k-images-idx3-ubyte"
    content <- BStringLazy.readFile imagesFileName
    let (s, (r, c), images) = runGet (readImages) content
    print $ (s, r, c, length images) --}
