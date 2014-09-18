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

readLabels = do
    magic <- getWord32be
    count <- getWord32be
    list  <- listOfWord8 
    return (count, list)

main = do
    content <- BStringLazy.readFile "/home/daiver/Downloads/t10k-labels-idx1-ubyte"
    print $ runGet (readLabels) content
