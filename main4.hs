import Network.Curl
import Control.Monad
import Control.Applicative
import System.Process
import Text.Printf
import GHC.IO.Exception

--page = "http://rusfolder.com/100000"
--page = "http://rusfolder.com/random/images/?session=91b288d3fc0e8893e72536344e423636&mem"
page = "http://rusfolder.com/random/images/?session=91b288d3fc0e8893e72536344e423636&mem"

loadCommand = "wget -O images/%d.gif --wait=9 --random-wait %s"
convertComand = "convert -verbose -coalesce images/%d.gif images/%d.png"

loadAndConvert :: Int -> IO GHC.IO.Exception.ExitCode
loadAndConvert i = (waitForProcess =<< (runCommand $ (printf loadCommand (i::Int) page))) >> (waitForProcess =<< (runCommand $ (printf convertComand i i)))
--loadAndConvert i = runCommand (printf loadCommand (i::Int) page) >> runCommand (printf convertComand i i)

main = do
    --mapM_ (loadAndConvert) [1..200]
    mapM_ (\i -> waitForProcess =<< (runCommand $ (printf loadCommand (i::Int) page))) [1..200]
    --print $ proc "ls" []
