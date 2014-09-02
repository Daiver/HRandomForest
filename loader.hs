import Network.Curl
import Control.Monad
import Control.Applicative
import System.Process
import Text.Printf
import GHC.IO.Exception

--page = "http://rusfolder.com/100000"
--page = "http://rusfolder.com/random/images/?session=91b288d3fc0e8893e72536344e423636&mem"
page = "http://rusfolder.com/random/images/?session=91b288d3fc0e8893e72536344e423636&mem"

loadCommand = "curl '%s' >> images/%d.gif ; sleep 1"
--loadCommand = "wget -O images/%d.gif --wait=9 --random-wait %s && touch file.is.done"
convertComand = "convert -verbose -coalesce images/%d.gif images/%d.png"

loadAndConvert :: Int -> IO GHC.IO.Exception.ExitCode
loadAndConvert i = (system $ (printf loadCommand page (i::Int) )) >> (system $ (printf convertComand i i))
--loadAndConvert i = runCommand (printf loadCommand (i::Int) page) >> runCommand (printf convertComand i i)

main = do
    mapM_ (loadAndConvert) [1..500]
    --mapM_ (\i -> waitForProcess =<< (runCommand $ (printf loadCommand (i::Int) page))) [1..2]
    --print $ proc "ls" []
