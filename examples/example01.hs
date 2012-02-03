import PTrader.Report
import Control.Monad
import System.Posix.Unistd

main = do
  let stocks = ["ENG.MC","SAN.MC","TEF.MC","OHL.MC","FER.MC"]
  
  forever $ do
    runReport (newScreen >> stocksState stocks) True
    sleep 60
