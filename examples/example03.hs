{- -----------------------------------------------------------------------------
PTrader is a Personal Stock Trader Toolbox.
Copyright (C) 2012  Luis Cabellos

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
----------------------------------------------------------------------------- -}
module Main where

-- -----------------------------------------------------------------------------
import Control.Concurrent( ThreadId, forkIO, killThread )
import Control.Monad( forM, forM_, forever )
import Safe( readDef )
import System.Exit( exitSuccess )
import System.Posix.Signals( Handler(..), installHandler, sigINT )
import System.Posix.Unistd( sleep )
import PTrader.Report(
  Report, runReport, newLine, newScreen, outStrLn,
  stocksState, indexState, stocksProfit, showHolds )
import PTrader.Portfolio(
  runPortfolio, ownedStocks, calcStockNet, calcStockPrice, holds, logStockValue )
import PTrader.Graph( GraphConfig(..), runGraph )
import PTrader.Query( StockValue(..), getMulValue )
import PTrader.Util( timeStamp )

-- -----------------------------------------------------------------------------
indexes :: [String]
indexes = ["^IBEX"]

myReport :: String -> Report ()
myReport filename = do
  stocks <- runPortfolio ownedStocks filename
  -- clear console screen
  newScreen
  -- print the state of indexes
  indexState indexes
  -- print the state of selected stocks
  stocksState (fmap fst stocks)
  -- print teoric profit
  spents <- runPortfolio (forM (map fst stocks) calcStockNet) filename
  stocksProfit (zip stocks spents)
  xs <- runPortfolio holds filename
  hs <- forM xs $ \(s,y,z) -> do
    p <- runPortfolio (calcStockPrice s) filename
    return (s,y,z,p)
  showHolds hs
  newLine
  timeStamp >>= outStrLn

-- -----------------------------------------------------------------------------
myDoubleRead :: String -> Double
myDoubleRead [] = 0
myDoubleRead (x:xs)
  | x == '+' = readDef 0 xs
  | otherwise = readDef 0 (x:xs)

-- -----------------------------------------------------------------------------
graphUpdate :: [String] -> [String] -> IO [Double]
graphUpdate inds stocks = do
  vis <- fmap (fmap myDoubleRead) $ getMulValue inds Change
  vss <- fmap (fmap myDoubleRead) $ getMulValue stocks Bid
  return $! vis ++ vss

-- -----------------------------------------------------------------------------
graphConfig :: GraphConfig
graphConfig = GraphConfig Nothing 30

graphThread :: String -> IO ThreadId
graphThread filename = do
  stocks <- fmap (fmap fst) $ runPortfolio ownedStocks filename
  refvals <- fmap (fmap $ readDef 0)
             $ getMulValue stocks PreviousClose :: IO [Double]
  let zeros = replicate (length indexes) 0
  forkIO $ runGraph graphConfig (indexes ++ stocks) (zeros ++ refvals)
    (graphUpdate indexes stocks)

-- -----------------------------------------------------------------------------
logYesterday :: String -> IO ()
logYesterday filename = do
  stocks <- fmap (fmap fst) $ runPortfolio ownedStocks filename
  forM_ (indexes ++ stocks) $ \s ->
    runPortfolio (logStockValue s) filename

-- -----------------------------------------------------------------------------
mainLoop :: String -> IO ()
mainLoop filename = do
  gtid <- graphThread filename
  logYesterday filename
  _ <- installHandler sigINT (CatchOnce (killThread gtid >> exitSuccess)) Nothing
  forever $ do
    -- print myReport using colors
    runReport (myReport filename) True
    -- wait 1 minute
    sleep 60

-- -----------------------------------------------------------------------------
