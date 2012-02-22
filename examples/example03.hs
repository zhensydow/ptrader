
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
import Control.Concurrent( ThreadId, forkIO, killThread, threadDelay )
import Control.Monad( forM, forever )
import System.Exit( exitSuccess )
import System.Posix.Signals( Handler(..), installHandler, sigINT )
import System.Posix.Unistd( sleep )
import PTrader.Report(
  Report, runReport, newLine, newScreen, outStrLn,
  stocksState, indexState, stocksProfit, showHolds )
import PTrader.Portfolio(
  runPortfolio, ownedStocks, calcStockNet, calcStockPrice, holds )
import PTrader.Graph( GraphConfig(..), runGraph )
import PTrader.Query( StockValue(..), getValue, getMulValue )
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
graphUpdate :: [String] -> IO [Double]
graphUpdate stocks = do
  vals <- forM stocks $ \sym -> do
    val <- getValue sym Bid
    return $ read val
  return vals

-- -----------------------------------------------------------------------------
graphConfig :: GraphConfig
graphConfig = GraphConfig Nothing 30

graphThread :: String -> IO ThreadId
graphThread filename = do
  stocks <- fmap (fmap fst) $ runPortfolio ownedStocks filename
  refvals <- fmap (fmap read) $ getMulValue stocks PreviousClose
  forkIO $ runGraph graphConfig stocks refvals (graphUpdate stocks)

-- -----------------------------------------------------------------------------
mainLoop :: String -> IO ()
mainLoop filename = do
  gtid <- graphThread filename
  _ <- installHandler sigINT (CatchOnce (killThread gtid >> exitSuccess)) Nothing
  forever $ do
    -- print myReport using colors
    runReport (myReport filename) True
    -- wait 1 minute
    sleep 60

-- -----------------------------------------------------------------------------
