
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
import Control.Monad( forM, forever )
import Control.Monad.IO.Class( liftIO )
import System.Exit( exitSuccess )
import System.Posix.Unistd( sleep )
import System.Posix.Signals( Handler(..), installHandler, sigINT )
import PTrader.Report(
  Report, runReport, newLine, newScreen, outStrLn,
  stocksState, indexState, stocksProfit )
import PTrader.Portfolio( runPortfolio, ownedStocks, calcStockNet )
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
  newLine
  timeStamp >>= outStrLn

-- -----------------------------------------------------------------------------
mainLoop :: String -> IO ()
mainLoop filename = do
  _ <- installHandler sigINT (CatchOnce exitSuccess) Nothing
  forever $ do
    -- print myReport using colors
    runReport (myReport filename) True
    -- wait 1 minute
    sleep 60

-- -----------------------------------------------------------------------------
