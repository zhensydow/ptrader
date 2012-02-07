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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module PTrader.Report(
  -- * Report Monad
  Report, runReport,
  -- * Report functions
  clearColor, newScreen, newLine, outStrLn, outStr, setBackgroundColor,
  setForegroundColor,
  -- * Pre-defined Reports
  stocksState, indexState, stocksProfit
  ) where

-- -----------------------------------------------------------------------------
import Control.Monad( when, forM_ )
import Control.Monad.IO.Class( MonadIO, liftIO )
import Control.Monad.Reader( MonadReader, ReaderT, runReaderT, ask )
import Control.Monad.State( MonadState, StateT, runStateT, modify )
import System.Console.ANSI(
  SGR(..), ConsoleLayer(..), Color(..), ColorIntensity(..),
  setSGRCode, clearScreenCode, setCursorPositionCode )
import PTrader.Query( StockValue(..), getMulValues )
import PTrader.Types( StockSymbol, CashValue )

-- -----------------------------------------------------------------------------
data ReportConfig = ReportConfig { reportInColor :: ! Bool }

-- -----------------------------------------------------------------------------
newtype Report a = Report
                   { runR :: StateT [String] (ReaderT ReportConfig IO) a }
                   deriving( Functor, Monad, MonadIO, MonadReader ReportConfig
                           , MonadState [String])

-- -----------------------------------------------------------------------------
runReport :: MonadIO m => Report a -> Bool -> m a
runReport report color = do
  let config = ReportConfig color
  (ret, out) <- liftIO $ runReaderT (runStateT (runR report) []) config
  liftIO . putStrLn . concat . reverse $ out
  return ret

-- -----------------------------------------------------------------------------
io :: IO a -> Report a
io = liftIO

-- -----------------------------------------------------------------------------
hasColor :: Report Bool
hasColor = fmap reportInColor ask

-- -----------------------------------------------------------------------------
setBackgroundColor :: ColorIntensity -> Color -> Report ()
setBackgroundColor i c = do
  color <- hasColor
  when color
    (modify (setSGRCode [SetColor Background i c] :))

-- -----------------------------------------------------------------------------
setForegroundColor :: ColorIntensity -> Color -> Report ()
setForegroundColor i c = do
  color <- hasColor
  when color
    (modify (setSGRCode [SetColor Foreground i c] :))

-- -----------------------------------------------------------------------------
clearColor :: Report ()
clearColor = do
  setBackgroundColor Dull Black
  setForegroundColor Vivid White

-- -----------------------------------------------------------------------------
newScreen :: Report ()
newScreen = modify ((clearScreenCode++(setCursorPositionCode 0 0)):)

-- -----------------------------------------------------------------------------
outStrLn :: String -> Report ()
outStrLn msg = modify ((msg++"\n") :)

outStr :: String -> Report ()
outStr msg = modify (msg :)

-- -----------------------------------------------------------------------------
newLine :: Report ()
newLine = modify ("\n" :)

-- -----------------------------------------------------------------------------
stocksState :: [StockSymbol] -> Report ()
stocksState stocks = do
  setForegroundColor Vivid Black
  outStrLn "Name\t\t\tValue\tChange\tOpen\tMin\tMax"
  clearColor
  dat <- io $ getMulValues stocks stockVals
  forM_ dat outStockState
    where
      stockVals = [StockName, Ask, PercentChange, Open, DayLow, DayHigh]

outStockState :: [String] -> Report ()
outStockState vals = do
  outName
  outStr ((vals !! 1) ++ "\t")
  outChange
  clearColor
  forM_ (drop 3 vals) (outStr . (++ "\t"))
  newLine

    where
      name = read (head vals) :: String
      outName = do
        setForegroundColor Dull Blue
        outStr (name ++ "\t")
        when (length name <8) $ outStr "\t"
        when (length name <16) $ outStr "\t"
        clearColor

      change = read (vals !! 2) :: String
      outChange = do
        if head change == '-'
          then setForegroundColor Vivid Red
          else setForegroundColor Vivid Green
        outStr (change ++ "\t")

-- -----------------------------------------------------------------------------
indexState :: [StockSymbol] -> Report ()
indexState idx = do
  setForegroundColor Vivid Black
  outStrLn "Name\t\tChange\tOpen\tMin\tMax"
  clearColor
  dat <- io $ getMulValues idx stockVals
  forM_ dat outIndexState
    where
      stockVals = [StockName, PercentChange, Open, DayLow, DayHigh]

outIndexState :: [String] -> Report ()
outIndexState vals = do
  outName
  outChange
  forM_ (drop 2 vals) (outStr . (++"\t"))
  newLine
    where
      name = read (head vals) :: String
      outName = do
        setForegroundColor Dull Blue
        outStr (name ++ "\t")
        when (length name <8) $ outStr "\t"
        clearColor

      change = read (vals !! 1) :: String
      outChange = do
        if head change == '-'
          then setForegroundColor Vivid Red
          else setForegroundColor Vivid Green
        outStr (change ++ "\t")
        clearColor

-- -----------------------------------------------------------------------------
stocksProfit :: [((StockSymbol,Int),CashValue)] -> Report ()
stocksProfit stocks = do
  setForegroundColor Vivid Black
  outStrLn "Name\tAmount\tValue\t\tSpent\t\tProfit"
  clearColor
  dat <- io $ getMulValues (fmap (fst.fst) stocks) stockVals
  forM_ (zip3 (fmap fst stocks) (fmap snd stocks) dat) outStockProfit 
    where
      stockVals = [Bid]

outStockProfit :: ((StockSymbol,Int),CashValue,[String]) -> Report ()
outStockProfit ((name,amount), spent, vals) = do
  outName
  outStr $ show amount ++ "\t"
  outValue
  outSpent
  outProfit
  newLine
    where
      outName = do
        setForegroundColor Dull Blue
        outStr (name ++ "\t")
        clearColor
      price = (fromRational . toRational) (read (head vals)::Double) :: CashValue
      value = price * fromIntegral amount
      profit = value - spent
      percent = (value * 100) / spent
      outValue = do
        outStr $ show value ++ "\t"
        when (value < 1000) $ outStr "\t"
      outSpent = do
        outStr $ show spent ++ "\t"
        when (spent < 1000) $ outStr "\t"        
      outProfit = do
        if profit < 0
          then setForegroundColor Vivid Red
          else setForegroundColor Vivid Green
        outStr $ show profit ++ " (" ++ show percent ++ "%)"
        clearColor

-- -----------------------------------------------------------------------------
