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
{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, TypeSynonymInstances #-}
module PTrader.Report(
  -- * Report Monad
  Report, runReport,
  -- * Report functions
  clearColor, newScreen, newLine, outStrLn, outStr, setBackgroundColor,
  setForegroundColor,
  -- * Pre-defined Reports
  stocksState, indexState, stocksProfit, showHolds
  ) where

-- -----------------------------------------------------------------------------
import Control.Monad( when, forM_ )
import Control.Monad.IO.Class( MonadIO, liftIO )
import Control.Monad.Reader( MonadReader, ReaderT, runReaderT, ask )
import Control.Monad.State( MonadState, StateT, runStateT, modify )
import Data.Time.Calendar( Day )
import System.Console.ANSI(
  SGR(..), ConsoleLayer(..), Color(..), ColorIntensity(..),
  setSGRCode, clearScreenCode, setCursorPositionCode )
import PTrader.Query( StockValue(..), getMulValues, getMulValue )
import PTrader.Types( StockSymbol, CashValue )

-- -----------------------------------------------------------------------------
data ReportConfig = ReportConfig { reportInColor :: ! Bool }

-- -----------------------------------------------------------------------------
newtype Report a = Report
                   { runR :: StateT [String] (ReaderT ReportConfig IO) a }
                   deriving( Functor, Monad, MonadIO, MonadReader ReportConfig
                           , MonadState [String])

-- -----------------------------------------------------------------------------
class RedGreen a where
  isRed :: a -> Bool
  isGreen :: a -> Bool
  isGreen = not . isRed

instance RedGreen [Char] where
  isRed [] = False
  isRed (x:_) = x == '-'

instance RedGreen CashValue where
  isRed x = x < 0

instance (Ord a) => RedGreen (a,a) where
  isRed (base,value) = value < base

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
newScreen = modify ((clearScreenCode ++ setCursorPositionCode 0 0):)

-- -----------------------------------------------------------------------------
outStrLn :: String -> Report ()
outStrLn msg = modify ((msg++"\n") :)

outStr :: String -> Report ()
outStr msg = modify (msg :)

outTab :: Report ()
outTab = outStr "\t"

-- -----------------------------------------------------------------------------
newLine :: Report ()
newLine = modify ("\n" :)

-- -----------------------------------------------------------------------------
setRGColor :: RedGreen a => a -> Report ()
setRGColor value = if isRed value
                   then setForegroundColor Vivid Red
                   else setForegroundColor Vivid Green

-- -----------------------------------------------------------------------------
outMainName :: String -> Int -> Report ()
outMainName name tabs = do
  setForegroundColor Vivid Blue
  outStr name
  when (tabs > 0) outTab
  forM_ [2..tabs] $ \i -> when (length name < (i-1)*8) outTab
  clearColor

outRGString :: String -> Report ()
outRGString val = setRGColor val >> outStr val >> outTab >> clearColor

-- -----------------------------------------------------------------------------
stocksState :: [StockSymbol] -> Report ()
stocksState [] = return ()
stocksState stocks = do
  setForegroundColor Vivid Black
  outStrLn "Name\t\t\tValue\tChange\tOpen\tMin\tMax"
  clearColor
  dat <- io $ getMulValues stocks stockVals
  forM_ dat outStockState
    where
      stockVals = [StockName, Ask, PercentChange, Open, DayLow, DayHigh]

outStockState :: [String] -> Report ()
outStockState vals
  | length vals < 3 = return ()
  | otherwise = do
    outMainName (read . head $ vals) 3
    outStr (vals !! 1) >> outTab
    outRGString $ read (vals !! 2)
    forM_ (drop 3 vals) ((>>outTab) . outStr)
    newLine

-- -----------------------------------------------------------------------------
indexState :: [StockSymbol] -> Report ()
indexState [] = return ()
indexState idx = do
  setForegroundColor Vivid Black
  outStrLn "Name\t\tChange\tOpen\tMin\tMax"
  clearColor
  dat <- io $ getMulValues idx stockVals
  forM_ dat outIndexState
    where
      stockVals = [StockName, PercentChange, Open, DayLow, DayHigh]

outIndexState :: [String] -> Report ()
outIndexState vals
  | length vals < 2 = return ()
  | otherwise = do
    outMainName (read . head $ vals) 2
    outRGString $ read (vals !! 1)
    forM_ (drop 2 vals) ((>>outTab) . outStr)
    newLine

-- -----------------------------------------------------------------------------
stocksProfit :: [((StockSymbol,Int),CashValue)] -> Report ()
stocksProfit [] = return ()
stocksProfit stocks = do
  setForegroundColor Vivid Black
  outStrLn "Name\tAmount\tValue\t\tSpent\t\tProfit"
  clearColor
  dat <- io $ getMulValue (fmap (fst.fst) stocks) Bid
  forM_ (zip3 (fmap fst stocks) (fmap snd stocks) dat) outStockProfit

outStockProfit :: ((StockSymbol,Int),CashValue,String) -> Report ()
outStockProfit ((name,amount), spent, val) = do
  outMainName name 1
  outStr (show amount) >> outTab
  outValue
  outSpent
  outProfit
  newLine
    where
      price = (fromRational . toRational) (read val::Double) :: CashValue
      value = price * fromIntegral amount
      profit = value - spent
      percent = (profit * 100) / spent
      outValue = do
        setRGColor (spent,value)
        outStr (show value) >> outTab
        clearColor
        when (value < 1000) outTab
      outSpent = do
        outStr (show spent) >> outTab
        when (spent < 1000) outTab
      outProfit = do
        setRGColor profit
        outStr $ show profit ++ " (" ++ show percent ++ "%)"
        clearColor

-- -----------------------------------------------------------------------------
showHolds :: [(StockSymbol,Day,CashValue,CashValue)] -> Report ()
showHolds [] = return ()
showHolds xs = do
  setForegroundColor Vivid Black
  outStrLn "Name\tPrice\tChange\tH.Price\tChange\tM.Price\tChange"
  clearColor
  dat <- io $ getMulValues (fmap (\(a,_,_,_)->a) xs) [Bid,PercentChange]
  forM_ (zip xs dat) outStockHold

outStockHold :: ((StockSymbol,Day,CashValue,CashValue),[String]) -> Report ()
outStockHold ((name,day,hPrice,mPrice),vals) = do
  outMainName name 1
  outStr (show price) >> outTab
  outRGString $ read (vals !! 1)
  outStr (show hPrice) >> outTab
  outPerChange hChange
  if mPrice > 0
    then do
      outStr (show mPrice) >> outTab
      outPerChange mChange
    else outTab >> outTab
  outStr $ show day
  newLine
    where
      price = (fromRational . toRational) (read (head vals)::Double) :: CashValue
      hChange = ((price - hPrice)*100)/price
      mChange = ((price - mPrice)*100)/price
      outPerChange val = do
        setRGColor val
        outStr (show val ++ "%") >> outTab
        clearColor

-- -----------------------------------------------------------------------------
