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
{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
module PTrader.Portfolio(
  CashValue,
  createNewPortfolio, runPortfolio, insertBuyTransaction, insertSellTransaction,
  calcStockAmount
  )where

-- -----------------------------------------------------------------------------
import Control.Monad( when )
import Control.Monad.IO.Class( MonadIO, liftIO )
import Control.Monad.Reader( MonadReader, ReaderT, runReaderT, ask )
import Data.Time.Calendar( Day, showGregorian )
import Data.Fixed( resolution )
import Database.SQLite(
  SQLiteHandle, SQLiteResult, Row, Value(..),
  openConnection, closeConnection, execParamStatement, execParamStatement_ )
import System.Directory( copyFile )
import PTrader.Types( StockSymbol, CashValue )
import Paths_ptrader( getDataFileName )

-- -----------------------------------------------------------------------------
newtype StockID = StockID Int
                deriving( Show )

-- -----------------------------------------------------------------------------
cashToValue :: CashValue -> Value
cashToValue v = Int val
  where
    val = round (v*(fromIntegral $ resolution v))

-- -----------------------------------------------------------------------------
data PortfolioConfig = PortfolioConfig { dbHandle :: SQLiteHandle }

-- -----------------------------------------------------------------------------
newtype Portfolio a = Portfolio
                      { runPF :: ReaderT PortfolioConfig IO a }
                    deriving( Functor, Monad, MonadIO, MonadReader PortfolioConfig )

-- -----------------------------------------------------------------------------
createNewPortfolio :: FilePath -> IO ()
createNewPortfolio filename = do
  old <- getDataFileName "portfolio.db"
  when (old /= filename) $
    copyFile old filename

-- -----------------------------------------------------------------------------
runPortfolio :: Portfolio a -> String -> IO a
runPortfolio portfolio db = do
  handle <- openConnection db
  val <- runReaderT (runPF portfolio) (PortfolioConfig handle)
  closeConnection handle
  return val

-- -----------------------------------------------------------------------------
io :: IO a -> Portfolio a
io = liftIO

-- -----------------------------------------------------------------------------
getDbHandle :: Portfolio SQLiteHandle
getDbHandle = fmap dbHandle ask

-- -----------------------------------------------------------------------------
execPFParamStatement :: SQLiteResult a => String -> [(String, Value)]
                        -> Portfolio (Either String [[Row a]])
execPFParamStatement sql params = do
  db <- getDbHandle
  io $ execParamStatement db sql params

execPFParamStatement_ :: String -> [(String,Value)] -> Portfolio Bool
execPFParamStatement_ sql params = do
  db <- getDbHandle
  res <- io $ execParamStatement_ db sql params
  maybe (return True) (\msg -> io $ putStrLn msg >> return False) res

-- -----------------------------------------------------------------------------
getStockID :: StockSymbol -> Portfolio (Maybe StockID)
getStockID symbol = do
  res <- execPFParamStatement sql [(":param1",Text symbol)]
  case res of
    Right ((((_,Int idx):_):_):_) -> return . Just . StockID . fromIntegral $ idx
    _ -> return Nothing

    where
      sql = "SELECT stockid FROM stock WHERE symbol=:param1"

-- -----------------------------------------------------------------------------
insertBuyTransaction :: Day -> StockSymbol -> Int -> CashValue -> CashValue
                        -> Portfolio Bool
insertBuyTransaction day symbol amount price total = do
  stockRet <- getStockID symbol
  case stockRet of
    Nothing -> return False
    Just (StockID idx) -> do
      execPFParamStatement_ sql [(":stockid", Int $ fromIntegral idx)
                                ,(":date", Text $ showGregorian day)
                                ,(":amount", Int $ fromIntegral amount)
                                ,(":price", cashToValue price)
                                ,(":total", cashToValue total)]
    where
      sql = "INSERT INTO buy VALUES (NULL,:stockid,:date,:amount,:price,:total)"

-- -----------------------------------------------------------------------------
insertSellTransaction :: Day -> StockSymbol -> Int -> CashValue -> CashValue
                         -> Portfolio Bool
insertSellTransaction day symbol amount price total = do
  stockRet <- getStockID symbol
  case stockRet of
    Nothing -> return False
    Just (StockID idx) -> do
      execPFParamStatement_ sql [(":stockid", Int $ fromIntegral idx)
                                ,(":date", Text $ showGregorian day)
                                ,(":amount", Int $ fromIntegral amount)
                                ,(":price", cashToValue price)
                                ,(":total", cashToValue total)]
    where
      sql = "INSERT INTO sell VALUES (NULL,:stockid,:date,:amount,:price,:total)"

-- -----------------------------------------------------------------------------
calcStockAmount :: StockSymbol -> Portfolio Int
calcStockAmount symbol = do
  stockRet <- getStockID symbol
  case stockRet of
    Nothing -> return 0
    Just (StockID idx) -> do
      let params = [(":param1",Int $ fromIntegral idx)]
      resBuy <- execPFParamStatement sqlBuy params
      let buyed = case resBuy of
            Right [[[(_,Int n)]]] -> fromIntegral n
            _ -> 0
      resSell <- execPFParamStatement sqlSell params
      let selled = case resSell of
            Right [[[(_,Int n)]]] -> fromIntegral n
            _ -> 0
      return (buyed - selled)

    where
      sqlBuy = "SELECT SUM(amount) FROM buy WHERE stockid=:param1"
      sqlSell = "SELECT SUM(amount) FROM sell WHERE stockid=:param1"
      
-- -----------------------------------------------------------------------------
