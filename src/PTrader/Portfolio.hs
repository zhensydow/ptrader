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
module PTrader.Portfolio(
  CashValue,
  createNewPortfolio, runPortfolio, insertBuyTransaction, insertSellTransaction,
  calcStockAmount, ownedStocks
  )where

-- -----------------------------------------------------------------------------
import Control.Monad( when, forM, liftM )
import Control.Monad.IO.Class( MonadIO, liftIO )
import Control.Monad.Reader( MonadReader, ReaderT, runReaderT, ask )
import Data.Maybe( catMaybes )
import Data.Time.Calendar( Day, showGregorian )
import Data.Fixed( resolution )
import Database.SQLite(
  SQLiteHandle, SQLiteResult, Row, Value(..),
  openConnection, closeConnection, execStatement,
  execParamStatement, execParamStatement_ )
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
execPFStatement :: SQLiteResult a => String
                   -> Portfolio (Either String [[Row a]])
execPFStatement sql = getDbHandle >>= \db -> io $ execStatement db sql

execPFParamStatement :: SQLiteResult a => String -> [(String, Value)]
                        -> Portfolio (Either String [[Row a]])
execPFParamStatement sql ps = do
  db <- getDbHandle
  io $ execParamStatement db sql ps

execPFParamStatement_ :: String -> [(String,Value)] -> Portfolio Bool
execPFParamStatement_ sql ps = do
  db <- getDbHandle
  res <- io $ execParamStatement_ db sql ps
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

getStockSymbol :: StockID -> Portfolio (Maybe StockSymbol)
getStockSymbol (StockID idx) = do
  res <- execPFParamStatement sql [(":par", Int . fromIntegral $ idx)]
  case res of
    Right ((((_,Text sym):_):_):_) -> return . Just $ sym
    _ -> return Nothing

    where
      sql = "SELECT symbol FROM stock WHERE stockid=:par"

-- -----------------------------------------------------------------------------
insertBuyTransaction :: Day -> StockSymbol -> Int -> CashValue -> CashValue
                        -> Portfolio Bool
insertBuyTransaction day symbol amount price total = do
  stockRet <- getStockID symbol
  case stockRet of
    Nothing -> return False
    Just (StockID idx) -> execPFParamStatement_ sql
                          [(":stockid", Int $ fromIntegral idx)
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
    Just (StockID idx) -> execPFParamStatement_ sql
                          [(":stockid", Int $ fromIntegral idx)
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
    Just idx -> do
      buyed <- calcBuyedStocks idx
      selled <- calcSelledStocks idx
      return (buyed - selled)

-- -----------------------------------------------------------------------------
calcBuyedStocks :: StockID -> Portfolio Int
calcBuyedStocks (StockID idx) = do
  res <- execPFParamStatement sql [(":par",Int $ fromIntegral idx)]
  case res of
    Right [[[(_,Int n)]]] -> return $ fromIntegral n
    _ -> return 0

    where
      sql  = "SELECT SUM(amount) FROM buy WHERE stockid=:par"

calcSelledStocks :: StockID -> Portfolio Int
calcSelledStocks (StockID idx) = do
  res <- execPFParamStatement sql [(":par",Int $ fromIntegral idx)]
  case res of
    Right [[[(_,Int n)]]] -> return $ fromIntegral n
    _ -> return 0

    where
      sql  = "SELECT SUM(amount) FROM sell WHERE stockid=:par"

-- -----------------------------------------------------------------------------
ownedStocks :: Portfolio [(StockSymbol, Int)]
ownedStocks = do
  resBuys <- execPFStatement sqlBuys :: Portfolio (Either String [[Row Value]])
  case resBuys of
    Right [xs] -> do
      liftM catMaybes $ forM xs $ \x -> do
        case extractData x of
          Just (idx, Int v) -> do
            sym <- getStockSymbol idx
            case sym of
              Just name -> do
                selled <- calcSelledStocks idx
                return . Just $ (name, (fromIntegral v) - selled)
              _ -> return Nothing
          _ -> return Nothing
    _ -> return []

    where
      sqlBuys = "SELECT stockid, sum(amount) AS amount FROM buy GROUP BY stockid"
      extractData xs = do
        stockid <- case lookup "stockid" xs of
          Just (Int x) -> return . StockID . fromIntegral $ x
          _ -> Nothing
        amount <- lookup "amount" xs
        return (stockid, amount)

-- -----------------------------------------------------------------------------
