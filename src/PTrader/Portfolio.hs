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
  -- * Types
  CashValue, ProfitType,
  -- * Portfolio Monad
  Portfolio, createNewPortfolio, runPortfolio,
  -- * Portfolio Update/Query functions
  insertBuyTransaction, insertSellTransaction, insertProfit,
  calcStockAmount, calcStockNet, ownedStocks
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

data ProfitType = Dividend | CapitalIncrease | Other
                deriving( Show )

instance Enum ProfitType where
  fromEnum Dividend = 0
  fromEnum CapitalIncrease = 1
  fromEnum Other = 2
  toEnum 0 = Dividend
  toEnum 1 = CapitalIncrease
  toEnum 2 = Other
  toEnum _ = error "invalid ProfitType"
                        
-- -----------------------------------------------------------------------------
cashResolution :: Int
cashResolution = fromIntegral $ resolution (undefined :: CashValue)

cashToValue :: CashValue -> Value
cashToValue v = Int val
  where
    val = round (v * fromIntegral cashResolution)

intToCash :: Integral a => a -> CashValue
intToCash v = fromIntegral v / fromIntegral cashResolution

intToStockID :: Integral a => a -> StockID
intToStockID = StockID . fromIntegral

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
runPortfolio :: MonadIO m => Portfolio a -> String -> m a
runPortfolio portfolio db = liftIO $ do
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
    Right ((((_,Int idx):_):_):_) -> return . Just . intToStockID $ idx
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
calcStockAmount symbol = 
  getStockID symbol 
  >>= maybe (return 0) (\idx -> do
                           bought <- calcBoughtStocks idx
                           sold <- calcSoldStocks idx
                           return (bought - sold))

-- -----------------------------------------------------------------------------
calcBoughtStocks :: StockID -> Portfolio Int
calcBoughtStocks (StockID idx) = do
  res <- execPFParamStatement sql [(":par",Int $ fromIntegral idx)]
  case res of
    Right [[[(_,Int n)]]] -> return $ fromIntegral n
    _ -> return 0

    where
      sql  = "SELECT SUM(amount) FROM buy WHERE stockid=:par"

calcSoldStocks :: StockID -> Portfolio Int
calcSoldStocks (StockID idx) = do
  res <- execPFParamStatement sql [(":par",Int $ fromIntegral idx)]
  case res of
    Right [[[(_,Int n)]]] -> return $ fromIntegral n
    _ -> return 0

    where
      sql  = "SELECT SUM(amount) FROM sell WHERE stockid=:par"

-- -----------------------------------------------------------------------------
calcStockNet :: StockSymbol -> Portfolio CashValue
calcStockNet symbol =
  getStockID symbol
  >>= maybe (return 0) (\idx -> do
                           bought <- calcTotalBought idx
                           sold <- calcTotalSold idx
                           return (bought - sold))

-- -----------------------------------------------------------------------------
calcTotalBought :: StockID -> Portfolio CashValue
calcTotalBought (StockID idx) = do
  res <- execPFParamStatement sql [(":par",Int $ fromIntegral idx)]
  case res of
    Right [[[(_,Int n)]]] -> return $ intToCash n
    _ -> return 0

    where
      sql  = "SELECT SUM(total) FROM buy WHERE stockid=:par"

calcTotalSold :: StockID -> Portfolio CashValue
calcTotalSold (StockID idx) = do
  res <- execPFParamStatement sql [(":par",Int $ fromIntegral idx)]
  case res of
    Right [[[(_,Int n)]]] -> return $ intToCash n
    _ -> return 0

    where
      sql  = "SELECT SUM(total) FROM sell WHERE stockid=:par"

-- -----------------------------------------------------------------------------
calcStockProfit :: StockSymbol -> Portfolio CashValue
calcStockProfit symbol =
  getStockID symbol
  >>= maybe (return 0) (\(StockID idx) -> do
                           let param =  [(":par",Int $ fromIntegral idx)]
                           res <- execPFParamStatement sql param
                           case res of
                             Right [[[(_,Int n)]]] -> return $ intToCash n
                             _ -> return 0)
    where
      sql  = "SELECT SUM(total) FROM profit WHERE stockid=:par"

-- -----------------------------------------------------------------------------
ownedStocks :: Portfolio [(StockSymbol, Int)]
ownedStocks = do
  resBuys <- execPFStatement sqlBuys :: Portfolio (Either String [[Row Value]])
  case resBuys of
    Right [rows] -> liftM catMaybes $ forM rows $ \row ->
      case extractData row of
        Just (idx, Int v) -> getStockSymbol idx
                             >>= maybe 
                             (return Nothing)
                             (\name -> do
                                 sell <- calcSoldStocks idx
                                 return . Just $ (name, fromIntegral v - sell))
        _ -> return Nothing
    _ -> return []

    where
      sqlBuys = "SELECT stockid, sum(amount) AS amount FROM buy GROUP BY stockid"
      extractData row = do
        stockid <- case lookup "stockid" row of
          Just (Int x) -> return . intToStockID $ x
          _ -> Nothing
        amount <- lookup "amount" row
        return (stockid, amount)

-- -----------------------------------------------------------------------------
insertProfit :: Day -> StockSymbol -> ProfitType -> CashValue -> Portfolio Bool
insertProfit day symbol ptype total = do
  stockRet <- getStockID symbol
  case stockRet of
    Nothing -> return False
    Just (StockID idx) -> execPFParamStatement_ sql
                          [(":stockid", Int $ fromIntegral idx)
                          ,(":date", Text $ showGregorian day)
                          ,(":type", Int . fromIntegral . fromEnum $ ptype )
                          ,(":total", cashToValue total)]
    where
      sql = "INSERT INTO profit VALUES (NULL,:stockid,:date,:type,:total)"

-- -----------------------------------------------------------------------------
