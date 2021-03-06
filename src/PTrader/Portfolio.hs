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
  CashValue, ProfitType(..),
  -- * Portfolio Monad
  Portfolio, createNewPortfolio, runPortfolio,
  -- * Portfolio Insert/Update functions
  insertBuyTransaction, insertSellTransaction, insertProfit, insertWatch,
  insertHold, logBuy, logSell, logMarketHold, logPortfolioHold, logStockValue,
  updateHold, insertStockSymbols, updateStocks, deleteHolds,
  -- * Portfolio queries
  calcStockAmount, calcStockNet, ownedStocks, calcStockProfit,
  calcStockPrice, calcStockMarketValue, calcStockGrossProfit,
  calcStockNetProfit, watchedStocks, holds, stockLogs
  )where

-- -----------------------------------------------------------------------------
import Prelude hiding( catch )
import Control.Monad( when, forM, forM_, liftM )
import Control.Monad.IO.Class( MonadIO, liftIO )
import Control.Monad.Reader( MonadReader, ReaderT, runReaderT, ask )
import Data.List.Split( splitOn )
import Data.Maybe( fromJust, catMaybes )
import Data.Time.Calendar( Day, showGregorian, fromGregorian, addDays )
import Data.Fixed( resolution )
import Data.Tuple( swap )
import Database.SQLite(
  SQLiteHandle, SQLiteResult, Row, Value(..),
  openConnection, closeConnection, execStatement,
  execParamStatement, execParamStatement_ )
import System.Directory( copyFile )
import PTrader.Types( StockSymbol, CashValue )
import PTrader.Query( StockValue(..), getValue )
import PTrader.Util( currentDay, readDouble )
import Paths_ptrader( getDataFileName )

-- -----------------------------------------------------------------------------
newtype StockID = StockID Int
                deriving( Show )

data ProfitType = Dividend | CapitalIncrease | Other | StockSell
                deriving( Show, Eq )

instance Enum ProfitType where
  fromEnum = fromJust . flip lookup profitTable
  toEnum = fromJust . flip lookup (map swap profitTable)

profitTable :: [(ProfitType, Int)]
profitTable = [ (Dividend, 0)
              , (CapitalIncrease, 1)
              , (Other, 2)
              , (StockSell, 3)]

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

strToDay :: String -> Day
strToDay str = fromGregorian (read ys) (read ms) (read ds)
  where
    (ys:ms:ds:_) = splitOn "-" str

-- -----------------------------------------------------------------------------
data PortfolioConfig = PortfolioConfig { dbHandle :: SQLiteHandle }

-- -----------------------------------------------------------------------------
newtype Portfolio a = Portfolio
                      { runPF :: ReaderT PortfolioConfig IO a }
                    deriving( Functor, Monad, MonadIO
                            , MonadReader PortfolioConfig )

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
getStockLog :: StockID -> Day -> Portfolio (Maybe CashValue)
getStockLog (StockID idx) day = do
  res <- execPFParamStatement sql [(":idx", Int . fromIntegral $ idx)
                                  ,(":day", Text $ showGregorian day)]
  case res of
    Right ((((_,Int val):_):_):_) -> return . Just . intToCash $ val
    _ -> return Nothing

    where
      sql = "SELECT price FROM log WHERE stockid=:idx AND date=:day"

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
insertWatch :: StockSymbol -> Portfolio Bool
insertWatch symbol = do
  stockRet <- getStockID symbol
  case stockRet of
    Nothing -> return False
    Just (StockID idx) -> execPFParamStatement_ sql
                          [(":stockid", Int $ fromIntegral idx)]
    where
      sql = "INSERT INTO watch VALUES (NULL,:stockid)"

-- -----------------------------------------------------------------------------
insertStockSymbols :: [StockSymbol] -> Portfolio ()
insertStockSymbols xs = forM_ xs $ \symbol -> do
  stockRet <- getStockID symbol
  case stockRet of
    Nothing -> execPFParamStatement_ sql [(":par", Text symbol)]
               >> return ()
    Just _ -> return ()

    where
      sql = "INSERT INTO stock VALUES (NULL,:par,'')"

-- -----------------------------------------------------------------------------
insertHold :: Day -> StockSymbol -> CashValue -> Portfolio Bool
insertHold day symbol price = do
  stockRet <- getStockID symbol
  case stockRet of
    Nothing -> return False
    Just (StockID idx) -> execPFParamStatement_ sql
                          [(":stockid", Int $ fromIntegral idx)
                          ,(":date", Text $ showGregorian day)
                          ,(":price", cashToValue price)]
    where
      sql = "INSERT INTO hold VALUES (NULL,:stockid,:date,:price)"

-- -----------------------------------------------------------------------------
updateStockLog :: Day -> StockSymbol -> CashValue -> Portfolio Bool
updateStockLog day symbol price = do
  stockRet <- getStockID symbol
  case stockRet of
    Nothing -> return False
    Just stock@(StockID idx) -> do
      logVal <- getStockLog stock day
      let params = [(":id", Int $ fromIntegral idx)
                   ,(":date", Text $ showGregorian day)
                   ,(":price", cashToValue price)]

      case logVal of
        Nothing -> execPFParamStatement_ sqlInsert params
        Just _ -> execPFParamStatement_ sqlUpdate params

    where
      sqlInsert = "INSERT INTO log VALUES (:id, :date, :price)"
      sqlUpdate = "UPDATE log SET price=:price WHERE stockid=:id AND date=:date"

-- -----------------------------------------------------------------------------
deleteHolds :: StockSymbol -> Portfolio Bool
deleteHolds symbol = do
  stockRet <- getStockID symbol
  case stockRet of
    Nothing -> return False
    Just (StockID idx) -> execPFParamStatement_ sql
                          [(":stockid", Int $ fromIntegral idx)]
    where
      sql = "DELETE FROM hold WHERE stockid=:stockid"

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
                           n <- calcStockAmount symbol
                           if n <= 0
                             then return 0
                             else do
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
calcStockGrossProfit :: StockSymbol -> Portfolio CashValue
calcStockGrossProfit symbol = do
  net <- calcStockNet symbol
  value <- calcStockMarketValue symbol
  return $! value - net

calcStockNetProfit :: StockSymbol -> Portfolio CashValue
calcStockNetProfit symbol = do
  gross <- calcStockGrossProfit symbol
  profit <- calcStockProfit symbol
  return $! gross + profit

-- -----------------------------------------------------------------------------
calcStockPrice :: StockSymbol -> Portfolio CashValue
calcStockPrice symbol = do
  n <- calcStockAmount symbol
  if n == 0
    then return 0
    else do
      net <- calcStockNet symbol
      return $! net / fromIntegral n

-- -----------------------------------------------------------------------------
calcStockMarketValue :: StockSymbol -> Portfolio CashValue
calcStockMarketValue symbol = do
  n <- calcStockAmount symbol
  if n == 0
    then return 0
    else do
      valStr <- io $ getValue symbol Bid
      let val = (fromRational . toRational) (read valStr :: Double)
      return $! val * fromIntegral n

-- -----------------------------------------------------------------------------
ownedStocks :: Portfolio [(StockSymbol, Int)]
ownedStocks = do
  resBuys <- execPFStatement sqlBuys :: Portfolio (Either String [[Row Value]])
  case resBuys of
    Right [rows] -> liftM (filter ((>0).snd).catMaybes) $ forM rows $ \row ->
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
watchedStocks :: Portfolio [StockSymbol]
watchedStocks = do
  res <- execPFStatement sql :: Portfolio (Either String [[Row Value]])
  case res of
    Right [rows] -> liftM catMaybes $ forM rows $
                    maybe (return Nothing) getStockSymbol . extractData
    _ -> return []

    where
      sql = "SELECT stockid FROM watch"
      extractData row = case lookup "stockid" row of
          Just (Int x) -> return . intToStockID $ x
          _ -> Nothing

-- -----------------------------------------------------------------------------
holds :: Portfolio [(StockSymbol, Day, CashValue)]
holds = do
 res <- execPFStatement sql :: Portfolio (Either String [[Row Value]])
 case res of
   Right [rows] -> liftM catMaybes $ forM rows $ \row ->
     case extractData row of
       Just (idx, Text s, Int v) ->
         getStockSymbol idx
         >>= maybe
         (return Nothing)
         (\name -> return . Just $ (name, strToDay s, intToCash v))
       _ -> return Nothing
   _ -> return []

   where
     sql = "SELECT stockid, date, price FROM hold"
     extractData row = do
       stockid <- case lookup "stockid" row of
         Just (Int x) -> return . intToStockID $ x
         _ -> Nothing
       date <- lookup "date" row
       price <- lookup "price" row
       return (stockid, date, price)

-- -----------------------------------------------------------------------------
stockLogs :: StockSymbol -> Portfolio [(Day, CashValue)]
stockLogs symbol =
  getStockID symbol >>= maybe (return []) obtainData

    where
      sql = "SELECT date, price FROM log WHERE stockid=:par"
      obtainData (StockID idx) = do
        let param =  [(":par",Int $ fromIntegral idx)]
        res <- execPFParamStatement sql param
        case res of
          Right [rows] -> liftM catMaybes $ forM rows $ \row ->
            case extractData row of
              Just (Text s, Int v) -> return . Just
                                     $ (strToDay s, intToCash v)
              _ -> return Nothing
          _ -> return []
      extractData row = do
        date <- lookup "date" row
        price <- lookup "price" row
        return (date, price)

-- -----------------------------------------------------------------------------
logBuy :: StockSymbol -> Int -> CashValue -> CashValue -> Portfolio Bool
logBuy symbol amount price total = do
  day <- currentDay
  insertBuyTransaction day symbol amount price total

-- -----------------------------------------------------------------------------
logSell :: StockSymbol -> Int -> CashValue -> CashValue -> Portfolio Bool
logSell symbol amount price total = do
  n <- calcStockAmount symbol
  if n < amount
    then return False
    else do
      day <- currentDay
      oldPrice <- calcStockPrice symbol
      ok <- insertSellTransaction day symbol amount price total
      if ok
        then let newPrice = (total / fromIntegral amount)
                 profit = (newPrice - oldPrice) * fromIntegral amount
             in insertProfit day symbol StockSell profit
        else return False

-- -----------------------------------------------------------------------------
logMarketHold :: StockSymbol -> Portfolio Bool
logMarketHold symbol = do
  day <- currentDay
  valStr <- io $ getValue symbol Bid
  let val = (fromRational . toRational) (read valStr :: Double)
  insertHold day symbol val

-- -----------------------------------------------------------------------------
logPortfolioHold :: StockSymbol -> Portfolio Bool
logPortfolioHold symbol = do
  day <- currentDay
  val <- calcStockPrice symbol
  insertHold day symbol val

-- -----------------------------------------------------------------------------
logStockValue :: StockSymbol -> Portfolio Bool
logStockValue symbol = do
  day <- fmap (addDays (-1)) currentDay
  valStr <- io $ getValue symbol PreviousClose
  case fmap (fromRational . toRational) (readDouble valStr) of
    Just val -> updateStockLog day symbol val
    Nothing -> return False

-- -----------------------------------------------------------------------------
updateHold :: StockSymbol -> (StockSymbol -> Portfolio Bool) -> Portfolio Bool
updateHold symbol f = do
  ok <- deleteHolds symbol
  if ok then f symbol else return False

-- -----------------------------------------------------------------------------
updateStocks :: Portfolio Bool
updateStocks = do
  resSel <- execPFStatement sqlSel :: Portfolio (Either String [[Row Value]])
  case resSel of
    Right [stocks] -> fmap and $ mapM updateStock stocks
    _ -> return False

    where
     sqlSel = "SELECT symbol FROM stock"

updateStock :: [(String, Value)] -> Portfolio Bool
updateStock [("symbol", Text symbol)] = do
  valStr <- io $ getValue symbol StockName
  execPFParamStatement_ sql [(":sym",Text symbol), (":name",Text $ read valStr)]
    where
      sql = "UPDATE stock SET name=:name WHERE symbol=:sym"

updateStock _ = return False

-- -----------------------------------------------------------------------------
