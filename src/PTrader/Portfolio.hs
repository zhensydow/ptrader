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
  createNewPortfolio, runPortfolio, insertBuyTransaction
  )where

-- -----------------------------------------------------------------------------
import Control.Monad( when )
import Control.Monad.IO.Class( MonadIO, liftIO )
import Control.Monad.Reader( MonadReader, ReaderT, runReaderT, ask )
import Data.Time.Calendar( Day )
import Database.SQLite( 
  SQLiteHandle, Value(..), openConnection, closeConnection,
  execParamStatement )
import System.Directory( copyFile )
import PTrader.Types( StockSymbol, CashValue )
import Paths_ptrader( getDataFileName )

-- -----------------------------------------------------------------------------
newtype StockID = StockID Int
                deriving( Show )

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
getStockID :: StockSymbol -> Portfolio (Maybe StockID)
getStockID symbol = do
  db <- getDbHandle
  res <- io $ execParamStatement db sql [(":param1",Text symbol)]
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
      return True

-- -----------------------------------------------------------------------------
