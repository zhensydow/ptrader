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
module PTrader.Query( 
  StockValue(..),
  getValue, getValues, getMulValue, getMulValues 
  ) where

-- -----------------------------------------------------------------------------
import Data.List( intercalate )
import Data.List.Split( splitOn )
import Network.Curl( CurlOption(..), curlGetString )


-- -----------------------------------------------------------------------------
data StockValue = StockSymbol
                | StockName
                | Ask 
                | Bid 
                | Change 
                | PercentChange 
                | DayLow 
                | DayHigh 
                | Open
                | PreviousClose
                | PERatio
                | PEGRatio
                | DividendYield
                | DividendShare
                | DividendPayDate
                | DividendExDate
                deriving( Show )
                        
-- -----------------------------------------------------------------------------
-- http://ilmusaham.wordpress.com/tag/stock-yahoo-data/
-- http://finance.yahoo.com/
yahooTag :: StockValue -> String
yahooTag StockSymbol = "s"
yahooTag StockName = "n"
yahooTag Ask = "a"
yahooTag Bid = "b"
yahooTag Change = "c1"
yahooTag PercentChange = "p2"
yahooTag DayLow = "g"
yahooTag DayHigh = "h"
yahooTag PreviousClose = "p"
yahooTag PERatio = "r"
yahooTag PEGRatio = "r5"
yahooTag DividendYield = "y"
yahooTag DividendShare = "d"
yahooTag DividendPayDate = "r1"
yahooTag DividendExDate = "q"

-- -----------------------------------------------------------------------------
yahooUrl = "http://finance.yahoo.com/d/quotes.csv"

-- -----------------------------------------------------------------------------
cleanLine = splitOn "," . filter (/='\r')

-- -----------------------------------------------------------------------------
getValue :: String -> StockValue -> IO String
getValue stock val = do
  (_,buf) <- curlGetString url [CurlFollowLocation True]
  return . head . cleanLine . head . lines $ buf
    where                     
      url = yahooUrl ++ "?s=" ++ stock ++ "&f=" ++ (yahooTag val)

-- -----------------------------------------------------------------------------
getValues :: String -> [StockValue] -> IO [String]
getValues stock vals = do
  (_,buf) <- curlGetString url [CurlFollowLocation True]
  return . cleanLine . head . lines $ buf
    where
      tags = concatMap yahooTag vals
      url = yahooUrl ++ "?s=" ++ stock ++ "&f=" ++ tags
    
-- -----------------------------------------------------------------------------
getMulValue :: [String] -> StockValue -> IO [String]
getMulValue stocks val = do
  (_,buf) <- curlGetString url [CurlFollowLocation True]
  return . map (head . cleanLine) . lines $ buf
    where                     
      names = intercalate "+" stocks
      url = yahooUrl ++ "?s=" ++ names ++ "&f=" ++ (yahooTag val)

-- -----------------------------------------------------------------------------
getMulValues :: [String] -> [StockValue] -> IO [[String]]
getMulValues stocks vals = do
  (_,buf) <- curlGetString url [CurlFollowLocation True]
  return . map cleanLine . lines $ buf
    where
      tags = concatMap yahooTag vals
      names = intercalate "+" stocks
      url = yahooUrl ++ "?s=" ++ names ++ "&f=" ++ tags
    
-- -----------------------------------------------------------------------------
