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
import PTrader.Report(
  Report, runReport, newScreen, newLine, stocksState, indexState )
import Control.Monad( forever )
import System.Exit( exitSuccess )
import System.Posix.Unistd( sleep )
import System.Posix.Signals( Handler(..), installHandler, sigINT )

-- -----------------------------------------------------------------------------
stocks :: [String]
stocks = ["ENG.MC","SAN.MC","TEF.MC","OHL.MC","FER.MC"]

indexes :: [String]
indexes = ["^IBEX"]

myReport :: Report ()
myReport = do
  -- clear console screen
  newScreen
  -- print the state of indexes
  indexState indexes
  newLine
  -- print the state of selected stocks
  stocksState stocks

-- -----------------------------------------------------------------------------
main :: IO ()
main = do
  _ <- installHandler sigINT (CatchOnce exitSuccess) Nothing
  forever $ do
    -- print myReport using colors
    runReport myReport True
    -- wait 1 minute
    sleep 60

-- -----------------------------------------------------------------------------
