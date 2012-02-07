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
module PTrader.Util( currentDay, timeStamp ) where

-- -----------------------------------------------------------------------------
import Control.Monad.IO.Class( MonadIO, liftIO )
import Data.Time.Calendar( Day )
import Data.Time.Clock( UTCTime(..), getCurrentTime )
import Data.Time.Format( formatTime )
import System.Locale( defaultTimeLocale )

-- -----------------------------------------------------------------------------
currentDay :: MonadIO m => m Day
currentDay = liftIO $ fmap utctDay getCurrentTime

-- -----------------------------------------------------------------------------
timeStamp :: MonadIO m => m String
timeStamp = liftIO $ fmap showf getCurrentTime
  where
    showf = formatTime defaultTimeLocale "%F %X"

-- -----------------------------------------------------------------------------
