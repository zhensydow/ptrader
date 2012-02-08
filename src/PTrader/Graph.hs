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
module PTrader.Graph where

-- -----------------------------------------------------------------------------
import Control.Monad( when )
import qualified Graphics.Rendering.Cairo as Cr
import System.Posix.Unistd( sleep )

-- -----------------------------------------------------------------------------
render = do
  Cr.setSourceRGBA 1 1 1 1
  Cr.rectangle 0 0 600 600
  Cr.fill
  Cr.setSourceRGBA 256 0 0 1
  Cr.moveTo 300 20
  Cr.lineTo 40 0
  Cr.lineTo 0 40
  Cr.stroke
  return ()

test = do
  Cr.withImageSurface Cr.FormatARGB32 300 300 $ \srf -> do
    Cr.renderWith srf render
    Cr.surfaceWriteToPNG srf "test.png"

-- -----------------------------------------------------------------------------
data GraphConfig = GraphConfig 
                   { graphIters :: Maybe Int 
                   , graphSleep :: ! Int }
                 deriving( Show )
                     
-- -----------------------------------------------------------------------------
runGraph :: GraphConfig -> IO [Double] -> IO ()
runGraph c f = graphLoop c [] f

graphLoop :: GraphConfig -> [[Double]] -> IO [Double] -> IO ()
graphLoop conf xs f = do
  x <- f
  let newxs = xs ++ [x]
  print newxs
  sleep (graphSleep conf)
  when notEnded    
    (graphLoop newConf newxs f)
    
    where
      newConf = case graphIters conf of
        Nothing -> conf
        Just n -> conf { graphIters= Just (n-1) }
      notEnded = case graphIters conf of
        Nothing -> True
        Just n -> n > 0
        
-- -----------------------------------------------------------------------------
