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
module PTrader.Graph( GraphConfig(..), runGraph) where

-- -----------------------------------------------------------------------------
import Control.Arrow( (***) )
import Control.Concurrent( threadDelay )
import Control.Monad( forM_, when )
import Data.List( transpose )
import qualified Graphics.Rendering.Cairo as Cr

-- -----------------------------------------------------------------------------
render :: Double -> Double -> [[Double]] -> Cr.Render ()
render w h xxs = do
  Cr.setSourceRGBA 1 1 1 1
  Cr.rectangle 0 0 w h
  Cr.fill
  Cr.setSourceRGBA 256 0 0 1
  forM_ rows (renderLine w h)
    where
      rows = fmap (zip [0..]) $ transpose xxs

-- -----------------------------------------------------------------------------
renderLine :: Double -> Double -> [(Double,Double)] -> Cr.Render ()
renderLine _ _ [] = return ()
renderLine w h (x:xs) = do 
  let (miny, maxy) = yLimits $ map snd (x:xs)
      maxx = fromIntegral $ max 5 (length xs)
      y:ys = map ((transx w maxx) *** (transy h miny maxy)) $ (x:xs)
  Cr.moveTo (fst y) (snd y)
  mapM_ (uncurry Cr.lineTo) ys
  Cr.stroke

-- -----------------------------------------------------------------------------
transy :: Double -> Double -> Double -> Double -> Double
transy l miny maxy y = l - (l * ((y - miny) / (maxy - miny)))

-- -----------------------------------------------------------------------------
transx :: Double -> Double -> Double -> Double
transx l maxx y = l * (y / maxx)

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
  Cr.withImageSurface Cr.FormatARGB32 600 300 $ \srf -> do
    Cr.renderWith srf (render 600 300 newxs)
    Cr.surfaceWriteToPNG srf "test.png"
  _ <- threadDelay (graphSleep conf * 10^(6 :: Int))
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
yLimits :: [Double] -> (Double, Double)
yLimits xs = if abs (m1 - m2) < 1.0 then (m1, m1+1) else (m1, m2)
  where
    m1 = fromInteger . floor $ minimum xs
    m2 = fromInteger . ceiling $ maximum xs

-- -----------------------------------------------------------------------------
