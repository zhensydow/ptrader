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
import Control.Monad( forM_, when, foldM_ )
import Data.List( transpose )
import qualified Graphics.Rendering.Cairo as Cr

-- -----------------------------------------------------------------------------
border :: Double
border = 0.02

-- -----------------------------------------------------------------------------
data Color = Color !Double !Double !Double

setColor :: Color -> Cr.Render()
setColor (Color r g b) = Cr.setSourceRGB r g b

-- -----------------------------------------------------------------------------
styles :: [Color]
styles = cycle [ Color 1 0 0, Color 0 0.6 0, Color 0 0 1
               , Color 1 0.5 0, Color 1 0 1, Color 0 0.7 1
               , Color 0.1 0.1 0.1 ]

-- -----------------------------------------------------------------------------
render :: Double -> Double -> [String] -> [Double] -> [[Double]] -> Cr.Render ()
render w h names refvals xxs = do
  Cr.setSourceRGB 1 1 1
  Cr.rectangle 0 0 w h
  Cr.fill
  forM_ (zip3 styles refvals rows) $ \(name,val,row) ->
    renderLine w h name val row
  Cr.setLineCap Cr.LineCapSquare
  Cr.setLineWidth 2
  Cr.setSourceRGB 0 0 0
  Cr.moveTo (border*w) (border*w)
  Cr.lineTo (border*w) (h - 2*border*w)
  Cr.lineTo (w - border*w) (h - 2*border*w)
  Cr.lineTo (w - border*w) (border*w)
  Cr.lineTo (border*w) (border*w)
  Cr.stroke
  foldM_ (renderLabel (h - 0.6*border*w)) (border*w) $ zip styles names

    where
      rows = fmap (zip [0..]) $ transpose xxs

-- -----------------------------------------------------------------------------
renderLabel :: Double -> Double -> (Color,String) -> Cr.Render Double
renderLabel y x (col,label) = do
  setColor col
  Cr.moveTo x y
  extents <- Cr.textExtents (' ':' ':label)
  Cr.showText label
  Cr.stroke
  return $! x + Cr.textExtentsXadvance extents

-- -----------------------------------------------------------------------------
renderLine :: Double -> Double -> Color -> Double -> [(Double,Double)]
              -> Cr.Render ()
renderLine _ _ _ _ [] = return ()
renderLine w h col refval (x:xs) = do
  let (miny, maxy) = yLimits $ refval : map snd (x:xs)
      offx = border*w
      maxx = fromIntegral $ max 5 (length xs)
      y:ys = map (transx offx w maxx *** transy offx h miny maxy) (x:xs)
      tval = transy offx h miny maxy refval
      ylast = transx offx w maxx maxx
  setColor col
  uncurry Cr.moveTo y
  mapM_ (uncurry Cr.lineTo) ys
  Cr.setDash [] 0
  Cr.setLineWidth 0.5
  Cr.stroke
  Cr.setLineWidth 0.2
  Cr.setDash [6,4] 0
  Cr.moveTo (fst y) tval
  Cr.lineTo ylast tval
  Cr.stroke
  Cr.setDash [] 0

-- -----------------------------------------------------------------------------
transy :: Double -> Double -> Double -> Double -> Double -> Double
transy lo l miny maxy y = offset - (lcanvas * ((y - miny) / (maxy - miny)))
  where
    offset = l - 2*lo
    lcanvas = l - 3*lo

-- -----------------------------------------------------------------------------
transx :: Double -> Double -> Double -> Double -> Double
transx lo l maxx y = lcanvas * (y / maxx) + lo
  where
    lcanvas = l - 2*lo

-- -----------------------------------------------------------------------------
data GraphConfig = GraphConfig
                   { graphIters :: Maybe Int
                   , graphSleep :: ! Int }
                 deriving( Show )

-- -----------------------------------------------------------------------------
genImage :: FilePath -> Int -> Int -> [String] -> [Double] -> [[Double]] -> IO ()
genImage fn w h xs vals ys = Cr.withImageSurface Cr.FormatARGB32 w h $ \srf -> do
    Cr.renderWith srf (render (fromIntegral w) (fromIntegral h) xs vals ys)
    Cr.surfaceWriteToPNG srf fn

-- -----------------------------------------------------------------------------
runGraph :: GraphConfig -> [String] -> [Double] -> IO [Double] -> IO ()
runGraph c names vals = graphLoop c names vals []

graphLoop :: GraphConfig -> [String] -> [Double] -> [[Double]] -> IO [Double]
             -> IO ()
graphLoop conf names vals xs f = do
  x <- f
  let newxs = xs ++ [x]
  genImage "test.png" 600 300 names vals newxs
  _ <- threadDelay (graphSleep conf * 10^(6 :: Int))
  when notEnded
    (graphLoop newConf names vals newxs f)

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
