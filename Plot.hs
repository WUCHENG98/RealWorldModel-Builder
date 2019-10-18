module Plot where
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Cairo
import Data.Default.Class
import Data.Colour
import Data.Colour.Names
import Control.Lens
import System.Environment(getArgs)
import Model

-- vals1 :: [(Double,Double,Double,Double)]
-- vals1 = [ (x,x,x/2,0) | x <- [1..20]]
-- model1:: Double -> Double
-- model1 = (\x -> x*x)


chart val model para= toRenderable layout
  where

    vals::[(Double,Double,Double,Double)]
    vals=[(x,y,0,dy)|([x],([y],dy))<- val]
    

    bars = plot_errbars_values .~ [symErrPoint x y dx dy | (x,y,dx,dy) <- vals]
         $ plot_errbars_title .~"test"
         $ def

    modeline = plot_lines_style . line_color .~ opaque blue
        $ plot_lines_values .~ [ [ (x,(head (((function model) para) [x]))) | (x,y,dx,dy) <- vals] ]
        $ plot_lines_title .~ "price 1"
        $ def

    points = plot_points_style .~ filledCircles 2 (opaque red)
	   $ plot_points_values .~ [(x,y) |  (x,y,dx,dy) <- vals]
           $ plot_points_title .~ "test data"
           $ def

    layout = layout_title .~ "Error Bars"
           $ layout_plots .~ [toPlot bars, toPlot points, toPlot modeline]
           $ def
           
           
draw val model para = renderableToFile def ((name model)++".png") (chart val model para)
