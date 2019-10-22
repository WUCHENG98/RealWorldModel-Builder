module Plot where
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Cairo
import Data.Default.Class
import Data.Colour
import Data.Colour.Names
import Control.Lens
import System.Environment(getArgs)
import Model
import Control.Monad(void)
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Grid
import Graphics.Rendering.Chart.Backend.Cairo(renderableToFile)
import System.Locale (defaultTimeLocale)
import Data.Time

-- Use the haskell library "haskell-chart" decoumtation: https://github.com/timbod7/haskell-chart/wiki

-- Create the residual diagram
chartresidual :: [([Double], ([Double], Double))] -> Model -> [Double] -> Renderable ()
chartresidual val model para= toRenderable layout
  where
    vals::[(Double,Double,Double,Double)]
    vals=[(x,y,0,dy)|([x],([y],dy))<- val]
    residual = [(x,((head (((function model) para) [x]))-y),0,dy)|([x],([y],dy))<- val]
    
    bars = plot_errbars_values .~ [symErrPoint x y dx dy | (x,y,dx,dy) <- residual]
         $ plot_errbars_title .~"Error"
         $ def

    points = plot_points_style .~ filledCircles 2 (opaque red)
	      $ plot_points_values .~ [(x,y) |  (x,y,dx,dy) <- residual]
        $ plot_points_title .~ "Residual"
        $ def
    zeroline = plot_lines_style .line_color .~ opaque red
            $ plot_lines_values .~[[(x,0)|(x,y,dx,dy) <- vals]]
            $ plot_lines_title .~ "Zero Line"
            $def

    layout = layout_title .~ "Residual"
           $ layout_plots .~ [toPlot bars, toPlot points, toPlot zeroline]
           $ def


--Create the data-model diagram 
chart :: [([Double], ([Double], Double))] -> Model -> [Double] -> Renderable ()
chart val model para= toRenderable layout
  where
    vals::[(Double,Double,Double,Double)]
    vals=[(x,y,0,dy)|([x],([y],dy))<- val]
    
    bars = plot_errbars_values .~ [ErrPoint (ErrValue x x x) (ErrValue (y-dy) y (y+dy)) | (x,y,dx,dy) <- vals]
         $ plot_errbars_title .~"Error"
         $ def

    modeline = plot_lines_style . line_color .~ opaque blue
        $ plot_lines_values .~ [ [ (x,(head (((function model) para) [x]))) | (x,y,dx,dy) <- vals] ]
        $ plot_lines_title .~ (name model)
        $ def

    points = plot_points_style .~ filledCircles 2 (opaque red)
	   $ plot_points_values .~ [(x,y) |  (x,y,dx,dy) <- vals]
           $ plot_points_title .~ "Raw Data"
           $ def

    layout = layout_title .~ (name model)++" Fitting Result"
           $ layout_plots .~ [toPlot bars, toPlot points, toPlot modeline]
           $ def


draw :: [([Double], ([Double], Double))] -> Model -> [Double] -> IO (PickFn ())
draw val model para = do
  zt <- getZonedTime
  renderableToFile def ((name model)++ (show zt) ++".png") (chart val model para)
  renderableToFile def ((name model)++ (show zt) ++"_residual.png") (chartresidual val model para)


-- test = do 
--   zt <-getZonedTime
--   print ((timeZoneOffsetString zt)++"111")


-- currentTime = fmap show getCurrentTime
-- zonedTime = fmap show getZonedTime
-- main = do 
--         (createDirectory "/Users/yunan.xu/Desktop/pj111")
--         zt <- getZonedTime
--         test
--         print zt