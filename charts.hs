import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Gtk
import Data.Colour
import Data.Colour.Names
import Data.Accessor
import System.Environment(getArgs)

chart lwidth = toRenderable layout
  where
    circle x = [ (r a * sin (a*dr),r a * cos (a*dr)) | a <- [0,30..360::Double] ]
      where
        dr = 2 * pi / 360
        r a = x * cos (a * 20 * pi /360)

    regCircle = [(x a, y a) | a <- [0,5..360::Double] ]
        where x a = cos (a * pi / 180)
              y a = sin (a * pi / 180)

    circleP = plot_lines_values ^= [circle 1]
            $ plot_lines_style ^= solidLine lwidth (opaque green)  
            $ defaultPlotLines

    circleD = plot_lines_values ^= [circle 0.8]
            $ plot_lines_style ^= solidLine lwidth (opaque blue)  
            $ defaultPlotLines

    points = plot_points_style ^= filledCircles 1 (opaque red)
           $ plot_points_values ^= circle 1
           $ plot_points_title ^= "test data"
           $ defaultPlotPoints

    circlePlot = plot_points_style ^= filledCircles 2 (opaque green)
           $ plot_points_values ^= regCircle
           $ plot_points_title ^= "dem points"
           $ defaultPlotPoints

    circleLine = plot_lines_values ^= [regCircle]
           $ plot_lines_style ^= solidLine lwidth (opaque orange)
           $ defaultPlotLines

    layout = layout1_title ^= "Parametric Plot"
           $ layout1_plots ^= [Left (toPlot circleP),
                               Left (toPlot circleD),
                               Left (toPlot points),
                               Left (toPlot circlePlot),
                               Left (toPlot circleLine)]
           $ defaultLayout1

main1 :: [String] -> IO(PickFn ())
main1 ["small"]  = renderableToPNGFile (chart 1.00) 320 240 "test_parametric_small.png"
main1 ["big"]    = renderableToPNGFile (chart 1.00) 800 600 "test_parametric_big.png"
main1 _          = renderableToWindow  (chart 1.00) 640 480 >> return undefined

main = getArgs >>= main1 >> return ()