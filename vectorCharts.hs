import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Gtk
import Data.Colour
import Data.Colour.Names
import Data.Accessor
import Vectors

showChart = renderableToWindow (chart 1.00) 640 640 >> return undefined

chart lwidth = toRenderable layout
    where
        unitCircle = [(x a, y a) | a <- [0,5..360::Double] ]
            where x a = cos (a * pi / 180)
                  y a = sin (a * pi / 180)

        circleLine = plot_lines_values ^= [unitCircle]
           $ plot_lines_style ^= solidLine lwidth (opaque orange)
           $ defaultPlotLines               

        xAxis = [(-1,0),(1,0)]
        yAxis = [(0,1),(0,-1)]

        axis = plot_lines_values ^= [xAxis,yAxis]
            $ plot_lines_style ^= solidLine lwidth (opaque orange)
            $ defaultPlotLines


        origin = (0,0)
        vectorList = map (\x -> [origin,x])
            vectors
        vectorsPlot = plot_lines_values ^= vectorList
            $ plot_lines_style ^= solidLine lwidth (opaque blue)
            $ defaultPlotLines

        complex = plot_lines_values ^= completeList
            $ plot_lines_style ^= solidLine lwidth (opaque red)
            $ defaultPlotLines


        layout = layout1_title ^= "Dot Plot"
                $ layout1_plots ^= [Left (toPlot circleLine),
                                    Left (toPlot axis),
                                    Left (toPlot vectorsPlot),
                                    Left (toPlot complex)]
                $ defaultLayout1