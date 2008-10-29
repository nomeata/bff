{-# LANGUAGE ExistentialQuantification, PolymorphicComponents,
             TemplateHaskell
  #-}

import Graphics.Rendering.Chart
import System.IO
import Data.List
import StatsDef


putGraph name statData filename = renderableToPDFFile r 600 400 filename
  where r = toRenderable $
		defaultLayout1
		{ layout1_title = "Measurements \"" ++ name ++"\""
		, layout1_plots = [
			("manual (put)", HA_Bottom, VA_Left,
				toPlot $ defaultPlotLines 
					{ plot_lines_values = [manualData]
					, plot_lines_style = solidLine 1 (Color 0 0 0)
					}
			),
			("automatic (bff get)", HA_Bottom, VA_Left,
				toPlot $ defaultPlotLines 
					{ plot_lines_values = [automaticData]
					, plot_lines_style = dashedLine 1 [10,10] (Color 0 0 0)
					}
			)
			]
		, layout1_vertical_axes = 
			linkedAxes' (autoScaledAxis (defaultAxis {axis_title =
					"Average time per run in ms"
				}))
		, layout1_horizontal_axes = 
			linkedAxes' (autoScaledAxis (defaultAxis {axis_title =
					"Size of original source"
				}))
		}
        (manualData, automaticData) = unzip $ map f statData
	f (s, m, a) = (Point (fromIntegral s) m, Point (fromIntegral s) a)

printTest :: String -> StatRunData -> IO ()
printTest name statData = do
	  putStr   $ "Measurements \"" ++ name ++ "\" "
	  let graphFileName = name ++ ".pdf"
	  putStrLn $ "Writing graph to " ++ graphFileName
	  putGraph name statData graphFileName

main = do cont <- readFile "stats.data"
	  mapM_ (uncurry printTest) (read cont)



