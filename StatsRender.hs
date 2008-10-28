{-# LANGUAGE ExistentialQuantification, PolymorphicComponents,
             TemplateHaskell
  #-}

import Graphics.Rendering.Chart
import System.IO
import Data.List
import StatsDef

putGraph name statData filename = renderableToPDFFile r 800 400 filename
  where r = toRenderable $
		defaultLayout1
		{ layout1_title = "Test \"" ++ name ++"\""
		, layout1_plots = [
			("Manual Getter", HA_Bottom, VA_Left,
				toPlot $ defaultPlotLines 
					{ plot_lines_values = [manualData]
					, plot_lines_style = solidLine 1 (Color 0 0 1)
					}
			),
			("Automatic Getter", HA_Bottom, VA_Left,
				toPlot $ defaultPlotLines 
					{ plot_lines_values = [automaticData]
					, plot_lines_style = solidLine 1 (Color 1 0 0)
					}
			)
			]
		, layout1_vertical_axes = 
			linkedAxes' (autoScaledAxis (defaultAxis {axis_title =
					"Average time per test in ms"
				}))
		, layout1_horizontal_axes = 
			linkedAxes' (autoScaledAxis (defaultAxis {axis_title =
					"Size of input data structure"
				}))
		}
        (manualData, automaticData) = unzip $ map f statData
	f (s, m, a) = (Point (fromIntegral s) m, Point (fromIntegral s) a)


printTest :: String -> StatRunData -> IO ()
printTest name statData = do
	  putStr   $ "Test \"" ++ name ++ "\" "
	  let graphFileName = name ++ ".pdf"
	  putStrLn $ "Writing graph to " ++ graphFileName
	  putGraph name statData graphFileName


main = do cont <- readFile "stats.data"
	  mapM_ (uncurry printTest) (read cont)
