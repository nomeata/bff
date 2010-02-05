{-# LANGUAGE ExistentialQuantification, PolymorphicComponents,
             TemplateHaskell
  #-}

import Graphics.Rendering.Chart
import Data.Accessor.Basic
import Data.Colour

import System.IO
import Data.List
import StatsDef


putGraph name statData filename = renderableToPDFFile r 600 400 filename
  where r = toRenderable $
  		compose [
		   set layout1_title $ "Measurements \"" ++ name ++"\""
		 , set layout1_plots [ 
		 	Left $ toPlot $ compose [
				  set plot_lines_title "manual (put)"
				, set plot_lines_style $ dashedLine 1 [4,4] (opaque black)
				, set plot_lines_values [manualData]
			] defaultPlotLines ,
		 	Left $ toPlot $ compose [
				  set plot_lines_title "automatic (bff get)"
				, set plot_lines_style $ solidLine 1 (opaque black)
				, set plot_lines_values [automaticData]
			] defaultPlotLines 
			]
		, set layout1_left_axis $ compose [
			set laxis_title "Average time per run in ms",
			set laxis_generate (autoScaledAxis defaultLinearAxis)
			] defaultLayoutAxis
		, set layout1_bottom_axis $ compose [
			set laxis_title "Size of original source",
			set laxis_generate (autoScaledAxis defaultLinearAxis)
			] defaultLayoutAxis
		] defaultLayout1
        (manualData, automaticData) = unzip $ map f statData
	f (s, m, a) = ((fromIntegral s, m), (fromIntegral s, a))

printTest :: String -> StatRunData -> IO ()
printTest name statData = do
	  putStr   $ "Measurements \"" ++ name ++ "\" "
	  let graphFileName = name ++ ".pdf"
	  putStrLn $ "Writing graph to " ++ graphFileName
	  putGraph name statData graphFileName

main = do cont <- readFile "stats.data"
	  mapM_ (uncurry printTest) (read cont)

