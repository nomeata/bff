{-# LANGUAGE ExistentialQuantification, PolymorphicComponents,
             TemplateHaskell
  #-}

import Data.List
import Text.Printf
import StatsDef

putTable statData = putStr (tabelize stringData)
  where stringData = ["size","manual (put)", "automatic (bff get)"] :
                     map (\(s,m,a) -> [show s, printf "%.3f" m, printf "%.3f" a]) statData

tabelize :: [[String]] -> String
tabelize table = unlines (map padLine table)
  where colWidths = map (maximum . map length) (transpose table)
        pad w s = replicate (w - length s) ' ' ++ s
	padLine ss = intercalate "|" (zipWith pad colWidths ss)

printTest :: String -> StatRunData -> IO ()
printTest name statData = do
	  putStr   $ "Measurements \"" ++ name ++ "\" "
          putStrLn $ ""
	  putTable statData
          putStrLn $ ""

main = do cont <- readFile "stats.data"
	  mapM_ (uncurry printTest) (read cont)
