import MyInterpret
import System.Directory
import System.IO


main =  (=<<) (either putStrLn return) $ catchInterpreterErrors $ do
        setCurrentDirectory "/tmp"
	hSetBuffering stdout NoBuffering
	putStrLn "Bff demo bot"
	putStrLn "Please enter source Data (on one line):"
	putStr "source = "
	source <- getLine
	putStrLn "Please enter get Function"
	putStr "get s = "
	getter <- getLine

	putStrLn "Running \"get source\""
	simpleInterpret ("source = " ++ source ++ "\nget s = " ++ getter ) "get source" >>= putStrLn

	putStrLn "Please enter modified view"
	view <- getLine

	putStrLn "Running \"bff get source view\""
	simpleInterpret ("source = " ++ source ++ "\nget s = " ++ getter  ++ "\nview = " ++ view ) "bff get source view" >>= putStrLn
