import BffInterpret
import System.Directory
import System.IO


main =  (=<<) (either putStrLn return) $ catchEvalErrors $ do -- <- still readable?
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
	interpretGetter source getter >>= putStrLn

	putStrLn "Please enter modified view"
	view <- getLine

	putStrLn "Running \"bff source get view\""
        interpretSetter source getter view >>= putStrLn
