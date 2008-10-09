{-# LANGUAGE DeriveDataTypeable #-}

import Mueval.Interpreter
import Mueval.Context

import Prelude hiding (catch)
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Error
import System.IO
import System.Posix.Signals
import Data.Typeable

data BffException = BffException String deriving (Typeable)

timeoutIO action = do
        mvar <- newEmptyMVar
        mainId <- myThreadId
        installHandler sigXCPU (CatchOnce $ throwTo mainId $ ErrorCall "Time limit exceeded.") Nothing
        forkIO $ do threadDelay (50 * 500000)
                   -- Time's up. It's a good day to die.
                    throwTo mainId (ErrorCall "Time limit exceeded")
                    yield -- give the other thread a chance
                    putStrLn "Killing main thread..."
                    killThread mainId -- Die now, srsly.
                    error "Time expired"
        
        forkIO $ (action >>= putMVar mvar) `catch`
                 (throwTo mainId)      
        takeMVar mvar


myInterpreter :: String -> IO String
myInterpreter exp = do
        when (unsafe exp) $ throwDyn (BffException "Indicators for unsafe computations found in exp")

        let modules = ("Bff":defaultModules)
        
        timeoutIO $
                interpreterSession False False True (Just modules) ""  exp >> return "TODO"
        

main = flip catchDyn (\(BffException s) -> putStrLn s) $ do
	hSetBuffering stdout NoBuffering
	putStrLn "Bff demo bot"
	putStrLn "Please enter source Data (on one line):"
	putStr "source = "
	source <- getLine
	putStrLn "Please enter get Function"
	putStr "get s = "
	getter <- getLine
	putStrLn "Running \"get source\""
	
	putStrLn =<< (myInterpreter $
	   "let { source = " ++ source ++ " ; " ++
           "get s = " ++ getter ++ " } " ++
           "in get source" )

	putStrLn "Please enter modified view"
	view <- getLine

	putStrLn "Running \"bff source get view\""
        
	putStrLn =<< (myInterpreter $
	   "let { source = " ++ source ++ " ; " ++
           "get s = " ++ getter ++ " ; " ++
           "view = " ++ view ++ " } " ++
           "in bff get source view")

            
