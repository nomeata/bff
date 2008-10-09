import MyInterpret
import System.Directory
import Network.CGI
import Text.XHtml
import Data.Maybe
import Data.List

page content =
       header << thetitle << "Bidirectionalization for Free -- Demo" +++
       body << (
        h1 << "Bidirectionalization for Free -- Demo" +++
        p << "TODO: Describe and link stuff here" +++
        form ! [method "POST", action "#"] << content +++
        p << "TODO: Some Footer"
       )
        

queryCode code = 
           p << (
                strong << "Enter the Haskell definitions:" +++ br +++
                textarea ! [name "code", cols "80", rows "15"] << code
           ) 

defaultCode =
	"source = [1,2,3,4,5]\n" ++
	"\n"++
        "get source = take 2 source\n"

outputResult :: Either String String -> Html
outputResult (Left s) = 
           p << (
                strong << "An error occured:" +++ br +++
                pre << s
                )
                
outputResult (Right s) = 
           p << (
                strong << "Output from Bff:" +++ br +++
                pre << s
                )
                
mkSubmit active what =
	p << ( submit (submitId what) ("Evaluate \"" ++ submitCode what ++ "\"") )
	     ! if active then [] else [disabled]

data Run = Get | Bff String 

submitId Get = "get source"
submitId (Bff suffix) = "submitBff" ++ suffix

submitCode Get = "get source"
submitCode (Bff suffix) = "bff"++suffix++" get source view"

main = do setCurrentDirectory "/tmp"
          runCGI (handleErrors cgiMain)

-- This function will not work in all casses, but in most.
addViewDefiniton code view = unlines (squashed ++ pad ++ new_line)
  where filtered = filter (not . defines "view") (lines code)
	squash [] = []
	squash ("":_) = [""]
	squash ("\r":_) = [""]
	squash ls = ls
	squashed = concat $ map squash $ group $ filtered
	pad | last squashed == "" || last squashed == "\r" = []
            | otherwise                                    = [""]
	new_line = ["view = " ++ view]
	
defines "" (' ':_) = True
defines "" ('=':_) = True
defines "" "" = False
defines "" _   = False
defines _  ""  = False
defines (i:is) (x:xs) | i == x = defines is xs
                      | i /= x = False
		   

cgiMain = do
        setHeader "Content-type" "text/xml"
	
	-- the next piece of code is not to be take seious
	todo <- (listToMaybe . catMaybes) `fmap`
                  mapM (\what -> (const what `fmap`) `fmap` getInput (submitId what))
                     [Get,          Bff "",      Bff "_Eq",     Bff "_Ord"]
        
	code    <- fromMaybe defaultCode `fmap` getInput "code"

        (newCode, addOutput) <- case todo of
	  Just what-> do
                ret <- liftIO $ catchInterpreterErrors $
				simpleInterpret code $ submitCode what
		return $ case what of 
		   (Bff suffix) -> (code, Just ret)
                   Get -> case ret of
			  Left err   -> (code, Just (Left err))
			  Right view -> (addViewDefiniton code view, Nothing)
          Nothing -> return (code, Nothing)

	let hasView = any (defines "view") (lines newCode)
	
	mbType <- liftIO $ either (const Nothing) Just `fmap`
			catchInterpreterErrors (simpleTypeOf newCode "get")
	let hasOrd = maybe False ("(Ord " `isPrefixOf`) mbType
	let hasEq = hasOrd || maybe False ("(Eq " `isPrefixOf`) mbType

        output $ showHtml $ page $
		queryCode newCode +++
		mkSubmit True Get +++
		mkSubmit (hasView && not hasEq) (Bff "") +++
		mkSubmit (hasView && not hasOrd) (Bff "_Eq") +++
		mkSubmit (hasView) (Bff "_Ord") +++
		maybe noHtml outputResult addOutput
		

