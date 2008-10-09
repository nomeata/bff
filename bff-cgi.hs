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

defaultCode = unlines
	[ "source = Node (Node (Leaf 1) (Node (Node (Leaf 2) (Leaf 4)) (Leaf 2))) (Leaf 3)" 
	, ""
	, "flatten (Leaf a) = [a]"
	, "flatten (Node t1 t2) = flatten t1 ++ flatten t2"
	, ""
        , "get source = take 2 (sort (flatten source))"
	]
	
outputErrors :: String -> Html
outputErrors s = 
           p << (
                strong << "An error occured:" +++ br +++
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
delDefinition name code = unlines squashed
  where filtered = filter (not . defines name) (lines code)
	squash [] = []
	squash ("":_) = [""]
	squash ("\r":_) = [""]
	squash ls = ls
	squashed = concat $ map squash $ group $ filtered

addDefiniton name def code = unlines (squashed ++ pad ++ new_line)
  where	squashed = lines (delDefinition name code)
	pad | last squashed == "" || last squashed == "\r" = []
            | otherwise                                    = [""]
	new_line = [name ++ " = " ++ def]
	
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

        (newCode, errors) <- case todo of
	  Just what-> do
                ret <- liftIO $ catchInterpreterErrors $
				simpleInterpret code $ submitCode what
		return $ case ret of 
		   Left err   -> (code, Just err)
	           Right dat -> case what of 
                     (Bff suffix) -> (addDefiniton "result" dat code, Nothing)
                     Get  ->         (addDefiniton "view"   dat
					(delDefinition "result" code), Nothing)
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
		maybe noHtml outputErrors errors
		

