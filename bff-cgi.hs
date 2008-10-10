import MyInterpret
import System.Directory
import Network.CGI
import Text.XHtml
import Data.Maybe
import Data.List

page code content =
       header << thetitle << "Bidirectionalization for Free -- Demo" +++
       body << (
        h1 << "Bidirectionalization for Free -- Demo" +++
        p << "TODO: Describe and link stuff here" +++
        form ! [method "POST", action "#"] << (
		p << (
			strong << "Enter the haskell definitions or load an example:" +++ br +++
			concatHtml (map (\(name,thisCode) -> 
				radio "load" name
				! (if thisCode == code then [checked] else [])
				+++ name +++ " "
			) examples) +++ mkSubmit True Load +++ br +++
			
			textarea ! [name "code", cols "80", rows "15"] << code
		) +++ content
	) +++
        p << "TODO: Some Footer"
       )
        
examples =
	[ ("halve", unlines
		[ "source = [1,2,3,4,5,6,7,8,9,10]"
		, ""
		, "get source = take (length source `div` 2) source"
		])
	, ("flatten", unlines
		[ "source = Node (Node (Leaf 1) (Node (Node (Leaf 2) (Leaf 4)) (Leaf 2))) (Leaf 3)" 
		, ""
		, "flatten (Leaf a) = [a]"
		, "flatten (Node t1 t2) = flatten t1 ++ flatten t2"
		, ""
		, "get source = take 3 (nub (flatten source))"
		])
	, ("nodups", unlines
		[ "source = [1,2,3,4,4,3,2,1,0,0,0]"
		, ""
		, "get source = nub source"
		])
	]

defaultCode = fromJust (lookup "flatten" examples)
	
outputErrors :: String -> Html
outputErrors s = 
           p << (
                strong << "An error occured:" +++ br +++
                pre << s
                )
                
mkSubmit active what = submit (submitId what) (submitLabel what)
     	               ! if active then [] else [disabled]

data Run = Get | Check | Load | Bff String 

submitId Get = "get source"
submitId Check = "check"
submitId Load = "load"
submitId (Bff suffix) = "submitBff" ++ suffix

submitCode Get   = Just ("get source")
submitCode Check = Nothing
submitCode Load  = Nothing
submitCode (Bff suffix) = Just ("bff"++suffix++" get source view")

submitLabel Check = "Re-check type of get"
submitLabel Load  = "Load"
submitLabel x   = "Evaluate \""++ fromJust (submitCode x) ++"\""

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
	todo <- (fromMaybe Check . listToMaybe . catMaybes) `fmap`
                  mapM (\what -> (const what `fmap`) `fmap` getInput (submitId what))
                     [Get, Bff "", Bff "_Eq", Bff "_Ord", Check, Load] 
        
	code <- fromMaybe defaultCode `fmap` getInput "code"

        (newCode, errors) <- case submitCode todo of
	  Just expr -> do
                ret <- liftIO $ catchInterpreterErrors $ simpleInterpret code expr
		return $ case ret of 
		   Left err   -> (code, Just err)
	           Right dat -> case todo of 
                     (Bff suffix) -> (addDefiniton "result" dat code, Nothing)
                     Get  ->         (addDefiniton "view"   dat
					(delDefinition "result" code), Nothing)
          Nothing -> case todo of
		   Load -> do loadWhat <- getInput "load"
			      return ( fromMaybe code $ loadWhat >>= flip lookup examples
                                     , Nothing)
		   Check -> return (code, Nothing)

	let hasView = any (defines "view") (lines newCode)
	
	mbType <- liftIO $ catchInterpreterErrors (simpleTypeOf newCode "get")
        canBff <- liftIO $ either (const False) (const True) `fmap`
			catchInterpreterErrors (simpleTypeOf newCode "bff get")
        canBffEq <- liftIO $ either (const False) (const True) `fmap`
			catchInterpreterErrors (simpleTypeOf newCode "bff_Eq get")
        canBffOrd <- liftIO $ either (const False) (const True) `fmap`
			catchInterpreterErrors (simpleTypeOf newCode "bff_Ord get")

        output $ showHtml $ page newCode $
		p << typeInfo mbType canBff canBffEq canBffOrd +++
		p << mkSubmit True Get +++
		p << mkSubmit (hasView && canBff) (Bff "") +++
		p << mkSubmit (hasView && canBffEq) (Bff "_Eq") +++
		p << mkSubmit (hasView && canBffOrd) (Bff "_Ord") +++
		maybe noHtml outputErrors errors
		
typeInfo (Left err) _ _ _ = p << 
	"Your " +++ tt << "get" +++ " function does not typecheck:" +++ br +++
	pre << err +++ br +++
	mkSubmit True Check

typeInfo (Right getType) canBff canBffEq canBffOrd = p << (
	"Your getter has the type: " +++ tt << ("get :: " ++ getType) +++ br +++
	"Therefore, a setter can be derived by " +++
		case (canBff, canBffEq, canBffOrd) of
			(True, _, _) -> 
				tt << "bff" +++ ", " +++
				tt << "bff_Eq" +++ " and " +++
				tt << "bff_Ord" +++ "."
			(False, True, _) -> 
				tt << "bff_Eq" +++ " and " +++
				tt << "bff_Ord" +++ "."
			(False, False, True) -> 
				tt << "bff_Ord" +++ "only ."
			(False, False, False) -> 
				toHtml "none of the bff functions."
		+++ br +++
	mkSubmit True Check
	)
