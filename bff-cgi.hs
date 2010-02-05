import MyInterpret
import System.Directory
import Network.CGI
import Text.XHtml
import Data.Maybe
import Data.List
import Data.ByteString.Lazy.UTF8 (fromString)

page code pageContent =
       header << (
	thetitle << "Bidirectionalization for Free! -- Demo" +++
	style ! [ thetype "text/css" ] << cdata cssStyle
       ) +++
       body << (
	thediv ! [theclass "top"] << (
		thespan ! [theclass "title"] << "Haskell" +++
		thespan ! [theclass "subtitle"] << "Bidirectionalization for Free!"
	) +++
	maindiv << (
        	p << ("This tool allows you to experiment with the "+++
                      "method described in the paper “" +++
		      hotlink "http://wwwtcs.inf.tu-dresden.de/~voigt/popl09-2.pdf"
                        << "Bidirectionalization for Free!" +++
		      "” (POPL'09) by " +++
		      hotlink "http://wwwtcs.inf.tu-dresden.de/~voigt/"
                        << "Janis Voigtländer" +++
	              "."
		)
			
	) +++
        form ! [method "POST", action "#"] << (
		maindiv << (
			 p << (
				"Enter the Haskell function definitions or load an example. "+++
				"You need to define " +++ tt << "source" +++ " and " +++
				tt << "get" +++ ". The code is evaluated inside a " +++
				tt << "let" +++ " block, so you can define functions by "+++
				"pattern matching, but you cannot define new data types. "+++				      "The type classes required by the bidirectionalization functions are defined for " +++ tt << "Maybe" +++
				", " +++ tt << "[]" +++ ", and this simple tree type:" +++
				pre << "data Tree a = Leaf a | Node (Tree a) (Tree a)" 
			) +++

			p << (
				concatHtml (map (\(name,thisCode) -> 
					radio "load" name
					! (if thisCode == code then [checked] else [])
					+++ name +++ " "
				) examples) +++
				mkSubmit True Load +++
				br +++
				textarea ! [name "code", cols "80", rows "10"] << code
			) 
			
		) +++
 		pageContent
	) +++
        maindiv << (
		p << (
		"The source code of this application and the underlying library can be found " +++
		hotlink "http://hackage.haskell.org/cgi-bin/hackage-scripts/package/bff" << "here"+++
		".") +++
		p << ("© 2008 Joachim Breitner <" +++
                      hotlink "mailto:mail@joachim-breitner.de" << "mail@joachim-breitner.de" +++
		      ">")
		)	
	)
       

cdata s = primHtml ("<![CDATA[\n"++ s ++ "\n]]>")

maindiv = thediv ! [theclass "main"]
        
examples =
	[ ("halve", unlines
		[ "source = [1,2,3,4,5,6,7,8,9,10]"
		, ""
		, "get as = take (length as `div` 2) as"
		])
	, ("flatten", unlines
		[ "source = Node (Leaf 'a') (Leaf 'b')" 
		, ""
		, "get (Leaf a) = [a]"
		, "get (Node t1 t2) = get t1 ++ get t2"
		])
	, ("rmdups", unlines
		[ "source = \"abcbabcbaccba\""
		, ""
		, "get = List.nub"
		])
	, ("top3", unlines
		[ "source = \"transformation\""
		, ""
		, "get = take 3 . List.sort . List.nub"
		])
	, ("tail", unlines
		[ "source = \"abcd\""
		, ""
		, "get = tail"
		])
	, ("sieve", unlines
		[ "source = \"abcdefg\""
		, ""
		, "get (a:b:cs) = b:get cs"
		, "get _        = []"
		])
	, ("doubleList", unlines
		[ "source = \"a\""
		, ""
		, "get = (\\s -> s ++ s)"
		])
	, ("tail/rmdups", unlines
		[ "source = \"abcbabcbaccba\""
		, ""
                , "rmdups = List.nub"
                , ""
		, "get = tail . rmdups"
		])
	]

defaultCode = fromJust (lookup "halve" examples)
	
outputErrors :: String -> Html
outputErrors s = 
           p << (
                strong << "An error occurred:" +++ br +++
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

submitLabel Check = "Re-check types"
submitLabel Load  = "Load example"
submitLabel x   = fromJust (submitCode x)

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
        setHeader "Content-type" "text/xml; charset=UTF-8"
	
	-- the next piece of code is not to be take seious
	todo <- (fromMaybe Check . listToMaybe . catMaybes) `fmap`
                  mapM (\what -> (const what `fmap`) `fmap` getInput (submitId what))
                     [Get, Bff "", Bff "_Eq", Bff "_Ord", Check, Load] 
        
	code <- fromMaybe defaultCode `fmap` getInput "code"

        (newCode, errors) <- case submitCode todo of
	  Just expr -> do
                ret <- liftIO $ catchInterpreterErrors $ simpleInterpret code expr
		return $ case (ret,todo) of 
		   (Left err, Bff _)  ->
			( delDefinition "result" code
			, Just err)
		   (Left err, Get)  ->
			( delDefinition "result" $
			  delDefinition "view" code
			, Just err)
		   (Right dat, Bff suffix)  ->
                     	( addDefiniton "result" dat code
			, Nothing)
		   (Right dat, Get)  ->
                     	( addDefiniton "view"   dat $
			  delDefinition "result" code
			, Nothing)
          Nothing -> case todo of
		   Load -> do loadWhat <- getInput "load"
			      return ( fromMaybe code $ loadWhat >>= flip lookup examples
                                     , Nothing)
		   Check -> return (code, Nothing)

	let hasView = any (defines "view") (lines newCode)
	
	mbType <- liftIO $ catchInterpreterErrors (simpleTypeOf newCode "get")
	mbTypeSrc <- liftIO $ catchInterpreterErrors (simpleTypeOf newCode "source")
        canBff <- liftIO $ either (const False) (const True) `fmap`
			catchInterpreterErrors (simpleTypeOf newCode "bff get source")
        canBffEq <- liftIO $ either (const False) (const True) `fmap`
			catchInterpreterErrors (simpleTypeOf newCode "bff_Eq get source")
        canBffOrd <- liftIO $ either (const False) (const True) `fmap`
			catchInterpreterErrors (simpleTypeOf newCode "bff_Ord get source")

        outputFPS $ fromString $ showHtml $ page newCode $
		p << typeInfo mbType mbTypeSrc canBff canBffEq canBffOrd +++
		maindiv << (
			p << (
				"You can use " +++ tt << "get source" +++ " to calculate "+++
                                "a view which you can then modify. If you have defined "+++
                                tt << "view" +++ ", then you can use the bidirectionalizer "+++
                                "to calculate the updated source. The result will be shown "+++
                                "in the code edit frame above." ) +++
			p << ( "Evaluate " +++
                               mkSubmit True Get +++ " " +++
			       mkSubmit (hasView && canBff) (Bff "") +++" " +++
			       mkSubmit (hasView && canBffEq) (Bff "_Eq") +++" " +++
			       mkSubmit (hasView && canBffOrd) (Bff "_Ord")
			) +++
			maybe noHtml outputErrors errors
		)
		
typeInfo (Left err) _ _ _ _ = maindiv << p << (
	"Your definitions do not typecheck:" +++ br +++
	pre << err +++ br +++
	mkSubmit True Check)

typeInfo _ (Left err) _ _ _ = maindiv << p << (
	"Your definitions do not typecheck:" +++ br +++
	pre << err +++ br +++
	mkSubmit True Check)

typeInfo (Right getType) (Right sourceType) canBff canBffEq canBffOrd = maindiv << (
	p << (
		"Your definitions have the following types: " +++
		pre << ("get :: " ++ getType ++ "\n"++
		        "source :: " ++ sourceType) +++
		"Therefore, an updater can be derived by " +++
		case (canBff, canBffEq, canBffOrd) of
			(True, _, _) -> 
				tt << "bff" +++ ", " +++
				tt << "bff_Eq" +++ ", and " +++
				tt << "bff_Ord" +++ "."
			(False, True, _) -> 
				tt << "bff_Eq" +++ " and " +++
				tt << "bff_Ord" +++ "."
			(False, False, True) -> 
				tt << "bff_Ord" +++ " only."
			(False, False, False) -> 
				"none of the " +++ tt << "bff" +++ " functions."
	) +++
	p << mkSubmit True Check
	)

cssStyle = unlines 
        [ "body { padding:0px; margin: 0px; }"
        , "div.top { margin:0px; padding:10px; margin-bottom:20px;"
        , "              background-color:#efefef;"
        , "              border-bottom:1px solid black; }"
        , "span.title { font-size:xx-large; font-weight:bold; }"
        , "span.subtitle { padding-left:30px; font-size:large; }"
        , "div.main { border:1px dotted black;"
        , "                   padding:10px; margin:10px; }"
        , "div.submain { padding:10px; margin:11px; }"
        , "p.subtitle { font-size:large; font-weight:bold; }"
        , "input.type { font-family:monospace; }"
        , "input[type=\"submit\"] { font-family:monospace; background-color:#efefef; }"
        , "span.mono { font-family:monospace; }"
        , "pre { margin:10px; margin-left:20px; padding:10px;"
        , "          border:1px solid black; }"
        , "textarea { margin:10px; margin-left:20px; padding:10px;  }"
        , "p { text-align:justify; }"
	]

