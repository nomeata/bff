import MyInterpret
import System.Directory
import Network.CGI
import Text.XHtml
import Data.Maybe

page content =
       header << thetitle << "Bidirectionalization for Free -- Demo" +++
       body << (
        h1 << "Bidirectionalization for Free -- Demo" +++
        p << "TODO: Describe and link stuff here" +++
        form ! [method "POST", action "#"] << content +++
        p << "TODO: Some Footer"
       )
        

queryCode md = 
           p << (
                strong << "Enter the Haskell definitions:" +++ br +++
                textarea ! [name "code", cols "80", rows "15"] <<
			(fromMaybe defaultCode md)
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
                
submit1 =  p << ( submit "submit1" "Calculate \"get source\"" )

submit2 =  p << ( submit "submit2" "Calculate \"bff get source view\"" )
           

main = do setCurrentDirectory "/tmp"
          runCGI (handleErrors cgiMain)

cgiMain = do
        setHeader "Content-type" "text/xml"
        mCode   <- getInput "code"
        mSubmit1<- getInput "submit1"
        mSubmit2<- getInput "submit2"

        content <- case (mCode, mSubmit1, mSubmit2) of

          (Just code, _, Just _) -> do
                -- Use wants Bff to be run
                ret <- liftIO $ catchInterpreterErrors $
				simpleInterpret code "bff get source view"
                return $ queryCode mCode +++
                         submit1 +++
                         submit2 +++
                         outputResult ret

          (Just code, Just _, _) -> do
                -- User wants getter to be run
                ret <- liftIO $ catchInterpreterErrors $
				simpleInterpret code "get source"
                return $ case ret of
                  Left err   -> queryCode mCode +++
                                submit1 +++
                         	submit2 +++
                                outputResult (Left err)
                  Right view -> let newCode = unlines (lines code ++ ["","view = " ++ view])
				in queryCode (Just newCode) +++
                                submit1 +++
                                submit2

          _ -> 
                -- Nothing present, ask for all
                  return $      queryCode mCode +++
                                submit1
        output $ showHtml $ page $ content
