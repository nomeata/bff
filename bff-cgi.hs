import BffInterpret
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
        

querySource md = 
           p << (
                strong << "Enter the source Data:" +++ br +++
                tt << "source = " +++ br +++
                textarea ! [name "source"] << (fromMaybe "[1,2,3,4,5]" md)
           ) 

queryGetter md =
           p << (
                strong << "Enter the getter function: " +++ br +++
                tt << "get s = " +++  br +++
                textarea ! [name "getter"] << (fromMaybe "take 2 s" md)
           ) 
                
queryView md =
           p << (
                strong << "Please update the View: " +++ br +++
                tt << "view = " +++ br +++
                textarea ! [name "view"] << (fromMaybe "1" md)
           ) 

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
                
submit1 =  p << ( submit "submit1" "Apply getter" )

submit2 =  p << ( submit "submit2" "Run Bff" )
           

main = do setCurrentDirectory "/tmp"
          runCGI (handleErrors cgiMain)

cgiMain = do
        setHeader "Content-type" "text/xml"
        mSource <- getInput "source"
        mGetter <- getInput "getter"
        mView   <- getInput "view"
        mSubmit1<- getInput "submit1"
        mSubmit2<- getInput "submit2"

        content <- case (mSource, mGetter, mView, mSubmit1, mSubmit2) of
          (Just source,Just getter,Just view, _, Just _) -> do
                -- All input present, run Bff
                ret <- liftIO $ catchEvalErrors $ interpretSetter source getter view
                return $ querySource mSource +++
                         queryGetter mGetter +++
                         submit1 +++
                         queryView   mView +++
                         submit2 +++
                         outputResult ret
          (Just source,Just getter, _ , Just _, _) -> do
                -- Source and Getter present, calculate view
                ret <- liftIO $ catchEvalErrors $ interpretGetter source getter
                return $ case ret of
                  Left err   -> querySource mSource +++
                                queryGetter mGetter +++
                                submit1 +++
                                outputResult (Left err)
                  Right view -> querySource mSource +++
                                queryGetter mGetter +++
                                submit1 +++
                                queryView   (Just view) +++
                                submit2
          _ -> 
                -- Nothing present, ask for all
                  return $      querySource mSource +++
                                queryGetter mGetter +++
                                submit1
        output $ showHtml $ page $ content
