-- zadanie 3

data Request = PutStrLn String | ReadLine
data Response = OK | OKStr String
type Dialog = [Response] -> [Request]

doRequest :: Request -> IO Response
doRequest ReadLine = 
  getLine >>= \s -> return (OKStr ((read s) :: String))
doRequest (PutStrLn i) = 
  putStrLn (show i) >> return OK
            
performIO :: Dialog -> IO ()
performIO dialog = 
  case (dialog undefined) of
    []         -> return ()
    (req:req') -> 
      doRequest req >>= 
        (\res -> performIO 
          (\res' -> tail (dialog (res:res'))))
          
main' :: Dialog
main' resps = ReadLine :
   (case resps of
      OKStr str : resps ->
         if str == ""
            then []
            else (PutStrLn . reverse $ str) :
               (case resps of
                  OK : resps -> main' resps))

main = performIO main'
