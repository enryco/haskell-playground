import Data.Char
import Data.Maybe()

type Key = String
type Val = String
type KeyVal = (Key, Val)
type KeyValStore = [KeyVal]

test_db = [
  ("a", "ufrtin a" ),
  ("b", "ufrtin b" )]

getValue :: String -> KeyValStore -> Maybe String
getValue _ [] = Nothing 
getValue key ((k,v):kvs) | k == key = Just v
                         | otherwise  = getValue key kvs

setValue :: Key -> Val -> KeyValStore -> KeyValStore
setValue k v kvs = (k,v):kvs

toUpperCase :: String -> String
toUpperCase []     = ""
toUpperCase (x:xs) = (toUpper x):toUpperCase xs


primitivelyParseCommand :: String -> [String]
primitivelyParseCommand ""  = []
primitivelyParseCommand str | head str == ' ' = primitivelyParseCommand $ tail str
                 | otherwise       = takeWhile notSpace str : primitivelyParseCommand (dropWhile notSpace str) where
                    notSpace c = c /= ' '

fixEscapedWhiteSpace :: [String] -> [String]
fixEscapedWhiteSpace []    = []
fixEscapedWhiteSpace [arg] = [arg]
fixEscapedWhiteSpace (arg:args) | last arg == '\\' = (init arg ++ [' '] ++ (head args)):fixEscapedWhiteSpace (tail args)
                                | otherwise        = arg:fixEscapedWhiteSpace args

joinWith :: [String] ->  String -> String
joinWith []   _       = [] 
joinWith [x] _        = x
joinWith (x:xs) delim = x ++ delim ++ joinWith xs delim

cutTheCrust :: String -> String
cutTheCrust = init . tail
 

fixEscapedCharacter :: [String] -> [String]
fixEscapedCharacter []    = []
fixEscapedCharacter [arg] = [arg]
fixEscapedCharacter (arg:args) | isEscaped        = cutTheCrust (escapedArgs `joinWith` " "):fixEscapedCharacter rest
                               | otherwise        = arg:fixEscapedCharacter args where
                                  (isEscaped, escapeChar) = isEscapedWith (head arg) listOfEscapedCharacters where
                                    isEscapedWith c cs = (length (filter (\ci -> ci == c) cs) > 0, c)
                                    listOfEscapedCharacters = ['\"','\'', '`']
                                  (escapedArgs, rest)     = splitAt (getEscapedPosition (arg:args)) (arg:args)
                                  getEscapedPosition args' = length (takeWhile (\arg' -> last arg' /= escapeChar) args') + 1

parseCommand :: String -> [String]
parseCommand = fixEscapedCharacter . fixEscapedWhiteSpace . primitivelyParseCommand


run :: KeyValStore -> IO KeyValStore
run db = do
  putStrLn $ show db
  command <- getLine
  if command == "" then run db else do
    let commands = parseCommand command
    putStrLn $ show commands
    case length commands of
      1 -> singleArgumentExe (head commands) db
      _ -> case (head commands) of
        "set" -> do
          let db' = setValue (commands !! 1) (commands !! 2) db
          putStrLn $ show db'
          run db'
        "get" -> do
          putStrLn $ show $ getValue (commands !! 1) db
          run db
        _ -> run db

    -- let entry = getValue "a" db
    -- let upp = toUpperCase entry
    -- putStrLn $ toUpperCase $ fromMaybe "" entry

singleArgumentExe :: String -> KeyValStore -> IO KeyValStore
singleArgumentExe command db = case command of
      "set" -> do
        putStrLn "Key:"
        key   <- getLine
        putStrLn "Value:"
        value <- getLine
        let db' = setValue key value db
        run db'
      "get" -> do
        putStrLn "Key:"
        key <- getLine
        putStrLn (show (getValue key db))
        run db
      _ -> run db


main :: IO KeyValStore
main = run test_db