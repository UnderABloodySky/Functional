import Data.List.Split
-- do
--   text <- getLine
--   if text /= "exit"
--   then do
--     appendFile "output.txt" (text ++ "\n")
--     main
--   else do
--     content <- readFile "output.txt"
--     putStrLn content
--     return ()

main :: IO()

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

-- main = do
--   readLines "A-small-practice.in"
--   return ()

-- main = do
--   x <- readLn
--   return ()

-- .X.O\n
-- ..OO\n
-- ....\n
-- .XTX\n
-- \n
-- XOXX\n
-- XOXX\n
-- TXOO\n
-- OOOX\n

-- parseQuestion :: String -> [Char]
-- parseQuestion x = let [num,base,target] = words $ x
-- 	in (num,base,target)


-- type DeMaze = ([Step],[Step])
--
-- parseMaze :: String -> DeMaze
-- parseMaze s = let
-- 	[go,back] = words s
-- 	parse = map $ read . return
-- 	in (parse go, parse back)
-- "2\n.X.O\n..OO\n....\n.XTX\nXOXX\nXOXX\nTXOO\nOOOX"
data Move = X | O | T | D deriving (Read)

type TTTT = (String,String,String,String)


parseTTTT :: String -> [TTTT]
parseTTTT x = words $ x

parseCases :: String -> [[TTTT]]
parseCases x = let (n:ts) = lines x in take (read n) . map parseTTTT $ ts

-- foldTTTT :: (String -> String -> String -> String -> String) -> TTTT -> String
-- foldTTTT = (\r1 r2 r3 r4 -> r1++r2++r3++r4)

getResults :: [TTTT] -> [String]
getResults ts = map (\(r1,r2,r3,r4) -> r1++r2++r3++r4) ts

-- (getResult t) : (getResults ts)

getResult :: TTTT -> [Char]
getResult (r1,r2,r3,r4) = r1++r2++r3++r4

main = do
  ts <- parseCases `fmap` getContents
  mapM_ putStrLn (getResults ts)
  return ()
  		-- putStr $ "Case #" ++ show i ++ ": "
  		-- putStrLn $ answer t
