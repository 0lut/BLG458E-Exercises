import Data.Maybe
import qualified Data.Map as Map  
import Prelude hiding (Word)
import System.IO
import System.Environment
data Trie = Trie {end::Bool, children:: Map.Map Char Trie} deriving Show
type Word = String

add :: Trie -> IO Trie
add tr = do
            putStrLn "Write a word"
            xs <- getLine
            let x = insert' xs tr 
            return x

searchWord :: Trie -> IO Trie
searchWord tr = do
            putStrLn "Enter word/prefix"
            pat <- getLine  
            let res = search pat tr
            case res of 
                True -> putStrLn $ unlines [show pat, "exists in dictionary"]
                False  -> putStrLn "Not exists in dictionary"  
                
            return tr

findPrefix :: Trie -> IO Trie
findPrefix tr = do 
            putStrLn "Enter word/prefix"
            pat <- getLine
            let w = prefix2 pat tr
            case w of 
                Just a -> putStrLn "Found words:" >> (putStrLn $ unlines a)
                Nothing -> putStrLn "No words found with given prefix!"
            return tr 


empty' = Trie False Map.empty


insert' :: Word -> Trie -> Trie
insert' [] (Trie e c) = (Trie True c)
insert' (x:xs) (Trie e c) = case Map.lookup x c of  
    Nothing -> (Trie (e) (Map.insert x (insert' xs empty') c))
    Just a ->  (Trie (e) (Map.insert x (insert' xs a) c))

        
insertList :: [Word] -> Trie -> Trie
insertList xs tr = Prelude.foldr (\x y-> insert' x y) tr xs

search :: String -> Trie -> Bool
search [] tr = end tr
search (x:xs) tr = case Map.lookup x (children tr) of 
                Nothing -> False 
                Just a -> search xs a 

foldrWithKey' f acc mp = foldr (uncurry f) acc (Map.toList mp) 

getWords :: Trie -> [Word]
getWords tr = getWords' [] [] tr
            where 
                getWords' acc1 acc2 (Trie e c) = case e of 
                        True -> acc1:rest
                        False -> rest
                        where 
                            rest = foldrWithKey' (\k a y -> getWords' (acc1++[k]) y a) acc2 c

takepr pr = take (length pr)  
prefix :: String -> Trie -> Maybe [Word]
prefix pr tr = case foldr (\x y -> if (takepr pr x) == pr then x:y else y) [] (getWords tr) of
                [] -> Nothing
                a -> Just a

prefix2 :: String -> Trie -> Maybe [Word]
prefix2 xs tr = case prefix2' xs tr of
                [] -> Nothing
                a -> Just (map (xs++) a)
                where
                    prefix2' [] tr = getWords tr                
                    prefix2' (x:xs) tr = case Map.lookup x (children tr)  of
                                        Just a -> prefix2' xs a
                                        Nothing ->  []
printMenu tr = do 
            putStrLn "Make a choice"
            choice <- getLine
            case head choice of 
                    'a' ->  add tr >>= printMenu 
                    's' -> searchWord tr >>= printMenu
                    'f' -> findPrefix tr >>= printMenu
                    'p' ->  putStrLn "List of words in dictionary" >>  (putStrLn $ unlines $ getWords tr) >> printMenu tr
                    'e' -> return ()
                    _ -> putStrLn "Invalid command!">> printMenu tr

main = do
        args <- getArgs
        contents <- readFile "words.txt"
        let q = lines contents
        let tr = insertList q empty' 
        printMenu tr        
