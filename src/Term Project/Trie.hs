import Data.Maybe
import qualified Data.Map as Map  
import Prelude hiding (Word)
import System.IO
import System.Environment
data Trie = Trie {end::Bool, children:: Map.Map Char Trie} deriving Show
type Word = String


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
                