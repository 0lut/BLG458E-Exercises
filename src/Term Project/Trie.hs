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
