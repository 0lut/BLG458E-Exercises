
import Data.Maybe
import qualified Data.Map as Map  
import Prelude hiding (Word)

data Trie = Trie {end::Bool, children:: Map.Map Char Trie} deriving Show
type Word = String



add = putStrLn "add"
remove = putStrLn " remove"
empty' = Trie False Map.empty


