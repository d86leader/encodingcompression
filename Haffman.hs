module Haffman
( buildHaffmanCodes
, encodeHaffman
, decodeHaffman
) where

import Data.List
import Data.Function
import Utils
import qualified Data.Map as Map

-- A tree for use in creating haffman code
data Tree a b    = Leaf b | Node a (Tree a b) (Tree a b)
type HaffmanTree = Tree Double (Double, Char)


buildHaffmanCodes ::
	[(Double, Char)] -> [(Char, String)] -- symbol and its probability to symbol and its code

-- passes a sorted input list to a real building function
buildHaffmanCodes list = buildHaffmanCodes' $ sortBy (compare `on` snd) list

buildHaffmanCodes' [] = []
buildHaffmanCodes' [(_, c)] = [(c, "0")]
buildHaffmanCodes' input_list = (unfold_tree . create_tree) input_list where
	all_symbols = map snd input_list -- list of all symbols to build from
	
	-- each tree node is an array of symbols downbranches and their probability sum
	unfold_tree ::
		HaffmanTree -> [(Char, String)]
	create_tree ::
		[(Double, Char)] -> HaffmanTree
	
	-- This funtion builds the foundation and then folds it into a tree
	create_tree = extend_tree . (map Leaf) where
		
		extend_tree [] = Leaf (0.0, 'a') -- TODO: something better goes here, an exception or else
		extend_tree [x] = x
		
		-- folding is done with taking the parts with the least probability
		-- and making a node with them as children
		-- and sorting the list so that the least probable elements are first
		extend_tree (x : y : rest) =
			extend_tree . (sortBy (compare `on` get_prob)) $
				(Node (probability_sum x y) x y) : rest
			where
				get_prob (Leaf (x, _)) = x
				get_prob (Node x _ _)  = x
				
				probability_sum x y = (get_prob x) + (get_prob y)
	
	unfold_tree unfl_tree = map (\x -> (x, extract_symbol unfl_tree x)) all_symbols where
		extract_symbol ::
			HaffmanTree -> Char -> String
		extract_symbol (Leaf (_, c)) x =
			if c == x
			then "!" -- means found
			else ""  -- means pass nothing upwards
		extract_symbol (Node _ left right) x =
			let left_str  = extract_symbol left  x
			    right_str = extract_symbol right x
			in  case (left_str, right_str) of
				-- 0 if found on left, 1 if found on right
				-- added to the string generated downwards
				("!", _) -> "0"
				(_, "!") -> "1"
				("", s)  -> '1' : s
				(s, "")  -> '0' : s


encodeHaffman ::
	[(Char, String)] -> [Char] -> String
encodeHaffman dict input =
	basicEncode (Map.fromList dict) input


decodeHaffman ::
	[(Char, String)] -> String -> [Char]
decodeHaffman dict input =
	basicDecode (Map.fromList dict) input
