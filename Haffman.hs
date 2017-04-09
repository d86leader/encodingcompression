module Haffman
( buildCodes
, encode
, decode
, encodeFromFiles
, decodeFromFiles
) where

import Data.List
import Data.Function
import qualified Data.Map as Map
import System.IO
import Data.Char (isSpace)
import System.Environment

import Utils

-- A tree for use in creating haffman code
data Tree a b    = Leaf b | Node a (Tree a b) (Tree a b)
type HaffmanTree = Tree Double (Double, Char)


buildCodes ::
	[(Double, Char)] -> [(Char, String)] -- symbol and its probability -> symbol and its code

-- passes a sorted input list to a real building function
buildCodes list = buildCodes' $ sortBy (compare `on` snd) list

buildCodes' [] = []
buildCodes' [(_, c)] = [(c, "0")]
buildCodes' input_list = (unfold_tree . create_tree) input_list where
	all_symbols = map snd input_list -- list of all symbols to build from
	
	-- each tree node is an array of symbols downbranches and their probability sum
	unfold_tree ::
		HaffmanTree -> [(Char, String)]
	create_tree ::
		[(Double, Char)] -> HaffmanTree
	
	-- This funtion builds the foundation and then folds it into a tree
	create_tree = extend_tree . (map Leaf) where
		
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
	
	unfold_tree (Leaf (_, x)) = [(x, "")]
	unfold_tree (Node _ left_branch right_branch) =
		let left  = unfold_tree left_branch
		    right = unfold_tree right_branch
		in  map (tangle '0') left  ++  map (tangle '1') right 
		where tangle c (sym, str) = (sym, c : str)


encode ::
	[(Char, String)] -> [Char] -> String
encode dict input =
	basicEncode (Map.fromList dict) input


decode ::
	[(Char, String)] -> String -> [Char]
decode dict input =
	basicDecode (Map.fromList dict) input


probs_from_text :: String -> [(Double, Char)]
probs_from_text = map parse_file_line . lines where
	parse_file_line :: String -> (Double, Char)
	parse_file_line line =
		let pr : c : _ = words line
		in  (read pr, head c)

strip_end :: String -> String
strip_end = reverse . dropWhile isSpace . reverse


encodeFromFiles ::
	[FilePath] -> IO String

encodeFromFiles (probs_name : working_name : []) = do
	probs_text   <- openFile probs_name ReadMode   >>= hGetContents
	working_text <- openFile working_name ReadMode >>= hGetContents >>= return . strip_end
	return $ encode ( buildCodes $ probs_from_text probs_text ) working_text

encodeFromFiles (probs_name : []) = do
	probs_text <- openFile probs_name ReadMode >>= hGetContents
	putStrLn $ show $ buildCodes $ probs_from_text probs_text
	return ""

encodeFromFiles _ = do
	prg_name <- getProgName
	putStrLn ("Usage: " ++ prg_name ++ " haffman_e probabilities [text]")
	return ""


decodeFromFiles ::
	[FilePath] -> IO String
decodeFromFiles (probs_name : working_name : []) = do
	probs_text   <- openFile probs_name ReadMode   >>= hGetContents
	working_text <- openFile working_name ReadMode >>= hGetContents >>= return . strip_end
	return $ decode ( buildCodes $ probs_from_text probs_text ) working_text
decodeFromFiles _ = do
	prg_name <- getProgName
	putStrLn ("Usage: " ++ prg_name ++ " haffman_e probabilities text")
	return ""
