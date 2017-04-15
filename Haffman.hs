module Haffman
( buildCodes
, encode
, decode
, encodeFromFiles
, decodeFromFiles
) where

import Data.List
import Data.Function
import System.IO
import Data.Char (isSpace)
import System.Environment
import qualified Data.Map     as M
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO

import Utils

-- A tree for use in creating haffman code
data Tree a b    = Leaf b | Node a (Tree a b) (Tree a b)
type HaffmanTree = Tree Double (Double, Char)


buildCodes ::
	[(Double, Char)] -> [(Char, T.Text)] -- symbol and its probability -> symbol and its code

-- passes a sorted input list to a real building function
buildCodes list = buildCodes' $ sortBy (compare `on` fst) list

buildCodes' [] = []
buildCodes' [(_, c)] = [(c, T.pack "0")]
buildCodes' input_list = (unfold_tree . create_tree) input_list where
	-- each tree node is an array of symbols downbranches and their probability sum
	unfold_tree ::
		HaffmanTree -> [(Char, T.Text)]
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
				(Node (x `probability_sum` y) x y) : rest
			where
				get_prob (Leaf (x, _)) = x
				get_prob (Node x _ _)  = x
				
				probability_sum x y = (get_prob x) + (get_prob y)
	
	unfold_tree (Leaf (_, x)) = [(x, T.empty)]
	unfold_tree (Node _ left_branch right_branch) =
		let left  = unfold_tree left_branch
		    right = unfold_tree right_branch
		in  map (tangle '0') left  ++  map (tangle '1') right 
		where tangle c (sym, str) = (sym, T.cons c str)


--------------------------------------------------------------------------------


encode ::
	[(Char, T.Text)] -> T.Text -> T.Text
encode dict input =
	basicEncode (M.fromList dict) input


decode ::
	[(Char, T.Text)] -> T.Text -> T.Text
decode dict input =
	basicDecode (M.fromList dict) input


probs_from_text :: T.Text -> [(Double, Char)]
probs_from_text = map parse_file_line . T.lines where
	parse_file_line :: T.Text -> (Double, Char)
	parse_file_line line =
		let pr : c : _ = T.words line
		    pr' = read . T.unpack $ pr
		    c'  = T.head c
		in  (pr', c')


--------------------------------------------------------------------------------


decodeFromFiles ( probs_name : working_name : [] ) = do
	probs_text   <- openFile probs_name ReadMode   >>= TIO.hGetContents
	working_text <- openFile working_name ReadMode >>= TIO.hGetContents >>= return . T.strip
	return $ coder ( buildCodes $ probs_from_text probs_text ) working_text

decodeFromFiles (probs_name : []) = do
	probs_text <- openFile probs_name ReadMode >>= TIO.hGetContents
	putStrLn $ show $ buildCodes $ probs_from_text probs_text
	return T.empty

decodeFromFiles _ = do
	prg_name <- getProgName
	putStrLn ("Usage: " ++ prg_name ++ " haffman_d probabilities [text]")
	return T.empty



encodeFromFiles (filename : []) = do
	probabilities <- countProbabilities filename >>= return . M.toList
	working_text  <- openFile working_name ReadMode >>= TIO.hGetContents >>= return . T.strip
	return $ encode ( buildCodes probabilities ) working_text

encodeFromFiles _ = do
	prg_name <- getProgName
	putStrLn ("Usage: " ++ prg_name ++ " haffman_e textfile")
	return T.empty
