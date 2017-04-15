module Utils
( basicEncode
, basicDecode
, countProbabilities
) where

import qualified Data.Map  as M
import qualified Data.Text as T
import Control.Monad

type Thesaurus = M.Map Char T.Text


basicEncode :: Thesaurus -> T.Text -> T.Text
basicEncode dict string = T.concatMap ((M.!) dict) string


basicDecode :: Thesaurus -> T.Text -> T.Text
basicDecode dict string = decode_next T.empty T.empty string where
	-- traverses map to find matching value. if multiple, finds the first one
	find_key_by_value :: T.Text -> Maybe Char
	find_key_by_value value =
		M.foldlWithKey return_match Nothing dict where
			return_match Nothing key val =
				if val == value
				then Just key
				else Nothing
			return_match found _ _ = found
	
	-- crawls the string "rest" with a window "current", decoding into "done"
	decode_next :: T.Text -> T.Text -> T.Text -> T.Text
	decode_next done current rest
		| current == T.empty && rest == T.empty  = done
		| rest == T.empty  = 
			error $ "unexpected end of string. Made out of it: " ++ T.unpack done
		| otherwise  =
			let Just (x, xs) = T.uncons rest
			    cur          = T.snoc current x
			    maybe_key    = find_key_by_value cur
			in  case maybe_key of
				Just key -> decode_next (T.snoc done key) T.empty xs
				Nothing  -> decode_next done cur xs


countProbabilities :: FilePath -> IO Thesaurus
countProbabilities filename = do
	text <- openFile filename ReadMode >>= TIO.hGetContents >>= return . T.strip
	let text_length   = T.length text
	let probabilities   = map symbol_and_probability . T.group $ text
	return M.fromList probabilities
	where symbol_and_probability chars =
		let c   = T.head   chars
		    len = T.length chars
		in  (c, len / text_length)
