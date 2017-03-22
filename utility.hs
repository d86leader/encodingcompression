module Utils
( basicEncode
, basicDecode
) where

import qualified Data.Map as Map
import Control.Monad

type Thesaurus = Map.Map Char String


basicEncode :: Thesaurus -> [Char] -> String
basicEncode dict string = join $ map ((Map.!) dict) string


basicDecode :: Thesaurus -> String -> [Char]
basicDecode dict string = decode_next [] "" string where
	-- traverses map to find matching value. if multiple, finds the first one
	find_key_by_value :: String -> Maybe Char
	find_key_by_value value =
		Map.foldlWithKey return_match Nothing dict where
			return_match Nothing key val =
				if val == value
				then Just key
				else Nothing
			return_match found _ _ = found
	
-- crawls the string "rest" with a window "current", decoding into "done"
	decode_next :: [Char] -> String -> String -> [Char]
	decode_next s "" "" = s
	decode_next s _  "" = error "unexpected end of string. Made out of it: " ++ s
	decode_next done current (x:xs) =
		let cur = current ++ [x]
		    maybe_key = find_key_by_value cur
		in  case maybe_key of
			Just key -> decode_next (done ++ [key]) "" xs
			Nothing  -> decode_next done cur xs
