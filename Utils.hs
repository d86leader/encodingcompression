module Utils
( basicEncode
, basicDecode
, countProbabilities
, noFileSupplied 
) where

import qualified Data.Map     as M
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import System.IO
import Control.Monad
import System.Environment
import Data.Function

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


countProbabilities :: FilePath -> IO (M.Map Double Char)
countProbabilities filename = do
	text <- openFile filename ReadMode >>= TIO.hGetContents >>= return . T.strip
	let text_length   = T.length text
	let grouped       = T.group text
	let probabilities = map (symbol_and_probability text_length) grouped
	return $ M.fromList probabilities
	where
		symbol_and_probability total_length chars =
			let c   = T.head chars
			    len = fromIntegral . T.length $ chars
			    frac_length = fromIntegral total_length
			in  (len / frac_length, c)


--------------------------------------------------------------------------------


noFileSupplied :: String -> IO T.Text
noFileSupplied usecase = do
	prg_name <- getProgName
	putStrLn ("Usage: " ++ prg_name ++ " " ++ usecase)
	return T.empty
