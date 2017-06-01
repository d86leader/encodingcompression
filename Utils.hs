module Utils
( basicEncode
, basicDecode
, onFile
, onFirstFile
, countProbabilities
, noFileSupplied 
, countProbabilitiesInFile 
, probs_from_text
) where

import qualified Data.Map     as M
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import System.IO
import System.Environment
import Data.Function
import Data.Tuple

import Debug.Hood.Observe

instance Observable T.Text where {observer = observeBase}

type Thesaurus = M.Map Char T.Text


onFile :: (T.Text -> a) -> FilePath -> IO a
onFile func filename = do
	text <- openFile filename ReadMode >>= TIO.hGetContents >>= return . T.strip
	return $ func text

onFirstFile :: (T.Text -> a) -> [FilePath] -> IO a
onFirstFile func [filename] = func `onFile` filename
onFirstFile _ _ = error "Called onFirstFile on an invalid array. Try providing exactly one file name."

--------------------------------------------------------------------------------

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
	--
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


--------------------------------------------------------------------------------


countProbabilities :: T.Text -> M.Map Char Double
countProbabilities text =
	let text_length = T.length text
	    occurences  = M.fromListWith (+) . ( `zip` [1,1..] ) $ T.unpack text
	in  M.map (symbol_and_probability text_length) occurences
	where symbol_and_probability = flip (/) `on` fromIntegral


countProbabilitiesInFile :: [FilePath] -> IO T.Text
countProbabilitiesInFile = onFirstFile (T.pack . show . M.toList . countProbabilities)

--------------------------------------------------------------------------------


probs_from_text :: T.Text -> [(Double, Char)]
probs_from_text = map swap . read . T.unpack


noFileSupplied :: String -> IO T.Text
noFileSupplied usecase = do
	prg_name <- getProgName
	putStrLn ("Usage: " ++ prg_name ++ " " ++ usecase)
	return T.empty
