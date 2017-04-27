module Arithmetic
( encode
, encodeFromFiles
, decode
, decodeFromFiles
) where

import qualified Data.Map     as M
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import System.IO
import Data.Ratio
import Data.Function
import Data.Tuple

import Utils


--map each symbol and its probability to its interval in (0; 1)
make_intervals :: [ (Char, Rational) ] -> M.Map Char (Rational, Rational) 
make_intervals symbols =
	-- this maps current symbol and previous interval to new interval
	-- the first symbol uses (0, 0) interval
	let r = zipWith intervaller symbols $ ('a', (0, 0)) : r
	in  M.fromList r
	where
		intervaller :: (Char, Rational) -> (Char, (Rational, Rational)) -> (Char, (Rational, Rational))
		intervaller (c, freq) (_, (_, prev_end)) = (c, (prev_end, prev_end + freq))


--------------------------------------------------------------------------------


--the main encoding happens here: it builds the interval that represents the text
build_interval :: [(Char, Rational)] -> T.Text -> (Rational, Rational)
build_interval chars text =
	build_interval' (make_intervals chars) text
	where
		build_interval' ::  M.Map Char (Rational, Rational) -> T.Text -> (Rational, Rational)
		build_interval' dict = fst . T.foldl go_deeper init where
			init = ( (0, 1), 1 ) -- initial interval and size koefficient
			--
			-- shirnks current interval to fit current symbol
			go_deeper :: ( (Rational, Rational), Rational ) -> Char -> ( (Rational, Rational), Rational )
			go_deeper ( (left, right), koef ) symbol =
				let (a,b) = dict M.! symbol
				    low   = a * koef
				    high  = b * koef
				    new_k = high - low
				in  ( (left + low, left + high), new_k )


dichotomy_encode :: (Rational, Rational) -> T.Text
dichotomy_encode (left, right) = dichotomy T.empty 0 0.5 where
	--left is included, right unencluded
	dichotomy :: T.Text -> Rational -> Rational -> T.Text
	dichotomy r cur addon
		| cur >= left && cur < right = r
		| cur + addon >= right = 
			dichotomy (T.snoc r '0') (cur) (addon / 2)
		| otherwise =
			dichotomy (T.snoc r '1') (cur + addon) (addon / 2)


--------------------------------------------------------------------------------


dichotomy_decode :: [(Char, Rational)] -> Integer -> Rational -> T.Text
dichotomy_decode f len' representer =
	dichotomy_decode' (0, 1) len'
	where
		freqs = make_intervals f
		-- find element (key, val) of map which the value belongs to
		lookup_interval :: Rational -> Rational -> Rational -> Maybe (Char, (Rational, Rational))
		lookup_interval value koef offset =
			M.foldlWithKey belongs Nothing freqs where
				belongs :: Maybe (Char, (Rational, Rational)) -> Char -> (Rational, Rational) -> Maybe (Char, (Rational, Rational))
				belongs (Just val) _ _ = Just val
				belongs Nothing key (a, b) =
					let left  = a * koef + offset
					    right = b * koef + offset
					in if value >= left && value < right
						then Just (key, (left, right))
						else Nothing
		--
		dichotomy_decode' :: (Rational, Rational) -> Integer -> T.Text
		dichotomy_decode' _ 0 = T.empty
		dichotomy_decode' (left, right) len =
			let koef = right - left
			    x = lookup_interval representer koef left
			in case x of
				Nothing -> error $ "error when calling lookup-interval with " ++ show (left, right) ++ 
					show representer ++ show koef
				Just (symbol, (a, b)) -> 
					T.cons symbol $ dichotomy_decode' (a, b) (len - 1)


text_to_fraction :: T.Text -> Rational
text_to_fraction text = fst $ T.foldl summ (0, 1/2) text where
	summ :: (Rational, Rational) -> Char -> (Rational, Rational)
	summ (s, x) '0' = (s,   x/2)
	summ (s, x) '1' = (s+x, x/2)


--------------------------------------------------------------------------------


encode :: [(Char, Rational)] -> T.Text -> T.Text
encode symbols text =
	let encoded = dichotomy_encode . build_interval symbols $ text
	    len = T.pack $ show (T.length text) ++ " "
	in  len `T.append` encoded


decode :: [(Char, Rational)] -> T.Text -> T.Text
decode symbols cipher =
	let [length', text] = T.words cipher
	    length = read $ T.unpack length'
	    representer = text_to_fraction text
	in  dichotomy_decode symbols length representer


--------------------------------------------------------------------------------


encodeFromFiles :: [FilePath] -> IO T.Text

encodeFromFiles [filename] = do
	probabilities <- map (\(x, y) -> (x, toRational y)) . M.toList <$> countProbabilities `onFile` filename
	working_text  <- openFile filename ReadMode >>= TIO.hGetContents >>= return . T.strip
	return $ encode probabilities working_text
encodeFromFiles _ = noFileSupplied "arithmetic_e textfile"


decodeFromFiles [probs_name, working_name] = do
	probs_text   <- openFile probs_name   ReadMode >>= TIO.hGetContents >>= return . T.strip
	working_text <- openFile working_name ReadMode >>= TIO.hGetContents >>= return . T.strip
	return $ decode (map (\(d,c) -> (c, toRational d)) . probs_from_text $ probs_text) working_text
decodeFromFiles _ = noFileSupplied "arithm_d probabilities textfile"
