module Arithmetic
( encode
-- , decode
) where

import qualified Data.Map     as M
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import Data.Ratio
import Data.Function


build_interval :: [(Char, Rational)] -> T.Text -> (Rational, Rational)
build_interval chars text =
	build_interval' (make_intervals chars) text
	where
		make_intervals :: [ (Char, Rational) ] -> M.Map Char (Rational, Rational) 
		make_intervals symbols =
			-- this maps current symbol and previous interval to new interval
			-- the first symbol uses (0, 0) interval
			let r = zipWith intervaller symbols $ ('a', (0, 0)) : r
			in  M.fromList r
			where
				intervaller :: (Char, Rational) -> (Char, (Rational, Rational)) -> (Char, (Rational, Rational))
				intervaller (c, freq) (_, (_, prev_end)) = (c, (prev_end, prev_end + freq))
		--
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
				    new_k = 1 / (high - low)
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


encode :: [(Char, Rational)] -> T.Text -> T.Text
encode symbols = dichotomy_encode . build_interval symbols
