module Arithmetic
( encodeArithmetic 
, decodeArithmetic
) where

import Data.Ratio

encodeArithmetic ::
	[(Char, Rational)] -> String -> Rational

decodeArithmetic ::
	[(Char, Rational)] -> Rational -> String


encodeArithmetic chars text =
	encodeArithmetic' (make_interval chars) text
	where
		make_interval :: [(Char, Rational)] -> [(Char, (Rational, Rational))]
		make_interval symbols =
			let r = zipWith intervaller symbols $ ('a', (0, 0)) : r in r
			where
				intervaller :: (Char, Rational) -> (Char, (Rational, Rational)) -> (Char, (Rational, Rational))
				intervaller (c, freq) (_, (_, prev_end)) = (c, (prev_end, prev_end + freq))
	
	encodeArithmetic' 
