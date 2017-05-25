module C_Arithm
(
) where

import qualified Data.Map     as M
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import System.IO
import Data.Ratio
import Data.Function
import Data.Tuple

import Utils

------ Model ------
-- We're using static model, so it's all easy

bits_in_int = 32
largest_integer = 2 ** bits_in_int - 1
integer_modulo = 2 ** 32

first_qtr = largest_integer / 4 + 1
half_int  = first_qtr * 2
third_qtr = first_qtr * 3

eof = '\0'

bit_to_text '1' = 1
bit_to_text '0' = 0

-- each symbol is bijected to his cumulative probability and
-- the previous symbols's one
-- note to self: cumulative probability is a sum of probabilities of all symbols
-- before current including current
type Prob_map = M.Map Char (Double, Double)

------ Encoding ------

encode :: Prob_map -> T.Text -> Integer -> T.Text
encode cumul_frequences text del =
    let init_low  = 0
        init_high = largest_integer
    in encode' init_low init_high text
    --
    where
        -- takes symbols one by one, shrinks (and extends back) the interval
        -- and writes the bits
        -- it is assumed that eof is present in text as the last and only last symbol
        encode' _ _ T.empty = T.empty
        encode' p_low p_high text =
            let Just (symbol, rest)        = uncons text
                (cumul_prob, cumul_pos) = cumul_frequences M.! symbol
                range                   = p_high - p_low + 1
                --
                low  = p_low + (range * cumul_pos)  / del
                high = p_low + (range * cumul_prob) / del - 1
                --
                (bits, n_low, n_high) = shrink_write low high 0 T.empty
            in  bits `T.append` encode' n_low n_high rest
        --
        -- instruction on how to shrink the interval and restore it back
        -- (to keep precision), and which bits correspond to what
        shrink_write :: Integer -> Integer -> Integer -> T.Text -> T.Text
            -> (T.Text, Integer, Integer)
        shrink_write p_low p_high bits_to_foll result
         -- zero if in lower half
         | p_high < half_int =
            let bits = write_bits '0' bits_to_foll
                r    = result `T.append` bits
                low  = p_low  * 2
                high = p_high * 2 + 1
            in  shrink_write low high 0 r
         -- 1 if in higher half
         | p_low >= half_int
            let bits = write_bits '1' bits_to_foll
                r    = result `T.append` bits
                low  = (p_low  - half_int) * 2
                high = (p_high - half_int) * 2 + 1
            in  shrink_write low high 0 r
         -- write and opposite bit lare if in the middle
         | p_low >= first_qtr && p_high < third_qtr =
                low  = (p_low  - first_qtr) * 2
                high = (p_high - first_qtr) * 2 + 1
            in  shrink_write low high (bits_to_foll + 1) r
         --
         -- it is said that this algorithm always terminates
         | otherwise = (result, p_low, p_high)
        --
        -- writes a bit and not that bit repeated
        write_bits value following =
            value `T.cons` ( replicate following (bnot value) )
        where
            bnot '1' = '0'
            bnot '0' = '1'


------ Decoding ------

decode :: Prob_map -> T.Text -> Integer -> T.Text
decode cumul_frequences cipher del =
    let init_low    = 0
        init_high   = largest_integer
        -- ehhh. Code is alright, i despise the need in this
        -- puts first bits_in_int bits to integer value
        init_repres =
            T.foldl (\s c -> s * 2 + bit_to_text c) 0
                $ take bits_in_int cipher
    in  decode' init_low init_high init_repres cipher
    where
        decode' _ _ _ T.empty = error "the provided text seems to be incorrect"
        decode' low high repres bits =
            let range  = high - low + 1
                accum  = ((repres - low + 1) * del - 1) / range
                -- first symbol with cumulative frequency greater than accumulated
                symbol = M.foldWithKey (first_gt accum) Nothing cumul_frequences where
                    first_gt :: Integer -> Maybe Char -> Char -> (Double, Double) -> Maybe Char
                    first_gt _ Just c _ _ = Just c
                    first_gt x Nothing c (y, _) =
                        if y > x
                        then Just c
                        else Nothing
                --
                (cumul_prob, cumul_pos) = cumul_frequences M.! symbol
                --
                c_low  = low + (range * cumul_pos)  / del
                c_high = low + (range * cumul_prob) / del - 1
                --
                (n_low, n_high, n_repr, n_bits) = shrink_read c_low c_high repres bits
            in  if symbol == '\0'
                then T.singleton symbol
                else symbol `T.cons` decode' n_low n_high n_repr n_bits
        --
        shrink_read low high x bits 
         | high < half_int =
            let val = (x * 2) `mod` integer_modulo + (bit_to_text . T.head $ bits)
                n_low  = 2 * low
                n_high = 2 * high + 1
            in  shrink_read n_low n_high val (T.tail bits)
         --
         | low >= half_int =
            let val = ((x - half_int) * 2) `mod` integer_modulo + (bit_to_text . T.head $ bits)
                n_low  = 2 * (low - half_int)
                n_high = 2 * (high - half_int) + 1
            in  shrink_read n_low n_high val (T.tail bits)
         --
         | low >= first_qtr && high < third_qtr =
            let val = ((x - first_qtr) * 2) `mod` integer_modulo + (bit_to_text . T.head $ bits)
                n_low  = 2 * (low - first_qtr)
                n_high = 2 * (high - first_qtr) + 1
            in  shrink_read n_low n_high val (T.tail bits)
         | otherwise = (low, high, x, bits)
            



















