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

first_qtr = largest_integer / 4 + 1
half_int  = first_qtr * 2
third_qtr = first_qtr * 3

eof = '\0'

------ Encoding ------

encode :: M.Map Char Double -> T.Text -> Integer -> T.Text
encode cumul_frequences text del =
    let init_low  = 0
        init_high = largest_integer
    in encode' init_low init_high text

    where
        -- takes symbols one by one, shrinks (and extends back) the interval
        -- and writes the bits
        -- it is assumed that eof is present in text as the last and only last symbol
        encode' _ _ T.empty = T.empty
        encode' p_low p_high p_cumul_prob text =
            let Just (symbol, rest) = uncons text
                cumul_prob       = cumul_frequences M.! symbol
                range            = p_high - p_low + 1

                low  = p_low + (range * cumul_prob) / del
                high = p_low + (range * p_cumul_prob) / del - 1

                (bits, n_low, n_high) = shrink_write low high 0 T.empty
            in  bits `T.append` encode' n_low n_high cumul_prob rest

        -- instruction on how to shrink the interval and restore it back
        -- (to keep precision), and which bits correspond to what
        shrink_write :: Integer -> Integer -> Integer -> T.Text -> T.Text
            -> (T.Text, Integer, Integer)
        shrink_write p_low p_high bits_to_foll result
         | p_high < half_int =
            let bits = write_bits 0 bits_to_foll
                r    = result `T.append` bits
                low  = p_low  * 2
                high = p_high * 2 + 1
            in  shrink_write low high 0 r

         | p_low >= half_int
            let bits = write_bits 1 bits_to_foll
                r    = result `T.append` bits
                low  = (p_low  - half_int) * 2
                high = (p_high - half_int) * 2 + 1
            in  shrink_write low high 0 r

         | p_low >= first_qtr && p_high < third_qtr =
                low  = (p_low  - first_qtr) * 2
                high = (p_high - first_qtr) * 2 + 1
            in  shrink_write low high (bits_to_foll + 1) r

         -- it is said that this algorithm always terminates
         | otherwise = (result, p_low, p_high)
        
        -- writes a bit and not that bit repeated
        write_bits value following =
            value `T.cons` ( replicate following (bnot value) )
        where
            bnot 1 = 0
            bnot 0 = 1


------ Decoding ------

decode :: M.Map Char Double -> T.Text -> T.Textdecode :: 
