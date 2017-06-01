module C_Arithm
( encode
, encodeFromFiles
, decode
, decodeFromFiles
, cumulatedFreqsInFile
) where

import qualified Data.Map          as M
import qualified Data.Text.Lazy    as T
import qualified Data.Text.Lazy.IO as TIO
import System.IO

import Utils

------ Model ------
-- We're using static model, so it's all easy

bits_in_int     = 16
largest_integer = 2 ^ bits_in_int - 1
integer_modulo  = 2 ^ bits_in_int

first_qtr = largest_integer `div` 4 + 1
half_int  = first_qtr * 2
third_qtr = first_qtr * 3

eof = '\0'

bit_to_text :: Char -> Integer
bit_to_text '1' = 1
bit_to_text '0' = 0

-- each symbol is bijected to his cumulative probability and
-- the previous symbols's one
-- note to self: cumulative probability is a sum of probabilities of all symbols
-- before current including current
type Prob_map = M.Map Char (Integer, Integer)

------ accumulating frequency counting ------

count_frequencies :: T.Text -> (Prob_map, Integer)
count_frequencies text =
    -- count occurences for each symbol and put into the list
    -- also add a code for eof symbol
    let occurences = (eof, 1) : (M.toAscList . M.fromListWith (+) . ( `zip` [1,1..] ) $ T.unpack text)
        -- make a list of partial sums from occurences
        cumulated  = zipWith accumulate occurences (('\0', (0, 0)) : cumulated)
        -- this number is used for delayed division (to find real probability)
        -- [ ... (_, (this one, _)) ]
        del = fst . snd . last $ cumulated
    in  (M.fromList cumulated, del)
    where accumulate (c, cur) (_, (prev, _)) = (c, (cur + prev, prev))

------ Encoding ------

encode :: Prob_map -> T.Text -> Integer -> T.Text
encode cumul_frequences text del =
    let init_low  = 0
        init_high = largest_integer
    in  encode' init_low init_high text 0
    --
    where
        -- takes symbols one by one, shrinks (and extends back) the interval
        -- and writes the bits
        -- it is assumed that eof is present in text as the last and only last symbol
        encode' :: Integer -> Integer -> T.Text -> Integer -> T.Text
        encode' p_low p_high text bits_to_follow
         | text == T.empty = T.empty
         | otherwise =
            let Just (symbol, rest)        = T.uncons text
                (cumul_prob, cumul_pos) = cumul_frequences M.! symbol
                range                   = p_high - p_low + 1
                --
                low  = p_low + (range * cumul_pos)  `div` del
                high = p_low + (range * cumul_prob) `div` del - 1
                --
                (bits, n_low, n_high, btf) =
                    shrink_write low high bits_to_follow T.empty
            in  bits `T.append` encode' n_low n_high rest btf
        --
        -- instruction on how to shrink the interval and restore it back
        -- (to keep precision), and which bits correspond to what
        shrink_write :: Integer -> Integer -> Integer -> T.Text
            -> (T.Text, Integer, Integer, Integer)
        shrink_write p_low p_high bits_to_foll result
         -- zero if in lower half
         | p_high < half_int =
            let bits = write_bits '0' bits_to_foll
                r    = result `T.append` bits
                low  = p_low  * 2
                high = p_high * 2 + 1
            in  shrink_write low high 0 r
         -- 1 if in higher half
         | p_low >= half_int =
            let bits = write_bits '1' bits_to_foll
                r    = result `T.append` bits
                low  = (p_low  - half_int) * 2
                high = (p_high - half_int) * 2 + 1
            in  shrink_write low high 0 r
         -- write and opposite bit lare if in the middle
         | p_low >= first_qtr && p_high < third_qtr =
            let low  = (p_low  - first_qtr) * 2
                high = (p_high - first_qtr) * 2 + 1
            in  shrink_write low high (bits_to_foll + 1) result
         --
         -- it is said that this algorithm always terminates
         | otherwise = (result, p_low, p_high, bits_to_foll)
        --
        -- writes a bit and not that bit repeated
        write_bits value following =
            value `T.cons`
                T.replicate (fromIntegral following) (bnot_text value)
            where
                bnot_text '1' = T.singleton '0'
                bnot_text '0' = T.singleton '1'


------ Decoding ------

decode :: Prob_map -> T.Text -> Integer -> T.Text
decode cumul_frequences cipher del =
    let init_low    = 0
        init_high   = largest_integer
        -- ehhh. Code is alright, i despise the need in this
        -- puts first bits_in_int bits to integer value
        init_repres =
            T.foldl (\s c -> s * 2 + bit_to_text c) 0
                $ T.take bits_in_int cipher
        -- don't forget to drop reAd bits in ciphertext
        cipher' = T.drop bits_in_int cipher
        -- also there's a strange thing with following bits..
        cipher'' = cipher' `T.snoc` '0' `T.append` (T.repeat '1')
        -- BINGO! TODO: not a kosyl solution
    in  decode' init_low init_high init_repres cipher''
    where
        --
        decode' low high repres bits
         | bits == T.empty = T.pack "--the provided text seems to be incorrect"
         | otherwise =
            let range  = high - low + 1
                accum  = ((repres - low + 1) * del - 1) `div` range
                -- first symbol with cumulative frequency greater than accumulated
                Just symbol = M.foldlWithKey (first_gt accum) Nothing cumul_frequences where
                    first_gt :: Integer -> Maybe Char -> Char -> (Integer, Integer) -> Maybe Char
                    first_gt _ (Just c) _ _ = Just c
                    first_gt a Nothing c (y, _) =
                        if y > a
                        then Just c
                        else Nothing
                --
                (cumul_prob, cumul_pos) = cumul_frequences M.! symbol
                --
                c_low  = low + (range * cumul_pos)  `div` del
                c_high = low + (range * cumul_prob) `div` del - 1
                --
                (n_low, n_high, n_repr, n_bits) = shrink_read c_low c_high repres bits
            in  if symbol == '\0'
                then T.singleton symbol
                else symbol `T.cons` decode' n_low n_high n_repr n_bits
        --
        shrink_read low high x bits 
--          | bits == T.empty = (0, 0, 0, T.empty) -- kostyl
         | bits == T.empty = error "something went wrong when decoding!"
         | high < half_int =
            let val = ((x * 2) `mod` integer_modulo) + (bit_to_text . T.head $ bits)
                n_low  = 2 * low
                n_high = 2 * high + 1
            in  shrink_read n_low n_high val (T.tail bits)
         --
         | low >= half_int =
            let val = (((x - half_int) * 2) `mod` integer_modulo) + (bit_to_text . T.head $ bits)
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

------ Utilitary routine ------

strip_eof :: T.Text -> T.Text
strip_eof text =
    let stripped = T.stripSuffix (T.singleton eof) text
    in  case stripped of
        Nothing   -> text
        Just t -> t

add_eof :: T.Text -> T.Text
add_eof text =
    if (T.singleton eof) `T.isSuffixOf` text
    then text
    else text `T.snoc` eof


------ Input-output encoding/decoding ------

encodeFromFiles :: [FilePath] -> IO T.Text

encodeFromFiles [filename] = do
    (freqs, del)  <- count_frequencies `onFile` filename
    working_text  <- openFile filename ReadMode >>= TIO.hGetContents >>= return . add_eof . T.strip
    return $ encode freqs working_text del
encodeFromFiles _ = noFileSupplied "arithmetic_e textfile"


decodeFromFiles :: [FilePath] -> IO T.Text

decodeFromFiles [probs_name, working_name] = do
    probs_text   <- openFile probs_name   ReadMode >>= TIO.hGetContents >>= return . T.strip
    let (freqs, del) = read . T.unpack $ probs_text
    working_text <- openFile working_name ReadMode >>= TIO.hGetContents >>= return . T.strip
    return . strip_eof $ decode freqs working_text del
decodeFromFiles _ = noFileSupplied "arithm_d probabilities textfile"
            

cumulatedFreqsInFile :: [FilePath] -> IO T.Text
cumulatedFreqsInFile = onFirstFile (T.pack . show . count_frequencies)



















