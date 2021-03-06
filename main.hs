import System.Environment
import qualified Data.Text.Lazy    as T
import qualified Data.Text.Lazy.IO as TIO

import qualified Haffman
import qualified Arithmetic
import qualified C_Arithm
import Utils

coderFromMode "haffman_e" = Haffman.encodeFromFiles
coderFromMode "haffman_d" = Haffman.decodeFromFiles
coderFromMode "haffman_b" = Haffman.buildFromFiles

coderFromMode "arithm_e"  = Arithmetic.encodeFromFiles
coderFromMode "arithm_d"  = Arithmetic.decodeFromFiles

coderFromMode "carithm_e" = C_Arithm.encodeFromFiles
coderFromMode "carithm_d" = C_Arithm.decodeFromFiles
coderFromMode "carithm_b" = C_Arithm.cumulatedFreqsInFile

coderFromMode "count_letters" = countProbabilitiesInFile

coderFromMode invalid = \_ -> return . T.pack $ (invalid ++ " is not an availible coder\n")

main = do
	argv <- getArgs
	prg_name <- getProgName
	if null argv
	then
		putStrLn $ "Usage: " ++ prg_name ++ " mode"
	else
		let (mode : filenames) = argv
		    coder = coderFromMode mode
		in  coder filenames >>= TIO.putStr
