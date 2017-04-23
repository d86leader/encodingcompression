import System.Environment
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO

import qualified Haffman
import Utils

coderFromMode "haffman_e" = Haffman.encodeFromFiles
coderFromMode "haffman_d" = Haffman.decodeFromFiles
coderFromMode "haffman_b" = Haffman.buildFromFiles
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
