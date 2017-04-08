import System.Environment
import System.IO

import qualified Haffman

coderFromMode "haffman_e" = Haffman.encodeFromFiles
coderFromMode "haffman_d" = Haffman.decodeFromFiles

main = do
	argv <- getArgs
	prg_name <- getProgName
	if length argv == 0
	then
		putStrLn $ "Usage: " ++ prg_name ++ " mode"
	else
		let (mode : filenames) = argv
		    coder = coderFromMode mode
		in  coder filenames >>= putStr
