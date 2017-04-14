import System.Environment
import System.IO
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO

import qualified Haffman

coderFromMode "haffman_e" = Haffman.encodeFromFiles
coderFromMode "haffman_d" = Haffman.decodeFromFiles
coderFromMode invalid = \_ -> return . T.pack $ (invalid ++ " is not an availible coder\n")

main = do
	argv <- getArgs
	prg_name <- getProgName
	if length argv == 0
	then
		putStrLn $ "Usage: " ++ prg_name ++ " mode"
	else
		let (mode : filenames) = argv
		    coder = coderFromMode mode
		in  coder filenames >>= TIO.putStr
