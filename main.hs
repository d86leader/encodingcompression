import System.Environment
import System.IO

import qualified Haffman

coderFromMode "haffman_e" = Haffman.encodeFromFiles
coderFromMode "haffman_d" = Haffman.decodeFromFiles

main = do
	(mode : filenames) <- getArgs
	let coder = coderFromMode mode
	in  coder filenames >>= putStr
