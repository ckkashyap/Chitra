import qualified Chitra.Canvas as C
import System.Environment

main = do
	args <- getArgs
	let width = read $ args!!0 :: Int
	let height = read $ args!!1 :: Int
	C.start width height
