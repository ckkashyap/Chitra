import qualified Chitra.Canvas as C
import System.Environment

main = do
	args <- getArgs
	case args of
		[w,h,port] -> C.start width height port
			where
				width = read w :: Int
				height = read h :: Int
		_ -> putStrLn "Usage: ./Main width height port"
