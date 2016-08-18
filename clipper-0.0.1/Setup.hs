import Distribution.Simple
import System.Environment

main = do
  args <- getArgs
  let newArgs = if "build" `elem` args then ["--with-gcc=c++"] else []
  defaultMainArgs $ newArgs ++ args
