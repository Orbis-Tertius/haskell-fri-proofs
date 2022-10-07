module Die (die) where


import qualified System.Exit as Exit
import System.IO.Unsafe (unsafePerformIO)


die :: String -> a
die = unsafePerformIO . Exit.die
