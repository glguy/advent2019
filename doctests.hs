{-# language BlockArguments #-}
import Data.Foldable
import System.Directory
import System.FilePath
import System.IO
import Test.DocTest

main :: IO ()
main =
  do execs <- listDirectory "execs"
     let files = map ("execs"</>)
               $ filter (\x -> takeExtension x == ".hs") execs
     for_ files \file ->
       do hPutStrLn stderr ("Testing " ++ file)
          doctest ["-icommon", file]
