module Main (main) where

import Advent.Intcode
import Data.Char (chr, ord)
import Data.List (intercalate)
import Data.Map (Map)
import Data.Sequence (Seq)
import System.Environment
import System.IO
import Text.Megaparsec (parse, errorBundlePretty)
import Text.Read (readMaybe)
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import System.Console.ANSI
import Control.Exception
import System.IO.Error
import System.Console.GetOpt
import System.Exit
import Data.Either
import Data.Foldable

------------------------------------------------------------------------
-- Command line arguments
------------------------------------------------------------------------

data Args = Args
  { argsFilename :: FilePath
  , overrides    :: [(Int,Int)]
  , initialmode  :: OutMode
  }

options :: [OptDescr (Either String (Args -> Args))]
options =

  [ let parseOverride str
          | [(pos,':':str1)] <- reads str
          , [(val,""      )] <- reads str1 =
                Right (\a -> a { overrides = overrides a ++ [(pos,val)] })
          | otherwise = Left ("Unable to parse override: " ++ show str)

    in Option ['s'] ["set"] (ReqArg parseOverride "POS:VAL") "Override initial memory"


  , let parseMode str
          | Just mode <- lookup str availableModes =
                Right (\a -> a{initialmode = mode})
          | otherwise =
                Left ("Valid modes:" ++ concat [' ':name | (name,_) <- availableModes])
    in Option ['o'] ["output"] (ReqArg parseMode "MODE") "Initial output mode"
  ]

getAppArgs :: IO Args
getAppArgs =
  do args <- getArgs
     case getOpt RequireOrder options args of
       (flagResult, filenames, errors)
         | null allErrors ->
              pure $ foldl (flip id)
                    Args { argsFilename = head filenames
                        , overrides = []
                        , initialmode = OutASCII }
                    updates

         | otherwise ->
              do errorPrintLn (usageInfo "Usage: IntcodeConsole <flags> FILENAME" options)
                 mapM_ errorPrintLn errors
                 exitFailure

         where
           (argErrors, updates) = partitionEithers flagResult
           filenameErrors =
             case filenames of
               []    -> ["missing filename"]
               _:_:_ -> ["too many filenames"]
               _     -> []
           allErrors = filenameErrors ++
                       argErrors ++
                       errors

------------------------------------------------------------------------
-- Initialization
------------------------------------------------------------------------

getMachine :: Args -> IO Machine
getMachine args =
  do imageText <- readFile (argsFilename args)
     case parse memoryParser (argsFilename args) imageText of
       Left e  -> fail (errorBundlePretty e)
       Right i ->
         do let i' = foldl' (\acc (p,v) -> set p v acc)
                            (new i)
                            (overrides args)
            pure i'


main :: IO ()
main =
  do args <- getAppArgs
     mach <- getMachine args
     launch Console
       { machine = mach
       , history = Seq.Empty
       , inputq  = []
       , outmode = initialmode args }

------------------------------------------------------------------------
-- Console logic
------------------------------------------------------------------------

data Console = Console
  { machine :: !Machine
  , history :: Seq Machine
  , inputq  :: [Int]
  , outmode :: OutMode
  }

data OutMode = OutASCII | OutInt
  deriving (Eq, Ord, Show, Read)

availableModes :: [(String, OutMode)]
availableModes = [("ascii", OutASCII), ("int", OutInt)]


addInput :: [Int] -> Console -> Console
addInput [] con = con
addInput xs con = con { history = history con Seq.|> machine con
                      , inputq = xs }


launch :: Console -> IO ()
launch con =
  case step (machine con) of
    Step m'      -> launch con{ machine = m' }
    StepOut o m' ->
      do emit o (outmode con)
         launch con{ machine = m' }
    StepIn resume ->
      case inputq con of
        x:xs -> launch con{ inputq = xs, machine = resume x }
        []   -> prompt con

    StepHalt ->
      do metaprintLn "<<HALT>>"
         prompt con

    StepFault ->
      do metaprintLn "<<FAULT>>"
         prompt con


prompt :: Console -> IO ()
prompt con =
      do -- draw the prompt
         setTitle ("Intcode Console PC:" ++ show (pc (machine con)))
         metaprint (show (length (history con)) ++ "> ")

         -- get next line
         setSGR [SetColor Foreground Dull Yellow]
         hFlush stdout
         line <- try getLine
         setSGR [Reset]

         -- process input
         case line of
           Left e | isEOFError e -> putStrLn ""
                  | otherwise    -> errorPrintLn (displayException e)
           Right [] -> prompt con
           Right ('!':command) -> runCommand command con
           Right str -> launch (addInput (map ord (str ++ "\n")) con)

runCommand :: String -> Console -> IO ()
runCommand command con =
  case words command of
    []       -> prompt con
    cmd:args ->
      case Map.lookup cmd commandImpls of
        Nothing   -> errorPrintLn "Unknown command"
        Just impl -> impl args con

------------------------------------------------------------------------
-- Command implementations
------------------------------------------------------------------------

commandImpls :: Map String ([String] -> Console -> IO ())
commandImpls = Map.fromList
  [("back", backCommand)
  ,("dump", dumpCommand)
  ,("ints", intsCommand)
  ,("output", outputCommand)
  ]

outputCommand :: [String] -> Console -> IO ()
outputCommand args con =
  case args of
    [modestr] | Just mode <- lookup modestr availableModes ->
        launch con{ outmode = mode }

    _ -> do errorPrintLn ("Valid modes:" ++ concat [' ':name | (name,_) <- availableModes])
            prompt con

backCommand :: [String] -> Console -> IO ()
backCommand args con =
  case args of
    [istr]
      | Just i <- readMaybe istr
      , (l, m Seq.:<| _) <- Seq.splitAt i (history con) ->
              launch con{ history = l
                        , machine = m
                        , inputq  = [] }
    _ ->
      do errorPrintLn "Bad arguments to back"
         prompt con

dumpCommand :: [String] -> Console -> IO ()
dumpCommand args con =
  case args of
    [filename] ->
      do let txt = intercalate "," (map show (memoryList (machine con))) ++ "\n"
         writeFile filename txt
         prompt con

    _ ->
      do errorPrintLn "Bad arguments to dump"
         prompt con

intsCommand :: [String] -> Console -> IO ()
intsCommand args con =
  case traverse readMaybe args of
    Just ints -> launch (addInput ints con)
    Nothing ->
      do errorPrintLn "Bad arguments to ints"
         prompt con

------------------------------------------------------------------------
-- Console IO
------------------------------------------------------------------------

metaprintLn :: String -> IO ()
metaprintLn str = metaprint (str ++ "\n")

metaprint :: String -> IO ()
metaprint str =
  do setSGR [ SetConsoleIntensity BoldIntensity
            , SetColor Foreground Dull Green ]
     putStr str
     setSGR [Reset]
     hFlush stdout

errorPrintLn :: String -> IO ()
errorPrintLn str =
  do setSGR [SetColor Foreground Dull Red]
     putStrLn str
     setSGR [Reset]
     hFlush stdout

emit :: Int -> OutMode -> IO ()
emit i mode
  | 0 <= i, i < 0x80, mode == OutASCII = putChar (chr i)
  | otherwise = metaprintLn ("<<OUT:" ++ show i ++ ">>")
