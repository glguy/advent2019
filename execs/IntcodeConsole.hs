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
  , initialRel   :: Int
  , initialPC    :: Int
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


  , let parsePC str
          | [(i,"")] <- reads str =
                Right (\a -> a{initialPC = i})
          | otherwise = Left "Unable to parse program counter"

    in Option ['p'] ["pc"] (ReqArg parsePC "POS") "Override initial program counter"


  , let parseRel str
          | [(i,"")] <- reads str, 0 <= i =
                Right (\a -> a{initialRel = i})
          | otherwise = Left "Unable to parse relative base"

    in Option ['r'] ["rel"] (ReqArg parseRel "POS") "Override initial relative base"
  ]

getAppArgs :: IO Args
getAppArgs =
  do args <- getArgs
     case getOpt RequireOrder options args of
       (flagResult, filenames, errors)
         | null allErrors ->
              pure $ foldl (flip id)
                    Args { argsFilename = head filenames
                        , overrides   = []
                        , initialmode = OutASCII
                        , initialRel  = 0
                        , initialPC   = 0 }
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
         do let i1 = foldl' (\acc (p,v) -> set p v acc)
                            (new i)
                            (overrides args)
                i2 = i1 { pc      = initialPC  args
                        , relBase = initialRel args }
            pure i2


main :: IO ()
main =
  do args <- getAppArgs
     mach <- getMachine args
     launch Console
       { machine = mach
       , looper  = looperStep mach
       , history = Seq.Empty
       , inputq  = []
       , outmode = initialmode args }

------------------------------------------------------------------------
-- Console logic
------------------------------------------------------------------------

data Console = Console
  { machine :: !Machine
  , looper  :: !(Maybe Machine)
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

looperStep :: Machine -> Maybe Machine
looperStep m =
  case step m of
    Step m'      -> Just m'
    StepOut _ m' -> Just m'
    _            -> Nothing

launch :: Console -> IO ()
launch con
  | Just (machine con) == looper con =
          do errorPrintLn "<<LOOP>>"
             prompt con{ looper = looperStep (machine con) }
  | otherwise =

  let looper' = looperStep =<< looperStep =<< looper con in
  case step (machine con) of
    Step m' ->
      launch con{ machine = m', looper = looper' }

    StepOut o m' ->
      do emit o (outmode con)
         launch con{ machine = m', looper = looper' }

    StepIn resume ->
      case inputq con of
        x:xs -> let m = resume x
                in launch con{ inputq = xs, machine = m, looper = looperStep m }
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
         setTitle ("Intcode Console" ++
                   " PC:" ++ show (pc (machine con)) ++
                   " REL:" ++ show (relBase (machine con)))
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
        Nothing   -> errorPrintLn "Unknown command" >> prompt con
        Just impl -> impl args con

------------------------------------------------------------------------
-- Command implementations
------------------------------------------------------------------------

commandImpls :: Map String ([String] -> Console -> IO ())
commandImpls = Map.fromList
  [("back"  , backCommand  )
  ,("dump"  , dumpCommand  )
  ,("ints"  , intsCommand  )
  ,("output", outputCommand)
  ,("help"  , helpCommand  )
  ,("info"  , infoCommand  )
  ,("changes" , changesCommand  )
  ]

infoCommand :: [String] -> Console -> IO ()
infoCommand _ con =
  do metaprintLn ("PC : " ++ show (pc      (machine con)))
     metaprintLn ("REL: " ++ show (relBase (machine con)))
     metaprintLn ("CNG: " ++ show (length (memory (machine con))))
     prompt con

helpCommand :: [String] -> Console -> IO ()
helpCommand _ con =
  do metaprintLn ("Available commands:" ++
                  concat [' ':name | name <- Map.keys commandImpls])
     prompt con

outputCommand :: [String] -> Console -> IO ()
outputCommand args con =
  case args of
    [modestr] | Just mode <- lookup modestr availableModes ->
        launch con{ outmode = mode }

    _ -> do errorPrintLn ("Valid modes:" ++ concat [' ':name | (name,_) <- availableModes])
            prompt con

changesCommand :: [String] -> Console -> IO ()
changesCommand _ con =
  do metaprintLn (show (memory (machine con)))
     prompt con

backCommand :: [String] -> Console -> IO ()
backCommand args con =
  case param of
    Just i
      | (l, m Seq.:<| _) <- Seq.splitAt i (history con) ->
              launch con{ history = l
                        , machine = m
                        , inputq  = [] }
    _ ->
      do errorPrintLn "Bad arguments to back"
         prompt con
  where
    param =
      case args of
        [istr] -> readMaybe istr
        []     -> Just (length (history con) - 1)
        _      -> Nothing

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
