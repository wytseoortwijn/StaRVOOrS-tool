module Main where

import System.Directory
import System.Environment as SE
import System.Process
import Types
import CommonFunctions
import Parser
import ErrM
import Absppdate
import UpgradePPDATE
import qualified Data.Map as MAP
import StaticAnalysis
import Translator2DATE
import System.Console.GetOpt
import Instrumentation
import Data.Functor

-----------
-- Flags --
-----------

data Flag = Version 
 | OnlyParse String
    deriving Show
   
options :: [OptDescr Flag]
options =
    [ Option "v" ["version"] (NoArg Version) "show version number"
    , Option "p" ["only_parse"] (ReqArg OnlyParse "address") "only parse the ppDATE script"    
    ]

-------------
-- Version --
-------------

version :: String
version = "StaRVOOrS 1.35.5"

----------
-- Main --
----------

main :: IO ()
main =
  do
    args <- SE.getArgs
    case getOpt Permute options args of 
      ([Version],[],[]) -> putStrLn version
      ([OnlyParse ppdate_fn],[],[]) -> 
        do s <- isPPDATEfile ppdate_fn
           if (s == "")
           then do ppdate_txt <- readFile ppdate_fn
                   case parse ppdate_txt of
                        Bad s        -> putStrLn $ "\nThe parsing has failed: " ++ s ++ "\n"
                        Ok absppdate -> let ppd = upgradePPD absppdate in
                                        case runStateT ppd emptyEnv of
                                             Bad s -> putStrLn $ "\nThe parsing has failed: " ++ s ++ "\n"
                                             Ok _  -> putStrLn "\nThe parsing was successful.\n"
           else putStrLn s
      ([],[java_fn_add, ppdate_fn, output_add],[]) -> run [] java_fn_add ppdate_fn output_add      
      (_,_,[]) -> do
                     name <- getProgName
                     putStrLn ("Usage: " ++ name ++ " [-OPTIONS] <java_source_files_address> <ppDATE_file> <output_address>")
      (_,_,errs) -> sequence_ $ map putStrLn errs


--Runs StaRVOOrS --

run :: [Flag] -> FilePath -> FilePath -> FilePath -> IO ()
run flags java_fn_add ppdate_fn output_add =  
 do putStrLn $ "\nWelcome to StaRVOOrS " ++ version ++ "\n" 
    b2 <- doesDirectoryExist output_add
    b3 <- doesDirectoryExist java_fn_add
    if (not b3)
    then putStrLn $ "\nError: Directory " ++ java_fn_add ++ " does not exist.\n"
    else 
       do s <- isPPDATEfile ppdate_fn
          if (not (s == ""))
          then putStrLn s
          else 
            if (not b2)
            then putStrLn $ "\nError: Directory " ++ output_add ++ " does not exist.\n"
            else 
              do let java_fn_add' = if ((last $ trim java_fn_add) == '/') 
                                    then java_fn_add 
                                    else java_fn_add ++ "/"
                 ppdateP <- fmap parse $ readFile ppdate_fn
                 case ppdateP of
                      Bad s        -> do putStrLn $ "\nThe parsing has failed: " ++ s
                      Ok absppdate -> 
                         do      
                            let output_addr = if ((last $ trim output_add) == '/') 
                                              then output_add
                                              else output_add ++ "/"
                            let output_add' = output_addr ++ "out"
                            checkOutputDirectory output_add'
                            createDirectoryIfMissing False output_add'      
                            createDirectoryIfMissing False (output_add' ++ "/workspace")
                            let ppd  = upgradePPD absppdate
                            case runStateT ppd emptyEnv of
                                 Bad s -> putStrLn s
                                 Ok _  -> do ppd' <- programVariables ppd java_fn_add'
                                             ppdate <- programMethods ppd' java_fn_add'
                                             putStrLn "Initiating static verification of Hoare triples with KeY."
                                             ppdate' <- staticAnalysis java_fn_add' ppdate output_add' ppdate_fn
                                             putStrLn "Initiating monitor files generation."
                                             let larva_fn  = generateLarvaFileName ppdate_fn
                                             let larva_add = output_addr ++ "out/" ++ larva_fn
                                             writeFile larva_add ""
                                             translate ppdate' larva_add
                                             putStrLn "Running LARVA..."
                                             rawSystem "java" ["-jar","larva.jar",larva_add,"-v","-o",output_add']
                                             putStrLn "Monitor files generation completed."
                                             removeDirectoryRecursive (output_add' ++ "/workspace") 
                                             out_dir_content <- getDirectoryContents output_add'
                                             putStrLn "StaRVOOrS has finished successfully.\n"

-------------------------
-- Auxiliary Functions --
-------------------------

checkOutputDirectory :: FilePath -> IO ()
checkOutputDirectory file = 
 do b <- doesDirectoryExist file
    if b then removeDirectoryRecursive file
         else return ()

generateLarvaFileName :: Filename -> Filename
generateLarvaFileName fn = 
 let (ext, _:name) = break ('.' ==) $ reverse fn 
     xs = splitOnIdentifier "/" name     
 in if (length xs == 1)
    then reverse name ++ ".lrv"
    else reverse (head xs) ++ ".lrv"

--Checks if the the file in FilePath has .ppd extension (i.e., is a ppDATE file)
isPPDATEfile :: FilePath -> IO String
isPPDATEfile ppdate_fn = 
 do b1 <- doesFileExist ppdate_fn
    if (not b1)
    then return $ "\nError: File " ++ ppdate_fn ++ " does not exist.\n"
    else let (ext,_) = break ('.' ==) $ reverse ppdate_fn
         in if (ext == "dpp")
            then return ""
            else return $ "\nError: File " ++ ppdate_fn ++ " is not a ppDATE file.\n"

