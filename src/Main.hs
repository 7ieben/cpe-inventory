module Main where

import Control.Monad (when, mapM_)
import Data.CPE
import Data.Maybe (mapMaybe)
import Prelude hiding (product)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hGetContents)
import System.Process (StdStream(CreatePipe), proc, createProcess, std_out, waitForProcess)

type CPEList = [CPERecord]

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 1) $ do
         prog <- getProgName
         putStrLn $ "Syntax: " ++ prog ++ " CPEFILE"
         exitFailure
  
  cpes <- fileToCPEList =<< readFile (head args) 
  (_, Just hout, _, p) <- createProcess (proc "pacman" ["-Q"]) { std_out = CreatePipe }
  waitForProcess p
  pout <- hGetContents hout
    
  mapM_ putStrLn $ mapMaybe (compareCPE ((map words . lines) pout)) cpes

  
fileToCPEList :: String -> IO CPEList
fileToCPEList file = return $ map read $ lines file

compareCPE :: [[String]] -> CPERecord -> Maybe String
compareCPE ss cpe = case version cpe of
                      "" -> if product cpe `elem` map head ss
                            then Just $ product cpe
                            else Nothing
                      _ -> if [product cpe, version cpe] `elem` ss
                           then Just $ unwords [product cpe, version cpe]
                           else Nothing
