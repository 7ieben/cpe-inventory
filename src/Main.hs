module Main where

import Prelude hiding (product)
import Data.CPE
import System.Process
import Control.Monad (when)
import Data.Maybe (catMaybes)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO
import Text.Printf (printf)

-------------------------------------------------------------------

type CPEList = [CPERecord]

-------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
          
  when (length args /= 1) $ do
         prog <- getProgName
         putStrLn $ "Syntax: " ++ prog ++ " CPEFILE"
         exitFailure
  
  content <- readFile (head args)
  let cpes = fileToCPEList content 
  (_, Just hout, _, p) <- 
      createProcess (proc "pacman" ["-Q"]) { std_out = CreatePipe }
  systemInventory <- hGetContents hout
  let installed = (map head . map words . lines) systemInventory
      found = catMaybes $ map (compareCPE installed) cpes
      
  mapM_ putStrLn found
  
fileToCPEList :: String -> CPEList
fileToCPEList file = map read $ lines file

compareCPE ::  [String] -> CPERecord -> Maybe String
compareCPE s cpe = if (product cpe) `elem` s 
                   then Just (product cpe)
                   else Nothing
