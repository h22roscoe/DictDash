module Main where

import System.IO
import System.Environment

import DictDash (dictDash, dictDashRoute)

main :: IO ()
main = do
  args <- getArgs
  main' args
  where
    -- main' can process the arguments given and perform the
    -- correct IO function depending on those arguments.
    main' :: [String] -> IO ()
    main' []            = withoutFile
    main' (file : rest) = do
      handle <- openFile file ReadMode
      contents <- hGetContents handle
      
      let [start, end, dict_temp] = lines contents
      let dict = words dict_temp
      
      putStrLn $ show $ dictDash start end dict

      let how = if null rest then "" else head rest
      
      if how `elem` ["how", "how?", "How", "How?"]
      then putStrLn $ show $ dictDashRoute start end dict
      else putStrLn "You can also find out the route by saying `how?`"


-- This IO is performed when no file name argument is given
-- and gathers the appropriate strings to perform the function on.
withoutFile :: IO ()
withoutFile = do
  putStr "Please enter the start word: "
  start <- getLine
  putStr "Please enter the word to finish at: "
  end <- getLine
  putStr "And now the space delimited dictionary of words: "
  dict_temp <- getLine
  let dict = words dict_temp
  
  putStrLn $ show $ dictDash start end dict
