module DictDash (dictDash, dictDashRoute) where

import Data.List
import Data.Maybe
import Data.Function (on)
import Control.Monad

-- Params:
--     start - Is the word in the dictionary we will start traveling from.
--     end   - Is the word in the dictionary we are trying to reach.
--     dict  - Is the dictionary of words that can be used to reach end.
-- Returns:
--     Just the number of single letter changes to get from start to end.
--     Or Nothing if there is no path from start to end.
dictDash :: String -> String -> [String] -> Maybe Int
dictDash start end dict
  | lenS == lenE = dictDash' start end sameLenDict 0
  | otherwise    = Nothing
  where
    lenS = length start
    lenE = length end
    sameLenDict  = filter ((== lenS) . length) dict

    -- 0 is passed into the helper function to make it tail recursive.
    -- This helper function performs the actual filtering and gathering
    -- of the algorithm.
    -- oneDiffs - Is the list of words in the dict which are one letter
    -- different to the start word
    -- nextWave - Is the list of all the minimum distances from each of the
    -- words in oneDiffs to the end (if there is one)
    -- We then choose the minimum of the nextWave if there is one.
    dictDash' _ _ [] _
      = Nothing -- Cannot perform the transition with this dictionary.
    dictDash' start end dict count
      | start == end = Just count -- The base case.
      | otherwise    = minimumMaybe nextWave
      where
        newDict  = filter ((/=) start) dict
        oneDiffs = filter (oneDiffSameLen start) newDict
        nextWave = [dictDash' newS end newDict (count + 1) | newS <- oneDiffs]

-- Params:
--    The exact same as dictDash above
-- Returns:
--    The shortest route taken between the start and end in the dictionary
--    if it exists. Otherwise Nothing.
dictDashRoute :: String -> String -> [String] -> Maybe [String]
dictDashRoute _ _ []
  = Nothing
dictDashRoute start end dict
  | start == end = Just [end]
  | otherwise    = fmap ((:) start) $ shortest nextWave
  where
    newDict  = delete start dict
    oneDiffs = filter (oneDiff start) newDict
    nextWave = [dictDashRoute newS end newDict | newS <- oneDiffs]

-- Params:
--     xs - A list of potential lists in which we want to find the shortest
-- Returns:
--     The shortest list in xs if there is one. Otherwise nothing. 
shortest :: [Maybe [a]] -> Maybe [a]
shortest []
  = Nothing
shortest xs
  = shortest' onlyJusts
  where
    onlyJusts = catMaybes xs

    -- shortest' is needed here so that if the filter actually empties the list
    -- then we can still return nothing. Otherwise we might get an error from
    -- minimumBy for calling on an empty list.
    shortest' [] = Nothing
    shortest' xs = Just $ minimumBy (compare `on` length) xs

-- Params:
--     xs - Is a list of possible Ints which we need to find the minimum of.
-- Returns:
--     Nothing if the list is empty or if any of the list is Nothing.
--     Just the minimum of the list in all other cases.
minimumMaybe :: [Maybe Int] -> Maybe Int
minimumMaybe []
  = Nothing
minimumMaybe xs
  = minimumMaybe' $ catMaybes xs -- If there is a Nothing in the list i.e. a
                                 -- particular route didn't work out, then
                                 -- we still want to see the minimum of the
                                 -- rest.
  where
    -- minimumMaybe' is needed so that I don't use minimum on an empty list
    -- which would throw an exception. 
    -- The onlyJusts could be empty even if xs wasn't.
    minimumMaybe' [] = Nothing
    minimumMaybe' xs = Just $ minimum xs

-- Params:
--     word  - Is just a String that we want to find if it is one letter
--             different to other
--     other - Is just the another String that may or may not be one
--             letter different to word
-- Returns:
--     Whether the two inputs are one letter different.
oneDiff :: String -> String -> Bool
oneDiff word other
  = if length word == length other
    then oneDiffSameLen word other
    else False -- Because all one letter differences are replacements
               -- and there are no additions or removals.

-- oneDiffSameLen counts the number of differences in two equal length strings
-- and if it reaches the end of both with one difference then returns True
-- Params:
--     same as oneDiff above except pre cond is they must be same length.
-- Returns:
--     Whether or not the two strings differ by 1 character replacement.
oneDiffSameLen :: String -> String -> Bool
oneDiffSameLen word other
  = oneDiffSameLen' word other 0
  where
    -- We now take in an extra arg in the helper function to tell us the number
    -- of differences which starts at 0 and if it ever goes > 1 we know it
    -- is false, however it is only true if we reach the end with one diff
    oneDiffSameLen' "" "" 1 = True
    oneDiffSameLen' "" "" _ = False
    oneDiffSameLen' (w : ws) (o : os) diffs
      = if diffs > 1 -- This is to save us going any further if there are too
        then False   -- many differences already. 
        else if w == o
        then oneDiffSameLen' ws os diffs
        else oneDiffSameLen' ws os (diffs + 1)

