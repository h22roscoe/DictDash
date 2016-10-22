import Test.QuickCheck
import Test.HUnit

import DictDash (dictDash)

-- These two props test the edge cases of the publicly exposed
-- function dictDash which are when the lengths don't match
-- or the dict given is empty
prop_differentLengthsGivesNothing start end dict
  = (length start) /= (length end) ==> dictDash start end dict == Nothing
 
prop_emptyDictGivesNothing start end
  = dictDash start end [] == Nothing

-- Establish some common useful strings to test with
start = "hit"
end   = "cog"
dict  = ["hit", "dot", "dog", "cog", "hot", "log"]
dict2 = ["dot", "dog", "cog", "hid", "hod", "had", "hat", "bat", "bot"]
noEnd = ["hat", "hot", "not", "nor", "xor"]
diffLengths
  = ["hate", "bit", "bob", "ma", "hi", "log", "toga", "bot", "cob", "cog"]

givenExample
  = TestCase $ assertEqual "Given example" (Just 4) (dictDash start end dict)

moreComplexExample
  = TestCase $ assertEqual "More complex" (Just 6) (dictDash start end dict2)

noEndTest
  = TestCase $ assertEqual "No end" Nothing (dictDash start end noEnd)

ignoreDiffLengths
  = TestCase $ assertEqual "Ignores different lengths in dict"
                           (Just 5)
                           (dictDash start end diffLengths)

tests
  = TestList [
    givenExample,
    moreComplexExample,
    ignoreDiffLengths
  ]

main = do
  quickCheck prop_differentLengthsGivesNothing
  quickCheck prop_emptyDictGivesNothing

  runTestTT tests
