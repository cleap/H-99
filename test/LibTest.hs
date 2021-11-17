module Main where

import Lib
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hspec
import System.Environment

{-
main :: IO ()
main =
    if myLast [1, 2, 3, 4] == 4 then return ()
    else fail "Wrong"
-}

main :: IO ()
main = do
    setEnv "TASTY_COLOR" "always"
    defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests = testGroup "Unit tests"
    [
        testCase "myLast [1, 2, 3, 4]" $ myLast [1,2,3,4] @?= 4,
        testCase "myLast ['x','y','z']" $ myLast ['x','y','z'] @?= 'z',
        testCase "myButLast [1,2,3,4]" $ myButLast [1,2,3,4] @?= 3,
        testCase "myButLast ['a'..'z']" $ myButLast ['a'..'z'] @?= 'y',
        testCase "elementAt [1,2,3] 2" $ elementAt [1,2,3] 2 @?= 2,
        testCase "elementAt \"haskell\" 5" $ elementAt "haskell" 5 @?= 'e',
        testCase "myLength [123, 456, 789]" $ myLength [123, 456, 789] @?= 3,
        testCase "myLength \"Hello, world!\"" $ myLength "Hello, world!" @?= 13,
        testCase "myReverse \"A man, a plan, a canal, panama!\"" $ myReverse "A man, a plan, a canal, panama!" @?= "!amanap ,lanac a ,nalp a ,nam A",
        testCase "myReverse [1,2,3,4]" $ myReverse [1,2,3,4] @?= [4,3,2,1],
        testCase "isPalindrome \"madamimadam\"" $ isPalindrome "madamimadam" @?= True,
        testCase "isPalindrome \"Steve\"" $ isPalindrome "Steve" @?= False,
        testCase "flatten (Elem 5)" $ flatten (Elem 5) @?= [5],
        testCase "flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])" $ flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]) @?= [1,2,3,4,5],
        testCase "compress \"aaaabccaadeeee\"" $ compress "aaaabccaadeeee" @?= "abcade",
        testCase "pack \"aaaabccaadeeee\"" $ pack "aaaabccaadeeee" @?= ["aaaa","b","cc","aa","d","eeee"],
        testCase "encode \"aaaabccaadeeee\"" $ encode "aaaabccaadeeee" @?= [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')],
        testCase "encodeModified \"aaaabccaadeeee\"" $ encodeModified "aaaabccaadeeee" @?= [Multiple 4 'a', Single 'b', Multiple 2 'c', Multiple 2 'a', Single 'd', Multiple 4 'e'],
        testCase "decodeModified [Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e']" $ decodeModified [Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e'] @?= "aaaabccaadeeee",
        testCase "encodeDirect \"aaaabccaadeeee\"" $ encodeDirect "aaaabccaadeeee" @?=  [Multiple 4 'a', Single 'b', Multiple 2 'c', Multiple 2 'a', Single 'd', Multiple 4 'e'],
        testCase "dupli [1,2,3]" $ dupli [1,2,3] @?= [1,1,2,2,3,3],
        testCase "repli \"abc\" 3" $ repli "abc" 3 @?= "aaabbbccc",
        testCase "dropEvery \"abcdefghik\" 3" $ dropEvery "abcdefghik" 3 @?= "abdeghk",
        testCase "split \"abcdefghik\" 3" $ split "abcdefghik" 3 @?= ("abc","defghik"),
        testCase "slice \"abcdefghik\" 3 7" $ slice "abcdefghik" 3 7 @?= "cdefg",
        testCase "rotate \"abcedfgh\" 3" $ rotate "abcdefg" 3 @?= "defgabc",
        testCase "rotate \"abcdefgh\" (-2)" $ rotate "abcdefgh" (-2) @?= "ghabcdef",
        testCase "removeAt 2 \"abcd\"" $ removeAt 2 "abcd" @?= ('b',"acd")
    ]
