import Test.HUnit
import Unit01

p01 = TestCase (do assertEqual "for myLast [1, 2, 3, 4],"
                               4
                               (myLast [1,2,3,4])
                   assertEqual "for myLast ['x','y','z'],"
                               'z'
                               (myLast ['x','y','z']))

p02 = TestCase (do assertEqual "for myButLast [1,2,3,4],"
                               3
                               (myButLast [1,2,3,4])
                   assertEqual "for myButLast ['a'..'z'],"
                               'y'
                               (myButLast ['a'..'z']))

p03 = TestCase (do assertEqual "for elementAt [1,2,3] 2,"
                               2
                               (elementAt [1,2,3] 2)
                   assertEqual "for elementAt \"haskell\" 5,"
                               'e'
                               (elementAt "haskell" 5))

p04 = TestCase (do assertEqual "for myLength [123, 456, 789],"
                               3
                               (myLength [123, 456, 789])
                   assertEqual "for myLength \"Hello, world!\","
                               13
                               (myLength "Hello, world!"))

p05 = TestCase (do assertEqual "for myReverse \"A man, a plan, a canal, panama!\","
                               "!amanap ,lanac a ,nalp a ,nam A"
                               (myReverse "A man, a plan, a canal, panama!")
                   assertEqual "for myReverse [1,2,3,4],"
                               [4,3,2,1]
                               (myReverse [1,2,3,4]))

p06 = TestCase (do assertEqual "for isPalindrome \"madamimadam\","
                               True
                               (isPalindrome "madamimadam")
                   assertEqual "for isPalindrome \"Steve\","
                               False
                               (isPalindrome "Steve"))

p07 = TestCase (do assertEqual "for flatten (Elem 5),"
                               [5]
                               (flatten (Elem 5))
                   assertEqual "for flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]),"
                               [1,2,3,4,5]
                               (flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])))

p08 = TestCase (do assertEqual "for compress \"aaaabccaadeeee\","
                               "abcade"
                               (compress "aaaabccaadeeee"))

p09 = TestCase (do assertEqual "for pack \"aaaabccaadeeee\","
                               ["aaaa","b","cc","aa","d","eeee"]
                               (pack "aaaabccaadeeee"))

p10 = TestCase (do assertEqual "for encode \"aaaabccaadeeee\","
                               [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
                               (encode "aaaabccaadeeee"))

unit01 = TestList [TestLabel "problem01" p01,
                   TestLabel "problem02" p02,
                   TestLabel "problem03" p03,
                   TestLabel "problem04" p04,
                   TestLabel "problem05" p05,
                   TestLabel "problem06" p06,
                   TestLabel "problem07" p07,
                   TestLabel "problem08" p08,
                   TestLabel "problem09" p09,
                   TestLabel "probelm10" p10]
