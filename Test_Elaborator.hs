-- Rudimentary test suite. Feel free to replace anything.

import Absyn
-- import Parser
import Elaborator
-- import Solver

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests

-- tests = testGroup "Minimal tests" [
--   testCase "parser" $
--     parseString dbt @?= Right dbi,
--   testCase "elaborator" $
--     elaborate dbi @?= Right dbf,
--   testCase "solver" $
--     solve dbf goal 3 @?= Right sol
--   ]
--   where
--     dbt = "resource r. component c: provides r."

--     dbi = (["r"], [IC "c" [(CKProvides, RSRes "r")]])
--     dbf = ([R "r"], [("c", [(R "r", (1,0))])])

--     goal = [(R "r", (0,1))]
--     sol = [("c", 1)]





tests :: TestTree
tests = testGroup "Tests" [unitests_Elaborator]
  


unitests_Elaborator::TestTree
unitests_Elaborator = testGroup "---------Unit Tests for Solver----------"
  [testCase "lookres Test 1: cannot find RName in [Resource]" $
      lookres [R "R", R "a"] "b" @?= Left "not found resource",
   testCase "lookres Test 2: resource and rname are both uppercase" $
      lookres [R "A", R "B"] "B" @?= Right (R "B"),
   testCase "lookres Test 3: lookup lowercased rname in uppercased resource list" $
      lookres [R "A", R "B"] "a" @?= Right (R "A"),
   testCase "lookres Test 3: lookup uppercased rname in lowercased resource list" $
      lookres [R "a", R "b"] "A" @?= Right (R "a"),
   testCase "lookres Test 4: lookup uncanonical rname and return the canonical capitalized resource" $
      lookres [R "Disk", R "USB-hub"] "usb-hub" @?= Right (R "USB-hub"),


   testCase "elaborate Test 1: given in test framwork" $
      elaborate  (["r"], [IC "c" [(CKProvides, RSRes "r")]]) @?= Right ([R "r"], [("c", [(R "r", (1,0))])]),
   testCase "elaborate Test 2: clause be combined into profile" $
      elaborate  (["r"], [IC "c" [(CKProvides, RSRes "r")]]) @?= Right ([R "r"], [("c", [(R "r", (1,0))])]),
   testCase "elaborate Test 3: 2 clause with same CKind " $
      elaborate  (["r"], [IC "c" [(CKProvides, RSRes "r"), (CKProvides, RSRes "r")]]) @?= Right ([R "r"],[("c",[(R "r",(2,0))])]),
   testCase "elaborate Test 3: 2 clause with different CKind" $
      elaborate  (["r"], [IC "c" [(CKProvides, RSRes "r"), (CKRequires, RSRes "r")]]) @?= Right ([R "r"],[("c",[(R "r",(1,1))])]),
   
   testCase "elaborate Test 5: update resource in component to canonical capitalized as declared , LOWER to UPPER" $
      elaborate  (["A"], [IC "c" [(CKProvides, RSRes "a")]]) @?= Right ([R "A"], [("c", [(R "A", (1,0))])]),
   testCase "elaborate Test 6: update resource in component to canonical capitalized as declared , UPPER to LOWER" $
      elaborate  (["a"], [IC "c" [(CKProvides, RSRes "A")]]) @?= Right ([R "a"], [("c", [(R "a", (1,0))])]),
   

   testCase "elaborate Test 7:  2 A == A, A" $
      elaborate  (["r"], [IC "c" [(CKProvides, RSNum 2 (RSRes "r")), (CKRequires, RSAnd (RSRes "r") (RSRes "r"))]]) @?= Right ([R "r"], [("c", [(R "r", (2,2))])]),
   testCase "elaborate Test 7:  require A; provides A == provide A; require A" $   
      elaborate (["a"],[IC "c" [(CKProvides,RSRes "a"),(CKRequires,RSRes "a")], IC "d" [(CKRequires,RSRes "a"), (CKProvides,RSRes "a")]]) @?= Right ([R "a"],[("c",[(R "a",(1,1))]),("d",[(R "a",(1,1))])])
   

  ]