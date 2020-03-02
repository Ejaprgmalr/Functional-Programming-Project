-- Rudimentary test suite. Feel free to replace anything.

import Absyn
-- import Parser
-- import Elaborator
import Solver

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
tests = testGroup "Tests" [unitests_Solver]
  



unitests_Solver :: TestTree
unitests_Solver = testGroup "---------Unit Tests for Solver----------"
  [testCase "combine Test 1: given in page 12" $
     combine [(R "A", (3, 5)), (R "C", (-2, 0)), (R "D", (3, 0))] [(R "A", (2, 7)), (R "B", (3, 4)), (R "D", (-3, 0))] @?= [(R "A", (5, 7)), (R "B", (3, 4)), (R "C", (-2, 0))],
   testCase "combine Test 2: without same resource" $
     combine [(R "a", (1, 2))] [(R "b", (1, 2))] @?=  [(R "a", (1, 2)), (R "b", (1, 2))],
   testCase "combine Test 3: cancled result." $
     combine [(R "a", (1, 0))] [(R "a", (-1,0 ))] @?= [],


   testCase "verify Test 1 : goal contains undeclared resource" $
     verify ([R "A", R "B"], [("ah", [(R "A", (1, 0))])]) [(R "C", (0, 1))] [("ah", 1)] @?= Left "goal contains database undeclased resource",
   testCase "verify Test 2 : simple verify" $
     verify ([R "A"], [("ah", [(R "A", (1, 0))])]) [(R "A", (0, 1))] [("ah", 1)] @?= Right [(R "A", (1, 1))],
   testCase "verify Test 3:  more complicated verify"$
      verify ([R "#1", R "GB-RAM"],[("Ram-Block",[(R "#1",(1,0))]), ("#11",[(R "#1",(-1,0)),(R "GB-RAM",(2,0))]),("#12",[(R "#1",(-1,0)),(R "MB-RAM",(2048,0))])]) [(R "GB-RAM",(0,1))] [("Ram-Block",1), ("#11", 1)] @?=Right [(R "GB-RAM", (2,1))]
   -- testCase "solve Test 3:"
   --   solve ([R "r"], [("c", [(R "r", (1,0))])]) [(R "r", (0,1))] [("c", 1)] = Right [("c", 1)]
  
  ]
