import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (assertEqual, assertBool, testCase)

import Eval
import Parser

main :: IO ()
--main = putStr "Заглушка"

main = defaultMain delimccTests
{-
wrap name str expected = testGroup name [ c1, c2 ]
  where
    isRight (Left _) = False
    isRight _ = True
    rez = parse str
    c1 = testCase (name ++ " parsable") $ assertBool  "parsable" (isRight rez)
    (Right tree) = rez
    c2 = testCase (name ++ " evaluates") $ assertEqual "parsable" (eval tree) expected
-}

delimccTests = testGroup "delimCC tests"
  [
   testCase "Test 1 " $ assertEqual [] (Right (TmCst 42)) (eval $ myParse "(reset (reset 42))"),
   testCase "Test 2 " $ assertEqual [] (Right (TmCst 42)) (eval $ myParse "(reset (shift k 42))"),
   testCase "Test 3 " $ assertEqual [] (Right (TmCst 42)) (eval $ myParse "(reset (shift k (k$42)))"),
   testCase "Test 4 " $ assertEqual [] (Right (TmCst 21)) (eval $ myParse "(reset (2*(shift k 21)))"),
   testCase "Test 5 " $ assertEqual [] (Right (TmCst 21)) (eval $ myParse "1+(reset (2*(shift k 20)))"),
   testCase "Test 6 " $ assertEqual [] (Right (TmCst 21)) (eval $ myParse "(reset (2*(shift k 20)))+1"),
   --next demo from Wiki
   testCase "Test 7 " $ assertEqual [] (Right (TmCst 42)) (eval $ myParse "2*(reset (1+(shift k (k$20))))"),
   --
   testCase "Test 8 " $ assertEqual [] (Right (TmCst 84)) (eval $ myParse "4*(reset (1+(shift k (k$(k$19)))))"),
   --new test
   testCase "Test 9 " $ assertEqual [] (Right (TmCst 42)) (eval $ myParse "(reset head (shift k (k$([(reset 1+(shift q (q$41))),15]))))"),
   testCase "Test 10 " $ assertEqual [] (Right (TmCst 42)) (eval $ myParse "head ([(if (isNil []) then 42 else 0),15])"),
   testCase "Test 11 " $ assertEqual [] (Right (TmCst 42)) (eval $ myParse "head ([(if (isNil (head [[],[],[]])) then 42 else 0),15])"),
   testCase "Test 12 " $ assertEqual [] (Right (TmCst 0)) (eval $ myParse "head ([(if (isNil (tail [])) then 0 else 42),15])"),
   testCase "Test 13 " $ assertEqual [] (Right (TmCst 42)) (eval $ myParse "head ([(if (isNil nil) then 42 else 0),15])"),
   testCase "Test 14 " $ assertEqual [] (Right (TmCst 0)) (eval $ myParse "head ([(if (reset 5*(shift k (k$0))) then 42 else 0),15])"),
   testCase "Test 15 " $ assertEqual [] (Right (TmCst 0)) (eval $ myParse "head ([(if (isNil(reset head (shift k (k$[])))) then 0 else 42),15])"),
   testCase "Test 16 " $ assertEqual [] (Right (TmList (TmCst 45) (TmList Nil Nil))) (eval $ myParse "tail ([(if (isNil(reset head (shift k (k$[])))) then 0 else 42),(reset (1+2)*(shift q (q$15))),[]])"),
   testCase "Test 17 " $ assertEqual [] (Right (TmCst 25)) (eval $ myParse "reset (if (head [1,2]) then 5 else 0)*(shift k (k$(k$1)))"),
   testCase "Test 18 " $ assertEqual [] (Right (TmCst 25)) (eval $ myParse "reset (head (tail [0,5,1]))*(shift k (k$(k$1)))"),

   --recursion test
   --factorial 6
   testCase "Test 19 " $ assertEqual [] (Right (TmCst 24)) (eval $ myParse "(Y$(\\ fun -> (\\ n -> if n then (n*(fun$(n+(-1)))) else 1)))$4"),
   --factorial 10
   testCase "Test 20 " $ assertEqual [] (Right (TmCst 3628800)) (eval $ myParse "(Y$(\\ fun -> (\\ n -> if n then (n*(fun$(n+(-1)))) else 1)))$10"),
   --mult of array
   testCase "Test 21 " $ assertEqual [] (Right (TmCst 3628800)) (eval $ myParse "reset ((Y$(\\ fun -> (\\ n -> (if (isNil n) then 1 else ( if (head n) then (head n)*(fun$(tail n)) else (shift q (q$0)))))))$([1,2,3,4,5,6,7,8,9,10]))"),
   --list length
   testCase "Test 22 " $ assertEqual [] (Right (TmCst 3)) (eval $ myParse "(Y$(\\ fun -> (\\ n -> if (isNil n) then 0 else (1+(fun$(tail n))))))$[1,2,3]"),
   --list sum
   testCase "Test 23 " $ assertEqual [] (Right (TmCst 6)) (eval $ myParse "(Y$(\\ fun -> (\\ n -> if (isNil n) then 0 else ((head n)+(fun$(tail n))))))$[1,2,3]")
  ]



{-
delimccTests = testGroup "delimCC tests" [evalTests, parserTests]

evalTests :: TestTree
evalTests = testGroup "Eval test"
  [
   testCase "Test 1 " $ assertEqual [] (Right (TmCst 42)) (eval (ResetShift (Var "k") "k" (ResetShift (Var "k") "k" (TmCst 42)))),
   testCase "Test 2 " $ assertEqual [] (Right (TmCst 42)) (eval (ResetShift (Var "k") "k" (TmCst 42))),
   testCase "Test 3 " $ assertEqual [] (Right (TmCst 42)) (eval (ResetShift (Var "k") "k" (App (Var "k") (TmCst 42)))),
   testCase "Test 4 " $ assertEqual [] (Right (TmCst 21)) (eval (ResetShift (Prod (TmCst 2) (Var "k")) "k" (TmCst 21))),
   testCase "Test 5 " $ assertEqual [] (Right (TmCst 21)) (eval (Add (TmCst 1) (ResetShift (Prod (TmCst 2) (Var "k")) "k" (TmCst 20)))),
   testCase "Test 6 " $ assertEqual [] (Right (TmCst 21)) (eval (Add (ResetShift (Prod (TmCst 2) (Var "k")) "k" (TmCst 20)) (TmCst 1))),
   --next demo from Wiki
   testCase "Test 7 " $ assertEqual [] (Right (TmCst 42)) (eval (Prod (TmCst 2) (ResetShift (Add (TmCst 1) (Var "k")) "k" (App (Var "k") (TmCst 20))))),
   --
   testCase "Test 8 " $ assertEqual [] (Right (TmCst 84)) (eval (Prod (TmCst 4) (ResetShift (Add (TmCst 1) (Var "k")) "k" (App (Var "k") (App (Var "k") (TmCst 19)))))),
   --new test
   testCase "Test 9 " $ assertEqual [] (Right (TmCst 42)) (eval (ResetShift (Head (Var "k")) "k" (App (Var "k") (TmList (ResetShift (Add (TmCst 1) (Var "q")) "q" (App (Var "q") (TmCst 41))) (TmList (TmCst 15) Nil)))))

  ]

parserTests :: TestTree
parserTests = testGroup "Parser test"
  [
   testCase "Test 1 " $ assertEqual [] (TmCst 42) (myParse "(reset (reset 42))"),
   testCase "Test 2 " $ assertEqual [] (ResetShift (Var "k") "k" (TmCst 42)) (myParse "(reset (shift k 42))"),
   testCase "Test 3 " $ assertEqual [] (ResetShift (Var "k") "k" (App (Var "k") (TmCst 42))) (myParse "(reset (shift k (k$42)))"),
   testCase "Test 4 " $ assertEqual [] (ResetShift (Prod (TmCst 2) (Var "k")) "k" (TmCst 21)) (myParse "(reset (2*(shift k 21)))"),
   testCase "Test 5 " $ assertEqual [] (Prod (TmCst 2) (ResetShift (Add (TmCst 1) (Var "k")) "k" (App (Var "k") (TmCst 20)))) (myParse "2*(reset (1+(shift k (k$20))))"),
   --new test
   testCase "Test 6 " $ assertEqual [] (ResetShift (Head (Var "k")) "k" (App (Var "k") (TmList (ResetShift (Add (TmCst 1) (Var "q")) "q" (App (Var "q") (TmCst 5))) (TmList (TmCst 15) Nil)))) (myParse "(reset head (shift k (k$([(reset 1+(shift q (q$5))),15]))))")
  ]

-}
{-
  [ wrap "test1" "(reset (reset 42))"               (Right 42)
  , wrap "test2" "(reset (shift k 42))"             (Right 42)
  , wrap "test3" "(reset (2*(shift k 21)))"         (Right 42)
  -- next demo from Wiki
  , wrap "test4" "2*(reset (1+(shift k (k 20))))"   (Right 42)
  ]
-}
