{-# LANGUAGE Safe #-}

module WojciechJarzabekTests(tests) where

import DataTypes

tests :: [Test]
tests =
  [ Test "inc"      (SrcString "input x in x + 1") (Eval [42] (Value 43))
  , Test "undefVar" (SrcString "x")                TypeError
  , Test "noVars"   (SrcString "2 + 2")            (Eval []   (Value 4))
  , Test "unused"   (SrcString "input x in 2 + 2") (Eval [42] (Value 4))
  , Test "waitWhat" (SrcString "input x in let x = x in x")
                                                   (Eval [1]  (Value 1))
  , Test "ifTest"   (SrcString "input x in if x > 0 then 1 else 0")
                                                   (Eval [10] (Value 1))
  , Test "op1"      (SrcString "4 * 2 + 3")        (Eval []   (Value 11))
  , Test "op2"      (SrcString "2 + 4 div 3")      (Eval []   (Value 3))
  , Test "op3"      
         (SrcString "if false and false or true then 1 else 0")
                                                   (Eval []   (Value 1))
  , Test "op4"      
         (SrcString "if false and true or false then 1 else 0")
                                                   (Eval []   (Value 0))
  , Test "mod"   (SrcString "input x in -x mod 4") (Eval [3]  (Value 1))
  , Test "div"   (SrcString "input x in -x div 4") (Eval [3]  (Value (-1)))
  , Test "not"   (SrcString "if not false then 1 else 0")
                                                   (Eval []   (Value 1))
  , Test "inv1" (SrcString "if 1 then 1 else 0")   TypeError
  , Test "inv2" (SrcString "input x in y")         TypeError
  , Test "inv3" (SrcString "not 42")               TypeError
  , Test "inv4" (SrcString "9000 * true")          TypeError
  , Test "inv5" (SrcString "true")                 TypeError
  , Test "multipleVars"
         (SrcString "input x y z in x + y - z")    (Eval [1,2,3] (Value 0))
  , Test "naming" (SrcString "input _d'oh_ in 1")  (Eval [1] (Value 1))
  , Test "zerodiv1" 
         (SrcString "if true then 1 else 1 div 0") (Eval [] (Value 1))
  , Test "zerodiv2" 
         (SrcString "if false then 1 else 1 div 0")(Eval [] RuntimeError)    
  , Test "fstTypeError" (SrcString "fst (true, 0)")TypeError
  , Test "sndTypeError" (SrcString "snd (0, true)")TypeError
  , Test "returnLst" (SrcString "[]:int")          TypeError
  , Test "exprMatch" (SrcFile "test1.pp6")         (Eval [] (Value 1))
  , Test "funcTest"  (SrcFile "test2.pp6")         (Eval [0] (Value 0))
  , Test "recursionTest"  (SrcFile "test2.pp6")    (Eval [15] (Value 610))
  , Test "pairAsArg" (SrcFile "test3.pp6")         (Eval [2] (Value 4))
  , Test "unitTest"  (SrcFile "test4.pp6")         (Eval [3] (Value 6))
  , Test "lazyIf" 
         (SrcString "if true then 1 else []:int")  TypeError
  , Test "mindwrecking1" 
         (SrcString "input x in x + (let x = true in if x then 1 else 2)")
                                                   (Eval [1] (Value 2))
  , Test "mindwrecking2" (SrcFile "test5.pp6")     (Eval [1] (Value 4))
  , Test "ifInFun" (SrcFile "test6.pp6")           (Eval [15] (Value 610))
  , Test "localVar1" (SrcFile "test7.pp6")         (Eval [] (Value 45))
  , Test "localVar2" (SrcFile "test8.pp6")         (Eval [] (Value 6))
  , Test "matchLambda" (SrcFile "test9.pp6")       (Eval [] (Value 9))
  , Test "globalLocal" (SrcFile "test10.pp6")      (Eval [2] (Value 4))
  , Test "simpleLambda" (SrcFile "test11.pp6")     (Eval [] (Value 42))
  , Test "oldTest" (SrcFile "test12.pp6")          TypeError
  , Test "simpleLm" 
         (SrcString "input x in let f = fn(x:int) -> x in f x")
                                                   (Eval [42] (Value 42))
  , Test "inception" (SrcFile "test13.pp6") (Eval [42] (Value 42))  
  , Test "compositeLambda" (SrcFile "test14.pp6")  (Eval [24,18] (Value 42))  
  , Test "noReturn" (SrcString "fn(a:unit) -> 42") TypeError
  , Test "boolLambda" (SrcFile "test15.pp6")       TypeError
  ]