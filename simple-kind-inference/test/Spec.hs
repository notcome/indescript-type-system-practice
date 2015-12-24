import Test.HUnit

import Data.KindInfer.BasicType
import TypeSystem.KindInfer

main :: IO ()
main = do
  runTestTT kindPprTest
  runTestTT adtPosTest
  runTestTT adtNegTest
  return ()

group :: String -> [Test] -> Test
group label = TestLabel label . TestList

-- Kind Prettyprint Test
kindPprTest :: Test
kindPprTest = group "Kind Prettyprint Test" [
    show star  ~=? "*"
  , show s2s   ~=? "* -> *"
  , show s_s2s ~=? "* -> * -> *"
  , show s2s_s ~=? "(* -> *) -> *"
  ] where
    (==>) = Arrow
    star  = Star
    s2s   = star ==> star
    s_s2s = star ==> s2s
    s2s_s = s2s  ==> star

-- ADT Positive Test
adtPosTest :: Test
adtPosTest = group "ADT Positive Test" [
    boolKind   ~=? boolKindInfered
  , maybeKind  ~=? maybeKindInfered
  , eitherKind ~=? eitherKindInfered
  , fixKind    ~=? fixKindInfered
  ] where
    inferKind t = case runKindInference [] [t] of
      Right [k] -> show k
      Left msg  -> msg
      _         -> error "impossible"

    -- data Bool = True | False
    -- Bool :: *
    boolKind = "*"
    boolKindInfered = inferKind $ ADT "Bool" [] [[], []]

    -- data Maybe a = Just a | Nothing
    -- Maybe :: * -> *
    maybeKind = "* -> *"
    maybeKindInfered = inferKind $ ADT "Maybe" ["a"] [[Var "a"], []]

    -- data Either a b = Left a | Right b
    -- Either :: * -> * -> *
    eitherKind = "* -> * -> *"
    eitherKindInfered = inferKind $ ADT
      "Either"
      ["a", "b"]
      [[Var "a"], [Var "b"]]

    -- data Fix a = In (a (Fix a))
    -- Fix :: (* -> *) -> *
    fixKind = "(* -> *) -> *"
    fixKindInfered = inferKind $ ADT
      "Fix"
      ["a"]
      [[(App (Var "a") (App (Lit "Fix") (Var "a")))]]

-- ADT Negative Test
adtNegTest :: Test
adtNegTest = group "ADT Negative Test" [
    wrongFixMsg ~=? wrongFixRes
  ] where
    inferKind t = case runKindInference [] [t] of
      Right [k] -> error "Should fail, but kind infered: " ++ show k
      Left  msg -> msg
      _         -> error "impossible"

    -- data Fix a = In (Fix (a (Fix a)))
    wrongFixMsg = "Cannot unify kinds, cycle detected."
    wrongFixRes = inferKind $ ADT "Fix" ["a"]
      [[(App (Var "Fix")
             (App (Var "a")
                  (App (Lit "Fix") (Var "a"))))]]
