import Test.HUnit

import Data.KindInfer.BasicType
import TypeSystem.KindInfer

main :: IO ()
main = do
  runTestTT adtPosTest
  return ()

group :: String -> [Test] -> Test
group label = TestLabel label . TestList

-- ADT Positive Test
adtPosTest :: Test
adtPosTest = group "ADT Positive Test" [
    boolKind   ~=? boolKindInfered
  , maybeKind  ~=? maybeKindInfered
  , eitherKind ~=? eitherKindInfered
  , fixKind    ~=? fixKindInfered
  ] where
    inferKind t = case runKindInference [] [t] of
      Right [k] -> k
      _         -> error "impossible"

    -- data Bool = True | False
    -- Bool :: *
    boolKind = Star
    boolKindInfered = inferKind $ ADT "Bool" [] [[], []]

    -- data Maybe a = Just a | Nothing
    -- Maybe :: * -> *
    maybeKind = Star `Arrow` Star
    maybeKindInfered = inferKind $ ADT "Maybe" ["a"] [[Var "a"], []]

    -- data Either a b = Left a | Right b
    -- Either :: * -> * -> *
    eitherKind = Star `Arrow` (Star `Arrow` Star)
    eitherKindInfered = inferKind $ ADT
      "Either"
      ["a", "b"]
      [[Var "a"], [Var "b"]]

    -- data Fix a = In (Fix (a (Fix a)))
    -- Fix :: (* -> *) -> *
    fixKind = (Star `Arrow` Star) `Arrow` Star
    fixKindInfered = inferKind $ ADT
      "Fix"
      ["a"]
      [[(App (Lit "Fix")
             (App (Var "a")
                  (App (Lit "Fix") (Var "a"))))]]
