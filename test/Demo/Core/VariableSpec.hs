module Demo.Core.VariableSpec (spec) where

import Data.Aeson (Value (..))
import Data.IntMap.Strict qualified as IM
import Data.Text qualified as T
import Demo.Core.Types (VarStore (..), insertVar, emptyVarStore)
import Demo.Core.Variable
  ( VarRef (..)
  , findVarReferences
  , substituteVars
  , substituteVarsEither
  )
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "findVarReferences" $ do
    it "finds no references in empty text" $
      findVarReferences "" `shouldBe` []

    it "finds no references in text without variables" $
      findVarReferences "hello world" `shouldBe` []

    it "finds single variable reference" $
      findVarReferences "echo $v1" `shouldBe` [VarRef 1 "$v1"]

    it "finds multiple variable references" $
      findVarReferences "echo $v1 $v2 $v3"
        `shouldBe` [VarRef 1 "$v1", VarRef 2 "$v2", VarRef 3 "$v3"]

    it "finds variable in middle of text" $
      findVarReferences "prefix $v42 suffix" `shouldBe` [VarRef 42 "$v42"]

    it "handles multi-digit indices" $
      findVarReferences "$v123" `shouldBe` [VarRef 123 "$v123"]

  describe "substituteVars" $ do
    it "returns text unchanged when no variables" $
      let store = emptyVarStore
       in substituteVars store "hello world" `shouldBe` "hello world"

    it "substitutes single string variable" $
      let store = insertVar 1 (String "foo") emptyVarStore
       in substituteVars store "echo $v1" `shouldBe` "echo foo"

    it "substitutes single number variable" $
      let store = insertVar 1 (Number 42) emptyVarStore
       in substituteVars store "echo $v1" `shouldBe` "echo 42"

    it "substitutes multiple variables" $
      let store =
            insertVar 1 (String "hello") $
              insertVar 2 (String "world") emptyVarStore
       in substituteVars store "$v1 $v2" `shouldBe` "hello world"

    it "substitutes array variable as JSON" $
      let store = insertVar 1 (Array mempty) emptyVarStore
       in substituteVars store "arr: $v1" `shouldBe` "arr: []"

  describe "substituteVarsEither" $ do
    it "returns Left for missing variable" $
      let store = emptyVarStore
       in substituteVarsEither store "echo $v1" `shouldBe` Left 1

    it "returns Right for present variable" $
      let store = insertVar 1 (String "test") emptyVarStore
       in substituteVarsEither store "echo $v1" `shouldBe` Right "echo test"

    it "returns Left for first missing variable" $
      let store = insertVar 1 (String "x") emptyVarStore
       in substituteVarsEither store "$v1 $v2" `shouldBe` Left 2

  describe "property tests" $ do
    it "substitution doesn't change text without variables" $
      property $ \(NonEmpty s) ->
        let txt = T.pack (filter (/= '$') s)
            store = emptyVarStore
         in substituteVars store txt == txt

    it "number of references equals number of $v patterns" $
      property $ \n ->
        n >= 1 && n <= 100 ==>
          let txt = T.intercalate " " ["$v" <> T.pack (show i) | i <- [1 .. n]]
           in length (findVarReferences txt) == n
