module DPLLSpec where
import           Test.Hspec
import           CDCL.DPLL (rmPureVars)

spec :: Spec
spec = do
    describe "rmPureVars" $ do
        it "Empty List" $ do
            rmPureVars [] `shouldBe` ([], [])
        it "Singleton List" $ do
            rmPureVars [[]] `shouldBe` ([[]], [])
        it "Filled List 1" $ do
            rmPureVars [[1,2,3],[1]] `shouldBe` ([], [1,2,3])
        it "Filled List 2" $ do
            rmPureVars [[1,2,3],[-1]] `shouldBe` ([], [2,3,-1])
        it "Filled List 3" $ do
            rmPureVars [[1,2,3],[-1],[1]] `shouldBe` ([[-1],[1]], [2,3])
