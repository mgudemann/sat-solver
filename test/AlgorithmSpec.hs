module AlgorithmSpec where
import           CDCL.Algorithm (cdcl)
import           CDCL.Types (CDCLResult (..))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Test.Hspec

import           Test.QuickCheck
import           Test.QuickCheck.Monadic

import           Data.Coerce
import qualified Picosat as PicoSAT

spec :: Spec
spec = do
    describe "Compare with picoSAT solver" $ do
        it "compare SAT / UNSAT result term reduced to empty list" $ do
          let problem = [[1, 2], [- 1]]
              csol = cdcl (map (map fromIntegral) problem) False False False
          psol <- PicoSAT.solve problem
          (case (psol, csol) of
             (PicoSAT.Solution _, SAT) -> True
             _                          -> False) `shouldBe` True
        it "compare SAT / UNSAT results" $ do
          property $ \clauses -> prop_picoSATcomparison clauses

prop_picoSATcomparison :: [[NonZero Int]] -> Property
prop_picoSATcomparison cl = withMaxSuccess 10000 ( monadicIO ( do
  let clauses = coerce cl
  picoSol <- run (PicoSAT.solve clauses)
  let cdclSol = cdcl (map (map fromIntegral) clauses) False False False
  assert ( case (picoSol, cdclSol) of
             (PicoSAT.Unsatisfiable, UNSAT) -> True
             (PicoSAT.Unknown, _)           -> False
             (PicoSAT.Solution _, SAT)      -> True
             _                              -> False )))
