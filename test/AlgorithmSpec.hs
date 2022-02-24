module AlgorithmSpec where
import           CDCL.Algorithm 
                    ( solve
                    , cdcl)
import           CDCL.Types
                    ( SATResult (..)
                    , CDCLResult (..))
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
    describe "Pure reduction should provide a variable assignment" $ do
        it "[[1, 2], [- 1]] is SAT with assignment" $ do
            let problem = [[1, 2], [- 1]]
                solution = solve problem

                resultAssignment = (case solution of
                  Satisfiable list -> list
                  _                -> error "Unsatisfiable but expected (Satisfiable [2, -1])")
            resultAssignment`shouldBe` [2, -1]
        it "[[1, 2, 3], [- 1, - 2], [1]] is SAT with assignment" $ do
            let problem = [[1, 2], [- 1]]
                solution = solve problem

                resultAssignment = (case solution of
                  Satisfiable list -> list
                  _                -> error "Unsatisfiable but expected (Satisfiable [3, -2, 1])")
            resultAssignment`shouldBe` [2, -1]

prop_picoSATcomparison :: [[NonZero Int]] -> Property
prop_picoSATcomparison cl = withMaxSuccess 10000 ( monadicIO ( do
  let clauses = coerce cl
  picoSol <- run (PicoSAT.solve clauses)
  let cdclSol = cdcl (map (map fromIntegral) clauses) False False False
  ( case (picoSol, cdclSol) of
      (PicoSAT.Unsatisfiable, UNSAT) -> pure $ collect "UNSAT"   True
      (PicoSAT.Unknown, _)           -> pure $ collect "Unknown" False
      (PicoSAT.Solution _, SAT)      -> pure $ collect "SAT"     True
      _                              -> pure $ collect "no Idea" False )))
