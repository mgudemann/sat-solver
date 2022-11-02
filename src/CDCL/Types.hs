{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
---------------------------------------------------------------------
-- |
-- Module      :   CDCL.Types
-- Description :   Contains type declaration for CDCL Package
-- Copyright   :   (c) Thanh Nam Pham, 2021
-- License     :   Apache-2.0
-- Maintainer  :
-- Stability   :
-- Portability :
--
---------------------------------------------------------------------
module CDCL.Types where

import qualified Data.Map.Strict as Map

import           Data.Set (Set)
import qualified Data.Set as Set (empty, insert)

-- | SATResult
data SATResult = Satisfiable [Int]
               | Unsatisfiable

-- | Datatype for CDCL
data CDCLResult
    =   SAT -- ^ Satisfiable, no information about solution or statistics
    |
        -- | Formula resolved, with TupleList to show how it was solved
        SAT_SOLUTION TupleList
    |
        -- | Formula resolved. Followed with TupleList, #Decisions, #LearnedClauses, #Restarts
        SAT_WITH_STATS TupleList Integer Integer Integer Integer
    |
        -- | Formula resolved. Followed with TupleList, MappedTupleList, LearnedClauses, #Decisions, #Learned Clauses, #Restarts
        SAT_WITH_FULL_STATS TupleList MappedTupleList [Clause] Integer Integer Integer Integer
    |
        -- | Formula not resolved
        UNSAT
    |
        UNSAT_WITH_STATS [Clause] [Clause]
    deriving(Eq, Ord)

instance Show CDCLResult where
    show SAT = "Result:\nSAT"

    show (SAT_SOLUTION tl) = "Result:\nSAT " ++ show tl

    show (SAT_WITH_STATS tl decisions decisionLvl learned restarts) = "Result:\nSAT " ++ show tl ++ "\nStatistics:" ++ "\nDecisions:"
     ++ show decisions ++ "\nAmount of learned Clauses: " ++ show learned ++ "\nHighest Decisionlevel: " ++ show decisionLvl
     ++ "\nAmount of Restarts: " ++ show restarts

    show (SAT_WITH_FULL_STATS tl mtl learned decisions decisionLvl int1 restarts) = "Result:\nSAT " ++ show tl ++ "\n\nStatistics:" ++
     "\n\nDecisions:\n" ++ show mtl ++ "\nLearned Clauses: " ++ show learned ++ "\n\nAmount of Decisions: " ++ show decisions ++
     "\nHighest Decisionlevel: " ++ show decisionLvl ++ "\nAmount of learned Clauses: "  ++ show int1 ++ "\nAmount of Restarts: " ++ show restarts

    show UNSAT = "UNSAT"

    show (UNSAT_WITH_STATS cl conf) = "UNSAT. Learned Clauses: \n" ++ show cl  ++ "\nClauses which caused conflict:\n" ++ show conf

-- | Datatype for Reason
--   Shows if it was a decision or if the set Literal has a clause as Reason
--   for the set BoolVal
data Reason =
        -- | The algorithm decided that the Literal will have the value it has
        Decision
    |
        -- | The algorithm calculated the BoolVal based on the other set BoolVal
        Reason Clause
    deriving (Show, Eq, Ord)

-- | Datatyp for BoolVal
--   Is used to show which value the set Literal has
data BoolVal =
        -- | Valuewise 0
        BFalse
    |
        -- | Valuewise 1
        BTrue
    |
        -- | Valuewise -1
        BNothing
    deriving (Show, Eq, Ord)

-- | Datatype for InterpResult
data InterpretResult =
        -- | Valuewise 1. Clause is solved
        OK
    |
        -- | Valuewise 0. The conflict clause will be shown in Clause
        NOK Clause
    |
        -- | Valuewise -1. A Literal isn't set, which can solve the clause.
        UNRESOLVED
    deriving (Show, Eq, Ord)

-- | Literal defined as Integer
newtype Literal = Lit Integer
    deriving (Show, Eq, Ord)

-- | Level is associated with the decision level.
--   Defined as an Integer
newtype Level = Level Integer
    deriving (Show, Eq, Ord)

-- | Activity defined as Integer
newtype Activity = Activity Integer
    deriving (Show, Eq, Ord)

-- | Period. If Period is 0 half the current activitymap
newtype Period = Period Integer
    deriving (Eq)

-- | Clause defined as a List of Literals
type Clause = Set Literal

-- | Tuple of 2 Clauses
--   First clause in tuple is reduced via Unitresolution
--   Second clause is the clause in its original form
type ReducedClauseAndOGClause = (Clause, Clause)

-- | ClauseList defined as a List of ReducedClauseAndOGClause
type ClauseList = [ReducedClauseAndOGClause]

-- | Tuple is defined as a Tuple of (Literals, Integer).
--   Integers in this case are only 0 or 1 valuewise.
type Tuple = (Literal, BoolVal)

-- | List containing Tupels. Only used to show the result.
type TupleList = [Tuple]

-- | A tuple of Tuple and Reason
type TupleClause = (Tuple, Reason)

-- | TupleClauseList is a list of TupleClause
type TupleClauseList = [TupleClause]

-- | Defined as Map.Map Integer TupleList
type MappedTupleList = Map.Map Level TupleClauseList

-- | Shows how often a Literal is found in the formulas
-- | Defined as Map.Map Literal Activity
type ActivityMap = Map.Map Literal Activity

-- | Is a single Tuple containing the Literal and activty
--   Defined as (Literal, Activty)
type LiteralActivity = (Literal, Activity)

-- | Defined by using three Types.
--   These are ClauseList, TupelClauseList and MappedTupleList
type TriTuple = (ClauseList , TupleClauseList, MappedTupleList)

-- | Increase the given level by one
increaseLvl :: Level -> Level
increaseLvl (Level i) = Level (i + 1)

-- | Decrease the given level by one
decreaseLvl :: Level -> Level
decreaseLvl (Level i) = Level (i - 1)

-- | Get the current level
getLevel :: Level -> Integer
getLevel (Level i) = i

-- | Get the Integervalue of the given Literal
getLiteralValue :: Literal -> Integer
getLiteralValue (Lit x) = x

-- | Multiply the given Integervalue with -1
negateLiteralValue :: Literal -> Literal
negateLiteralValue (Lit x) = Lit (-x)

-- | Get the Integervalue of the given Activity
getActivityValue :: Activity -> Integer
getActivityValue (Activity i) = i

-- | Increase the Activity by one
increaseActivity :: Activity -> Activity
increaseActivity (Activity i) = Activity (i + 1)

-- | Divide the activity by 2. Round the Value down
divideActivity :: Activity -> Activity
divideActivity (Activity i) = Activity (i `div` 2)

-- | Transforms a given List of Integerlists into a ClauseList.
transformClauseList :: [[Integer]] -> ClauseList
transformClauseList [] = []
transformClauseList (xs : ys)
    | null ys = [transformClause xs Set.empty]
    | otherwise = transformClause xs Set.empty : transformClauseList ys
-- | Transforms a list of Integers into a ReducedClauseAndOGClause
transformClause :: [Integer] -> Clause -> ReducedClauseAndOGClause
transformClause (xs : ys) varList
    | null ys = (Set.insert (Lit xs)  varList, Set.insert (Lit xs)  varList)
    | otherwise = transformClause ys (Set.insert (Lit xs) varList)
-- | Checks if Interpretresult contains NOK.
--   Return true if it does, else false
getNOK :: InterpretResult -> Bool
getNOK (NOK _) = True
getNOK _ = False

-- | Returns the Clause which caused the NOK in InterpretResult
getEmptyClause :: InterpretResult -> Clause
getEmptyClause (NOK x) = x

-- | Returns the Clause in Reason
getReason :: Reason -> Clause
getReason (Reason r) = r

-- | Decrease a given Period by 1
decreasePeriod :: Period -> Period
decreasePeriod (Period r) = Period (r - 1)

-- | Returns the Reduced Clause from the ClauseTuple
getClauseFromReducedClauseAndOGClause :: ReducedClauseAndOGClause -> Clause
getClauseFromReducedClauseAndOGClause (x, _) = x

-- | Returns the OG State of the Clause from the ClauseTuple
getOGFromReducedClauseAndOGClause :: ReducedClauseAndOGClause -> Clause
getOGFromReducedClauseAndOGClause (_, x) = x
