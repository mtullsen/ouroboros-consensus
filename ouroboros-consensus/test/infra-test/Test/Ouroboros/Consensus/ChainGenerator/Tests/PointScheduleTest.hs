{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Ouroboros.Consensus.ChainGenerator.Tests.PointScheduleTest (tests) where

import           Control.Monad.IOSim (runSimOrThrow)
import           Ouroboros.Consensus.Block.Abstract (SlotNo (SlotNo))
import           Ouroboros.Consensus.Config (SecurityParam (SecurityParam))
import qualified Test.Ouroboros.Consensus.ChainGenerator.Adversarial as A
import           Test.Ouroboros.Consensus.ChainGenerator.Params (Asc (Asc))
import           Test.Ouroboros.Consensus.ChainGenerator.Tests.Adversarial
                     (SomeTestAdversarial (..),
                     TestAdversarial (TestAdversarial, testAscA, testAscH, testRecipeA, testRecipeA'))
import           Test.Ouroboros.Consensus.ChainGenerator.Tests.GenChain
                     (genChains)
import           Test.Ouroboros.Consensus.ChainGenerator.Tests.PointSchedule
import qualified Test.QuickCheck as QC
import           Test.QuickCheck (Property)
import           Test.QuickCheck.Random (QCGen)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)
import           Test.Util.Orphans.IOLike ()

tests :: TestTree
tests = testGroup "pregenerated point schedule tests"
    [ testProperty "yoink" prop_pointSchedule
    ]

prop_pointSchedule :: SomeTestAdversarial -> QCGen -> QC.Property
prop_pointSchedule (SomeTestAdversarial _ _ TestAdversarial {testAscH, testAscA, testRecipeA, testRecipeA'}) seed =
  runSimOrThrow $ runTest (exampleTestSetup testAscH testAscA testRecipeA testRecipeA' seed)

exampleTestSetup ::
  Asc ->
  Asc ->
  A.AdversarialRecipe base hon ->
  A.SomeCheckedAdversarialRecipe base hon ->
  QCGen ->
  TestSetup
exampleTestSetup (Asc ascH) ascA recipeA recipeA' seed =
  TestSetup {
    secParam      = SecurityParam k
  , genesisWindow = GenesisWindow (SlotNo (ceiling (3 * fromIntegral k / ascH)))
  , goodSchedule
  , badSchedule
  }
  where
    k = 3
    (goodSchedule, badSchedule) = generatePointSchedule goodChain badChain
    (goodChain, badChain) = (undefined,undefined) --genChains ascA recipeA recipeA' seed

newtype GenesisWindow = GenesisWindow { getGenesisWindow :: SlotNo }
  deriving stock (Show)

data TestSetup = TestSetup {
    secParam      :: SecurityParam
  , genesisWindow :: GenesisWindow
  , goodSchedule  :: PointSchedule
  , badSchedule   :: PointSchedule
  }
  deriving stock (Show)

runTest :: TestSetup -> m Property
runTest TestSetup{} =
  undefined
