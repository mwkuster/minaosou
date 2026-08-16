{-# LANGUAGE OverloadedStrings #-}

module SrsSpec (spec) where

import Test.Hspec

import qualified Api
import qualified Srs

import qualified Data.Map.Strict as M
import Data.Time (UTCTime(..), fromGregorian, secondsToDiffTime)

-- | WaniKani's own stage ladder, as the API reports it: eight reviewed
-- stages, passing at Guru 1, burning at 9.
wkSystem :: Api.SrsSystem
wkSystem = Api.SrsSystem
  { Api.srsName          = "Default system"
  , Api.srsStartingStage = 1
  , Api.srsPassingStage  = 5
  , Api.srsBurningStage  = 9
  , Api.srsStageSeconds  =
      M.fromList (zip [1 ..] (map (* 3600) [4, 8, 23, 47, 167, 335, 719, 2879]))
  }

at :: UTCTime
at = UTCTime (fromGregorian 2026 8 15) (secondsToDiffTime 0)

-- | An item that has burned, with the given review count and miss counts.
burnedItem :: Int -> Int -> Int -> Int -> Bool -> (Api.Progress, Api.ReviewStat)
burnedItem sid reviews mWrong rWrong hasReading =
  ( Api.Progress (Api.SubjectId sid) 9 (Just at) (Just at)
  , Api.ReviewStat (Api.SubjectId sid) reviews mWrong rWrong hasReading
  )

-- | An item still working its way up: started, not burned.
liveItem :: Int -> Int -> Int -> Int -> Bool -> (Api.Progress, Api.ReviewStat)
liveItem sid stage reviews mWrong hasReading =
  ( Api.Progress (Api.SubjectId sid) stage (Just at) Nothing
  , Api.ReviewStat (Api.SubjectId sid) reviews mWrong 0 hasReading
  )

split :: [(Api.Progress, Api.ReviewStat)] -> ([Api.Progress], [Api.ReviewStat])
split = unzip

shouldBeNear :: Double -> Double -> Expectation
shouldBeNear actual expected =
  abs (actual - expected) < 0.01 * max 1 (abs expected)
    `shouldBe` True

spec :: Spec
spec = describe "Srs" $ do

  describe "burnedCohort" $ do
    it "counts only the items that have burned" $ do
      let (ps, rs) = split
            [ burnedItem 1 8 0 0 True
            , burnedItem 2 12 2 1 True
            , liveItem   3 4 3 1 True
            ]
          c = Srs.burnedCohort ps rs
      Srs.cohItems c   `shouldBe` 2
      Srs.cohReviews c `shouldBe` 20

    it "excludes radicals from the reading denominator" $ do
      -- WaniKani mirrors a radical's meaning counts into its reading fields
      -- with no misses, which would flatter reading accuracy if counted.
      let (ps, rs) = split [ burnedItem 1 10 0 0 False, burnedItem 2 10 0 3 True ]
          c = Srs.burnedCohort ps rs
      Srs.cohReviews c        `shouldBe` 20
      Srs.cohReadingReviews c `shouldBe` 10

    it "ignores an item with no answer record" $ do
      let ps = [ fst (burnedItem 1 8 0 0 True), fst (burnedItem 99 8 0 0 True) ]
          rs = [ snd (burnedItem 1 8 0 0 True) ]
      Srs.cohItems (Srs.burnedCohort ps rs) `shouldBe` 1

    it "reports mean and median review counts separately" $ do
      let (ps, rs) = split
            [ burnedItem 1 8 0 0 True
            , burnedItem 2 8 0 0 True
            , burnedItem 3 8 0 0 True
            , burnedItem 4 56 0 0 True
            ]
          c = Srs.burnedCohort ps rs
      Srs.cohortMeanReviews   c `shouldBe` Just 20
      Srs.cohortMedianReviews c `shouldBe` Just 8

    it "has nothing to report when nothing has burned" $ do
      let (ps, rs) = split [ liveItem 1 4 3 1 True ]
      Srs.cohortMeanReviews (Srs.burnedCohort ps rs) `shouldBe` Nothing

  describe "itemFailureBracket" $ do
    it "brackets between perfectly overlapping and wholly separate misses" $ do
      let (ps, rs) = split [ burnedItem 1 100 20 30 True ]
      Srs.itemFailureBracket (Srs.burnedCohort ps rs) `shouldBe` Just (0.3, 0.5)

    it "cannot exceed one review in one" $ do
      let (ps, rs) = split [ burnedItem 1 10 8 9 True ]
      fmap snd (Srs.itemFailureBracket (Srs.burnedCohort ps rs)) `shouldBe` Just 1

  describe "uniformModel" $ do
    it "costs one stage below the passing stage" $
      Srs.smDrops (Srs.uniformModel wkSystem 0.2 4) `shouldBe` [(3, 1)]

    it "costs two stages from the passing stage upwards" $
      Srs.smDrops (Srs.uniformModel wkSystem 0.2 6) `shouldBe` [(4, 1)]

    it "never drops below the first stage" $
      Srs.smDrops (Srs.uniformModel wkSystem 0.2 1) `shouldBe` [(1, 1)]

  describe "project" $ do
    it "costs one review per stage when nothing is ever missed" $ do
      -- The account this was built against bears this out exactly: all
      -- 1,143 items that burned without a single miss took 8 reviews.
      let pj = Srs.project wkSystem (Srs.uniformModel wkSystem 0)
      Srs.pjReviewsPerItem pj `shouldBeNear` 8

    it "takes the sum of the intervals to burn when nothing is ever missed" $ do
      let pj = Srs.project wkSystem (Srs.uniformModel wkSystem 0)
      Srs.pjDaysToBurn pj `shouldBeNear` 174.25

    -- Cross-checked against an independent solution of the same chain: a
    -- dense linear solve for the expected-visit vector, rather than the
    -- forward recursion 'Srs.project' uses.
    it "matches an independently solved chain at 10% failure" $ do
      let pj = Srs.project wkSystem (Srs.uniformModel wkSystem 0.10)
      Srs.pjReviewsPerItem pj `shouldBeNear` 10.4832
      Srs.pjDaysToBurn     pj `shouldBeNear` 203.942

    it "matches an independently solved chain at 25% failure" $ do
      let pj = Srs.project wkSystem (Srs.uniformModel wkSystem 0.25)
      Srs.pjReviewsPerItem pj `shouldBeNear` 19.0294
      Srs.pjDaysToBurn     pj `shouldBeNear` 275.395

    it "attributes the right share of the wait to the pre-passing stages" $ do
      let pj = Srs.project wkSystem (Srs.uniformModel wkSystem 0.15)
          below = sum [ d | (s, d) <- Srs.pjDays pj, s < Api.srsPassingStage wkSystem ]
      below `shouldBeNear` 5.9092

    it "costs strictly more the more often reviews are missed" $ do
      let loads = [ Srs.pjReviewsPerItem (Srs.project wkSystem (Srs.uniformModel wkSystem f))
                  | f <- [0, 0.1, 0.2, 0.3, 0.4] ]
      and (zipWith (<) loads (drop 1 loads)) `shouldBe` True

    it "stays finite even when every review is missed" $ do
      let pj = Srs.project wkSystem (Srs.uniformModel wkSystem 1.0)
      isInfinite (Srs.pjReviewsPerItem pj) `shouldBe` False

  describe "overallFailure" $
    it "reproduces a uniform rate" $ do
      let model = Srs.uniformModel wkSystem 0.18
      Srs.overallFailure (Srs.project wkSystem model) model `shouldBeNear` 0.18

  describe "fitUniformFailure" $ do
    it "recovers the rate that produced a given cost per item" $ do
      let cost = Srs.pjReviewsPerItem (Srs.project wkSystem (Srs.uniformModel wkSystem 0.22))
      case Srs.fitUniformFailure wkSystem cost of
        Nothing -> expectationFailure "expected a reachable cost"
        Just f  -> f `shouldBeNear` 0.22

    it "round-trips back to the cost it was fitted to" $
      case Srs.fitUniformFailure wkSystem 17.43 of
        Nothing -> expectationFailure "expected a reachable cost"
        Just f  ->
          Srs.pjReviewsPerItem (Srs.project wkSystem (Srs.uniformModel wkSystem f))
            `shouldBeNear` 17.43

    it "refuses a cost below what a flawless run takes" $
      Srs.fitUniformFailure wkSystem 7.5 `shouldBe` Nothing

  describe "failureScaleFor" $ do
    it "finds a scale that hits the requested cost per item" $ do
      let model = Srs.uniformModel wkSystem 0.2
      case Srs.failureScaleFor wkSystem model 12 of
        Nothing -> expectationFailure "expected a reachable target"
        Just k  ->
          Srs.pjReviewsPerItem (Srs.project wkSystem (Srs.scaleFailure k model))
            `shouldBeNear` 12

    it "refuses a target beyond what near-certain failure would cost" $ do
      let model = Srs.uniformModel wkSystem 0.2
          worst = Srs.pjReviewsPerItem (Srs.project wkSystem (Srs.scaleFailure 1000 model))
      Srs.failureScaleFor wkSystem model (2 * worst) `shouldBe` Nothing
