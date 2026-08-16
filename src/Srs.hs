-- | Projecting a review workload from measured behaviour.
--
-- An item walks the SRS stages as a Markov chain: each review either
-- advances it one stage or knocks it back, and it leaves the chain for good
-- when it burns. Everything reported here follows from a single quantity --
-- the expected number of reviews an item spends at each stage on its way
-- from lesson to burned -- so that is what 'visitsPerStage' computes, and
-- the rest is bookkeeping on top of it.
--
-- The numbers fed into the chain are the user's own. The stage ladder and
-- its intervals come from the API ('Api.SrsSystem'), and the failure rate
-- is fitted ('fitUniformFailure') so the chain reproduces what the account
-- actually cost: the mean number of reviews its burned items really took
-- ('burnedCohort'). A projection therefore describes the person asking
-- rather than an average learner.
module Srs
  ( -- * Measuring
    Cohort(..)
  , burnedCohort
  , cohortBy
  , cohortMeanReviews
  , cohortMedianReviews
  , itemFailureBracket
    -- * The chain
  , StageModel(..)
  , uniformModel
  , scaleFailure
  , Projection(..)
  , project
  , overallFailure
  , failureScaleFor
  , fitUniformFailure
  ) where

import qualified Api
import Util (median)

import Data.List (foldl', sort)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust, mapMaybe)

--------------------------------------------------------------------------------
-- Measuring
--------------------------------------------------------------------------------

-- | The items that have completed the whole ladder, and what they cost.
--
-- Burned items are the only cohort whose review count is finished rather
-- than still running, so they are the one place the true price of an item
-- can be read off directly instead of modelled. They are not a perfectly
-- neutral sample -- they skew towards earlier, easier levels, simply
-- because those have had the time to burn -- but they are measured fact,
-- which no amount of modelling can substitute for.
data Cohort = Cohort
  { cohItems          :: Int
  , cohReviewsPerItem :: [Int]
    -- ^ Reviews taken by each item, lesson to burned.
  , cohReviews        :: Int
  , cohMeaningWrong   :: Int
  , cohReadingWrong   :: Int
  , cohReadingReviews :: Int
    -- ^ Reviews belonging to items that are actually asked for a reading,
    -- which is the right denominator for reading accuracy (radicals never
    -- are -- see 'Api.rsHasReading').
  } deriving (Show, Eq)

burnedCohort :: [Api.Progress] -> [Api.ReviewStat] -> Cohort
burnedCohort = cohortBy (isJust . Api.pgBurnedAt)

-- | The answer record of whichever items a predicate selects, joined onto
-- their progress. Splitting by burned-or-not is what makes the skew
-- visible: an item that has burned was learned long ago, at a level that
-- was easier than the one being studied now.
cohortBy :: (Api.Progress -> Bool) -> [Api.Progress] -> [Api.ReviewStat] -> Cohort
cohortBy keep progress stats = foldl' add empty selected
  where
    byId = M.fromList [ (Api.rsSubjectId s, s) | s <- stats ]
    selected = mapMaybe
      (\p -> if keep p then M.lookup (Api.pgSubjectId p) byId else Nothing)
      progress

    empty = Cohort 0 [] 0 0 0 0
    add c s = Cohort
      { cohItems          = cohItems c + 1
      , cohReviewsPerItem = Api.rsReviews s : cohReviewsPerItem c
      , cohReviews        = cohReviews c + Api.rsReviews s
      , cohMeaningWrong   = cohMeaningWrong c + Api.rsMeaningWrong s
      , cohReadingWrong   = cohReadingWrong c + Api.rsReadingWrong s
      , cohReadingReviews = cohReadingReviews c
                              + (if Api.rsHasReading s then Api.rsReviews s else 0)
      }

-- | Mean reviews an item took from lesson to burned -- the number a daily
-- workload is built from, since the load is the lesson rate times this.
-- The mean, not the median: a handful of leeches taken twenty times over
-- are real reviews that really have to be sat.
cohortMeanReviews :: Cohort -> Maybe Double
cohortMeanReviews c
  | cohItems c == 0 = Nothing
  | otherwise       = Just (fromIntegral (cohReviews c) / fromIntegral (cohItems c))

-- | The typical item, for contrast with the mean. A gap between the two is
-- the leech tail, and worth seeing.
cohortMedianReviews :: Cohort -> Maybe Double
cohortMedianReviews = median . map fromIntegral . sort . cohReviewsPerItem

-- | Bounds on the share of reviews that were answered wrong.
--
-- Only per-question miss counts are recorded, never "this review went
-- wrong", so the item-level rate the SRS actually responds to has to be
-- bracketed. The low end assumes every reading miss fell on a review whose
-- meaning was missed too; the high end assumes no miss ever coincided with
-- another, and that no question was ever missed twice in one review.
itemFailureBracket :: Cohort -> Maybe (Double, Double)
itemFailureBracket c
  | cohReviews c == 0 = Nothing
  | otherwise         = Just (rate (max m r), rate (m + r))
  where
    m = cohMeaningWrong c
    r = cohReadingWrong c
    rate x = min 1 (fromIntegral x / fromIntegral (cohReviews c))

--------------------------------------------------------------------------------
-- The chain
--------------------------------------------------------------------------------

-- | How one stage behaves: the chance a review there fails to advance the
-- item, and where it lands when that happens (a distribution over stages,
-- conditional on having failed, so the weights sum to 1).
data StageModel = StageModel
  { smFail  :: Double
  , smDrops :: [(Int, Double)]
  } deriving (Show, Eq)

-- | A failure rate of exactly 1 would mean an item can never leave the
-- stage and its expected review count is infinite. Cap it just short, so a
-- pathological input yields a very large number rather than a division by
-- zero.
maxFail :: Double
maxFail = 0.995

clampFail :: Double -> Double
clampFail = max 0 . min maxFail

-- | The same failure rate at every stage, with WaniKani's documented
-- penalty: a miss costs one stage before the passing stage and two from it
-- on, never falling below the first.
--
-- This penalty is the one thing here that the API does not expose, so it is
-- written down rather than measured. It matters less than it looks: it only
-- fixes the /exchange rate/ between a failure rate and a review count, and
-- since 'fitUniformFailure' calibrates the rate against the review count
-- the account actually paid, an error in the penalty is largely absorbed by
-- the fit.
--
-- Nor is a uniform rate a claim that every stage is equally hard -- misses
-- really do cluster in the early stages. It is the most a per-subject
-- answer record can support: WaniKani reports lifetime totals per item, not
-- per stage, so there is nothing to fit a per-stage profile to.
uniformModel :: Api.SrsSystem -> Double -> Int -> StageModel
uniformModel sys f s = StageModel (clampFail f) [(landing, 1)]
  where
    penalty = if s >= Api.srsPassingStage sys then 2 else 1
    landing = max (Api.srsStartingStage sys) (s - penalty)

-- | Multiply every stage's failure rate by the same factor.
scaleFailure :: Double -> (Int -> StageModel) -> Int -> StageModel
scaleFailure k model s =
  let m = model s
  in m { smFail = clampFail (k * smFail m) }

data Projection = Projection
  { pjVisits         :: [(Int, Double)]
    -- ^ Expected reviews at each stage, per item, from lesson to burned.
  , pjDays           :: [(Int, Double)]
    -- ^ Expected days spent at each stage, per item.
  , pjReviewsPerItem :: Double
  , pjDaysToBurn     :: Double
  } deriving (Show, Eq)

-- | Expected number of reviews an item spends at each stage before burning.
--
-- Let @W s@ be the vector of visits accumulated while climbing from stage
-- @s@ to @s+1@ for the first time. One review at @s@ always happens; with
-- probability @fail s@ the item instead lands at some @d <= s@ and has to
-- re-climb @d -> s@ before trying again:
--
-- > W s = (unit s + sum over d of  fail s * weight d * (W d + ... + W (s-1))) / (1 - fail s)
--
-- Because a miss never moves an item /forwards/, the right-hand side only
-- mentions stages below @s@, so this is a plain forward recursion -- no
-- linear system to solve. Total visits are the sum of the @W s@.
visitsPerStage :: Int -> Int -> (Int -> StageModel) -> [(Int, Double)]
visitsPerStage lo hi model
  | hi < lo   = []
  | otherwise = zip [lo .. hi] (foldl' (zipWith (+)) zeros climbs)
  where
    zeros  = replicate (hi - lo + 1) 0
    unit s = [ if j == s then 1 else 0 | j <- [lo .. hi] ]

    -- Built lowest stage first; 'prefixes' carries the running sums
    -- @W lo + ... + W s@, so the inner @W d + ... + W (s-1)@ is one
    -- subtraction rather than a re-summation.
    (climbs, prefixes) = foldl' step ([], []) [lo .. hi]

    prefixAt k
      | k < lo    = zeros
      | otherwise = prefixes !! (k - lo)

    step (ws, ps) s =
      let m    = model s
          pass = max (1 - maxFail) (1 - smFail m)
          back = foldl' (zipWith (+)) zeros
                   [ map (* (smFail m * wt)) (zipWith (-) (prefixAt (s - 1)) (prefixAt (d - 1)))
                   | (d, wt) <- smDrops m
                   ]
          w    = map (/ pass) (zipWith (+) (unit s) back)
      in (ws ++ [w], ps ++ [zipWith (+) (prefixAt (s - 1)) w])

project :: Api.SrsSystem -> (Int -> StageModel) -> Projection
project sys model = Projection
  { pjVisits         = visits
  , pjDays           = days
  , pjReviewsPerItem = sum (map snd visits)
  , pjDaysToBurn     = sum (map snd days)
  }
  where
    lo     = Api.srsStartingStage sys
    hi     = Api.srsBurningStage sys - 1
    visits = visitsPerStage lo hi model
    days   = [ (s, v * stageDays s) | (s, v) <- visits ]
    stageDays s =
      fromIntegral (fromMaybe 0 (M.lookup s (Api.srsStageSeconds sys))) / 86400

-- | The failure rate a projection implies overall: each stage's rate
-- weighted by how often an item is reviewed there. Unlike a plain average
-- over stages, this does not let rarely-visited stages dominate.
overallFailure :: Projection -> (Int -> StageModel) -> Double
overallFailure pj model
  | total <= 0 = 0
  | otherwise  = sum [ v * smFail (model s) | (s, v) <- pjVisits pj ] / total
  where total = sum (map snd (pjVisits pj))

-- | The factor every stage's failure rate would have to be multiplied by
-- for an item to cost exactly @target@ reviews between lesson and burn.
--
-- Reviews per item increases monotonically with the factor, so a bisection
-- suffices. 'Nothing' means the target is out of reach: below the number of
-- stages -- what a flawless run costs -- no factor can help, and beyond
-- what near-certain failure costs there is nothing left to give.
failureScaleFor :: Api.SrsSystem -> (Int -> StageModel) -> Double -> Maybe Double
failureScaleFor sys model target
  | target <= at 0     = Nothing
  | target >= at upper = Nothing
  | otherwise          = Just (bisect 0 upper (60 :: Int))
  where
    upper = 1000
    at k  = pjReviewsPerItem (project sys (scaleFailure k model))
    bisect lo hi 0 = (lo + hi) / 2
    bisect lo hi n =
      let mid = (lo + hi) / 2
      in if at mid < target then bisect mid hi (n - 1) else bisect lo mid (n - 1)

-- | The per-review failure rate that would make an item cost @target@
-- reviews from lesson to burned -- i.e. the rate implied by what the
-- account actually paid, rather than one inferred from answer counts.
fitUniformFailure :: Api.SrsSystem -> Double -> Maybe Double
fitUniformFailure sys = failureScaleFor sys (uniformModel sys 1)
