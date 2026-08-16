# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build and Run

```bash
cabal build
cabal run minaosou            # starts a study session (default)
cabal run minaosou -- --help
cabal run minaosou -- whoami
cabal run minaosou -- reviews
cabal run minaosou -- study --batch-size 5
cabal run minaosou -- forecast --lessons 10 --reviews-per-day 100
```

Token resolution order: `--token` flag → `WANIKANI_API_TOKEN` env var → `~/.config/minaosou/config`

Config file format (`~/.config/minaosou/config`):
```
token=<your-api-token>
batch_size=10
requeue_after=7
audio_player=mpv --really-quiet
```

`audio_player` is optional. If set, Ctrl-p plays the WaniKani pronunciation audio during reading questions (vocabulary only). The URL is appended as the last argument to the command.

Run tests: `cabal test`

## Architecture

This is a WaniKani (kanji/vocabulary SRS) CLI+TUI app. The study flow:
1. `Main.hs` parses args, loads config, resolves token
2. For `study`: fetch available assignments from API → fetch subject details → run interactive TUI → optionally submit results back to WaniKani

### Modules

- **`Api.hs`** — WaniKani REST API client (`req` library). Fetches users, summaries, assignments, subjects; submits reviews. Subjects are batch-fetched in chunks of 100.
- **`Cli.hs`** — `optparse-applicative` command/option definitions (`WhoAmI`, `Reviews`, `Study`, `Leeches`, `Forecast`, `Init`).
- **`Config.hs`** — Simple key=value config file parser for `~/.config/minaosou/config`.
- **`ForecastCache.hs`** — Local cache of the two bulk collections `forecast` reads, with `updated_after` incremental refresh. See "Forecasting" below.
- **`Srs.hs`** — Pure workload model behind `minaosou forecast`: the SRS ladder as an absorbing Markov chain. See "Forecasting" below.
- **`Romaji.hs`** — Romaji→hiragana converter (longest-match-first lookup table). Handles consonant doubling, palatalized sounds, etc. Used by `Tui.normReading`.
- **`Tui.hs`** — `brick`-based interactive study session. Manages a queue of questions (`QMeaning`/`QReading`), tracks per-subject progress, and produces `Submission` records. Modes: Normal → WrongAnswer → Feedback → ConfirmSubmit → Finished.

### TUI Keybindings
- `Enter` — submit answer
- `Ctrl-o` — override as correct
- `Ctrl-r` — requeue question later (no penalty)
- `Ctrl-a` — show all info overlay (level, SRS stage, components, meanings, readings, mnemonics; for kanji subjects also lists vocabulary that uses the kanji and visually similar kanji; for vocab subjects also shows the readings and radical composition of each component kanji); ↑↓/j/k to scroll, Ctrl-a/Esc/Enter to close
- `Ctrl-u` — show user info overlay (username, level, profile URL)
- `Ctrl-v` — show review schedule overlay (next 24h); ↑↓/j/k to scroll, Ctrl-v/Esc to close
- `Ctrl-p` — play pronunciation audio (vocabulary, requires `audio_player` in config)
- `Ctrl-s` — submit batch to WaniKani
- `Esc`/`Ctrl-q` — quit

### Session timer

The top-right corner shows elapsed session time (`⏱ MM:SS`, `H:MM:SS` past the hour). A background thread in `Tui.withTicker` posts a `Tick` event once a second, which only updates `stClock`; `stSessionStart` is the wall clock at TUI start. The timer is a row in the layout, not a `Brick` layer — a layer over the corner covers the box border underneath it.

The clock **stops** when the last item leaves the queue: `Tui.Event.accountTime` runs after every key event and calls `freezeClock`, which sets `stSessionEnd` once `stQueue` is empty. Display reads `stSessionEnd` in preference to `stClock`, so ticks after the end are ignored. Freezing on a key event rather than on a `Tick` keeps the stop time at the moment of the last answer.

The same hook charges the interval since `stLastSample` to whichever subject was on screen for it (`chargeTime` → `stSubjTime`), so an item's total covers both its questions, wrong-answer screens, overlays, and requeued retries. The Done screen shows `avg/item:` and an `avg:` column per breakdown row; the per-row denominator counts only subjects that got screen time, so an abandoned session isn't averaged over items it never showed. It deliberately does **not** repeat the session total — the stopped corner timer is already showing it.

### Forecasting (`Srs.hs`, `minaosou forecast`)

`visitsPerStage` computes the expected number of reviews an item spends at each stage before burning; everything else (reviews/day, days to burn, pool sizes) is bookkeeping on top of it. The recursion is forward, not a linear solve: `W s` (visits accumulated climbing s → s+1) only references stages below `s`, because a miss never moves an item forwards.

Inputs are the user's own, in this order of preference:
1. **Stage ladder and intervals** — `Api.getSrsSystem` (`/v2/spaced_repetition_systems`). Not hard-coded; `srsStartingStage`/`srsPassingStage`/`srsBurningStage` parameterize everything.
2. **Cost per item** — `Srs.burnedCohort`: the mean `meaning_correct` over items that have burned. `meaning_correct` *is* the review count (WaniKani records one correct per completed review); verified against the live API, where all 1,143 items that burned with zero misses had exactly 8.
3. **Miss rate** — `Srs.fitUniformFailure` bisects for the uniform per-review rate that reproduces that measured cost. It is *not* read off the answer counts, which can only bracket it (`itemFailureBracket`) since WaniKani records misses per question, never per review.

**`/v2/reviews` returns an empty collection** — WaniKani no longer serves historical review records there (the POST used for submitting still works). That killed the obvious design, a per-stage failure profile from `starting_srs_stage`. `/v2/review_statistics` has only lifetime totals per subject, so a uniform rate is the most the data supports. Don't reach for per-stage rates again without checking that endpoint first.

**Request cost and the cache.** A full sweep is ~29 requests (1 SRS + ~14 pages review_statistics + ~14 pages assignments) against WaniKani's **60/minute per-account** budget. `Api.sentRequests` is a **process-global** MVar, so the sliding-window limiter resets on every invocation and cannot see what a previous `minaosou` process spent — three `forecast` runs in a minute reproducibly returned 429. Fixed by `ForecastCache`: both collections are stored in `~/.config/minaosou/forecast_cache.json` and refetched with WaniKani's `data_updated_at` fed back as `updated_after`, so a repeat run costs 3 requests. `--refresh` forces a full sweep. Verified: cached output is byte-identical to `--refresh` output. If you add another bulk endpoint to this command, cache it the same way.

The wrong-answer penalty (1 stage below passing, 2 from passing up) is the one thing the API does not expose, so `uniformModel` writes it down. It only fixes the exchange rate between miss rate and review count, and since the rate is *fitted* to the observed count, an error there is largely absorbed by the fit.

### Key Types

```haskell
-- Per-subject progress tracking
data Progress = Progress
  { pMeaningOk :: Bool, pReadingNeeded :: Bool, pReadingOk :: Bool
  , pMeaningWrong :: Int, pReadingWrong :: Int }

-- What gets submitted back to WaniKani
data Submission = Submission
  { subAssignmentId :: Int, subWrongMeaning :: Int, subWrongReading :: Int }

-- SRS stage category (Initiate/Apprentice/Guru/Master/Enlightened/Burned).
-- Assignment carries the current SRS stage; the post-review stage is read
-- from the createReview response (rrEndingSrsStage) rather than computed
-- locally, so it can never drift from what WaniKani actually persisted.
-- Subject carries the WaniKani level (subjLevel).
```

Answer normalization: meanings use case-folding + space-collapsing; readings use romaji→hiragana conversion via `Romaji.hs`.

The current SRS stage is shown in the question border (`Current · Apprentice`). After submission, each line in the results list shows the resulting stage (`→ Guru`).
