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
- **`Cli.hs`** — `optparse-applicative` command/option definitions (`WhoAmI`, `Reviews`, `Study`).
- **`Config.hs`** — Simple key=value config file parser for `~/.config/minaosou/config`.
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
