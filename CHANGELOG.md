# Revision history for minaosou

## 1.1.0 -- unreleased

* **Session timer.** The top-right corner counts up from the start of a study session (`MM:SS`, `H:MM:SS` past the hour) and stops when the last item is answered, so what it shows afterwards is how long the reviews took.
* **Time per item.** The end-of-session breakdown reports the average time spent per item overall, and per row in the by-type and by-SRS-stage tables. An item's time covers both its questions, any wrong-answer screens and overlays, and each requeued retry.

## 1.0.0 -- 2026-07-30

* **Renamed: `kroki` → `minaosou`.** The command, package, repository, and config directory are all now `minaosou` (見直そう, "let's review"). The old name collided with the well-known [kroki.io](https://kroki.io) diagram tool; this is otherwise the same application.
  * **Migrating from kroki:** move your config and data with `mv ~/.config/kroki ~/.config/minaosou` (it holds `config`, `leeches.json`, and `pending_reviews.json`). The command is now `minaosou` instead of `kroki`.
* First release under the new name; no functional changes from 0.9.9.

_Entries below were released under the old name, `kroki`._

## 0.9.9 -- 2026-07-30

* The release pipeline now also builds a Windows (x86-64) binary, alongside Linux (static, x86-64) and macOS (arm64); the README points at the pre-built binaries on the releases page.
* Reworded the project description: "a WaniKani review client" (previously "minimal").

## 0.9.8 -- 2026-07-30

Two bug fixes.

* **Answered reviews are no longer lost when the network drops mid-session.** If the connection failed while submitting, an internal check for already-recorded reviews (itself a network call) could throw before the failed reviews were saved for retry, so they were silently dropped — neither submitted nor queued. Failed reviews are now always persisted and automatically resubmitted on the next run.
* **Text containing kanji/kana no longer clips at the right edge.** Wrapping measured width by character count, so every wide character pushed a line one cell past the pane and the terminal cut it off (a trailing kanji becoming `…`, a letter vanishing). Wrapping now measures true terminal display width.
* Release workflow: bumped GitHub Actions off the deprecated Node 20 runtime.

## 0.9.7 -- 2026-07-27

### Answer acceptance
* kroki now accepts every meaning WaniKani accepts: whitelisted auxiliary meanings and your own study-material synonyms are recognised. Previously a legitimate answer could be marked wrong, requeued, and even submitted to WaniKani as incorrect
* Add a meaning synonym to WaniKani directly from a wrong-answer screen — `Ctrl-y` on a meaning question opens an editable field; on success the synonym is accepted for the rest of the session and the item counts correct
* Romaji input: `nn` now commits a single ん (matching WaniKani/wanakana), so `zennin` and `zen'in` both give ぜんいん; write ん before a な-row syllable as `on'na`/`onnna`
* More British/American spelling normalisation (e.g. `neighbourhood`)

### Leech tracking
* Cross-session leech tracking: items you miss are remembered across sessions and surfaced in the all-info overlay (`Ctrl-a`) as "Missed before"
* New `kroki leeches` command lists tracked leeches worst-first; `kroki leeches --study` drills them without submitting to WaniKani. Missing a leech in practice raises its score; a clean round retires it; a relapse in a real review is weighted higher

### Learning aids
* Wrong-answer screen auto-surfaces the relevant mnemonic, flags visually-similar-kanji mix-ups by name, and shows component breakdowns (component kanji readings/meanings for vocabulary, radicals for kanji)
* Optional auto-play of reading audio on a vocab reading question's first appearance (opt-in `audio_autoplay`)
* End-of-session breakdown by SRS stage and subject type; rebalanced TUI colours/contrast

### Reliability
* Failed review submissions are retried automatically with backoff and, if still unsent, persisted and resubmitted on the next run instead of being lost; already-recorded reviews are not retried
* API calls are rate-limited to stay within WaniKani's request budget on large batches
* Error messages now report the actual HTTP status / network cause instead of a generic string
* `kroki init` no longer fails when a config already exists, and the config file (which holds your API token) is written with owner-only permissions

## 0.9.6 -- 2026-05-17

* All-info overlay (Ctrl-a): kanji subjects now list visually similar kanji, with their readings and meanings
* `kroki <command> --help` now also lists the global `--token` option, so every option a command accepts is shown

## 0.9.5 -- 2026-05-02

* `Enter` now also closes the all-info overlay (Ctrl-a), alongside Ctrl-a and Esc
* Wrong-answer feedback lines wrap when the list of accepted answers is long
* Ctrl-a on a fresh question (no input typed yet) now opens the all-info overlay for the just-answered subject, so you can review what you just got right or wrong before starting the next one

## 0.9.4 -- 2026-04-26

* TUI no longer freezes during review submission: POSTs run on a background thread, with a "Submitting…" banner and input blocked until the result arrives
* Submissions run in parallel (capped at 50 in-flight) to keep wall-time short on large batches without breaching WaniKani's 60 req/min limit
* Refactor: `src/Tui.hs` split into `Tui.State`, `Tui.Draw`, and `Tui.Event` submodules behind a thin facade

## 0.9.3 -- 2026-04-25

* All-info overlay (Ctrl-a): kanji subjects now list the vocabulary that uses them; vocabulary subjects show the accepted readings of each component kanji
* Fix duplicate hour label in the review schedule overlay (Ctrl-v)

## 0.9.2 -- 2026-04-12

* Release workflow: fix Linux static build by dropping `gmp-static`; grant `contents: write` so binaries upload to GitHub releases

## 0.9.1 -- 2026-04-12

* Add GitHub Actions release workflow producing static Linux x86-64 and macOS arm64 binaries on `v*` tags

## 0.9.0.0 -- 2026-04-12

* SRS stage shown in question border (`Current · Apprentice`) and in the Ctrl-a info overlay
* Post-submission list shows the resulting SRS stage per item (`→ Guru`)
* Subject level shown in Ctrl-a info overlay
* British/American spelling normalisation for meaning answers (including `-ourable`/`-ourite` suffixes)
* Pronunciation audio playback via configurable external player (Ctrl-p)
* Review schedule overlay for the next 24 hours (Ctrl-v)
* Romaji→hiragana live conversion during reading input
* Full WaniKani review flow: fetch → study → submit
