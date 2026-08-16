# minaosou

A terminal client for [WaniKani](https://www.wanikani.com/) — do your kanji and vocabulary SRS reviews without leaving the command line.

```
                                                                                             ⏱ 12:34
┌── Queue ─────────────────────┐ ┌── Current · Apprentice ──────────────────────────────────────────┐
│ 日 (Kanji) [meaning]         │ │                                                                  │
│ 学校 (Vocab) [reading]       │ │  日 — meaning                                                    │
│ 一 (Radical) [meaning]       │ │                                                                  │
│ 日 (Kanji) [reading]         │ │  ┌── Input ──────────────────────────────────────────────────┐  │
│                              │ │  │ sun                                                       │  │
│                              │ │  └───────────────────────────────────────────────────────────┘  │
│                              │ │                                                                  │
│ remaining: 3                 │ │  Enter=submit  Ctrl-o=override  Ctrl-r=requeue                  │
└──────────────────────────────┘ │  Ctrl-a=all info  Ctrl-u=user  Ctrl-v=reviews  Esc=quit         │
                                 └──────────────────────────────────────────────────────────────────┘
```

## Features

- Review meanings and readings in a split-pane TUI
- Current SRS stage (Apprentice / Guru / Master / Enlightened / Burned) shown in the question border
- Romaji input converted to hiragana live as you type (`gakkou` → `がっこう`)
- British/American spelling normalisation for meaning answers
- Per-item level, SRS stage, mnemonics, and component breakdowns (Ctrl-a) — kanji info lists vocabulary that uses the kanji; vocabulary info shows the readings and radical composition of each component kanji
- Review schedule for the next 24 hours (Ctrl-v)
- Session timer in the top-right corner, counting up from the start of the session and stopping when the last item is answered
- Optional pronunciation audio via an external player (Ctrl-p), with an opt-in auto-play on a reading question's first appearance (`audio_autoplay`)
- Submits results back to WaniKani at the end of each batch; post-submit list shows the resulting SRS stage per item, with incorrect items highlighted
- Configurable batch size (0 = all available reviews)
- Wrong-answer screen auto-surfaces the relevant mnemonic, flags visually-similar-kanji mix-ups by name, and shows the components that build the answer — component kanji and their meanings/readings for vocabulary, component radicals and their meanings for kanji; scrollable if it doesn't fit
- End-of-session accuracy breakdown by subject type and SRS stage, each row with the average time spent per item
- Workload forecast (`minaosou forecast`): projects the daily review load your lesson pace settles at, measured from what your own burned items actually cost, and solves it backwards for a review budget you set
- Cross-session leech tracking (`minaosou leeches`): lists subjects you keep getting wrong across sessions, and `minaosou leeches --study` lets you drill them in a dedicated practice session that is never submitted to WaniKani
- Colour scheme puts visual focus on the current answer rather than the queue: unselected queue items are dimmed, and the answer input is rendered at full brightness
- Resilient submission: transient network failures (timeouts, dropped connections, 429/5xx) are retried automatically with backoff; a review that still fails to reach WaniKani is saved locally and retried automatically the next time you run `minaosou`, without asking you to answer it again

## Installation

### Pre-built binaries

Download the binary for your platform from the [latest release](https://github.com/mwkuster/minaosou/releases/latest):

- `minaosou-linux-x86_64` — Linux (static, x86-64)
- `minaosou-macos-arm64` — macOS (Apple Silicon)
- `minaosou-windows-x86_64.exe` — Windows (x86-64)

On Linux/macOS, make it executable and put it on your `PATH`:

```bash
chmod +x minaosou-linux-x86_64          # or minaosou-macos-arm64
mv minaosou-linux-x86_64 ~/.local/bin/minaosou
```

On Windows, place `minaosou-windows-x86_64.exe` somewhere on your `PATH`. Use a modern terminal (Windows Terminal) for correct Japanese/wide-character rendering — the legacy console renders kanji poorly.

### Build from source

Requires GHC and Cabal (tested with GHC 9.6, Cabal 3.x).

```bash
git clone https://github.com/mwkuster/minaosou
cd minaosou
cabal build
cabal install
```

Or run directly without installing:

```bash
cabal run minaosou -- [command]
```

### Migrating from kroki

This tool was previously called `kroki`. If you have an existing install, move your config and data to the new location:

```bash
mv ~/.config/kroki ~/.config/minaosou
```

That directory holds your `config`, `leeches.json`, and `pending_reviews.json`. The command is now `minaosou` instead of `kroki`.

## Configuration

Run the interactive setup wizard once to create `~/.config/minaosou/config`:

```bash
minaosou init
```

Or write the file manually:

```
token=<your-wanikani-api-token>
batch_size=10
requeue_after=7
audio_player=mpv --really-quiet
audio_autoplay=false
```

| Key | Default | Description |
|---|---|---|
| `token` | — | WaniKani API token (required) |
| `batch_size` | 10 | Reviews per session; `0` = all available. Also applies to `minaosou leeches --study` |
| `requeue_after` | 7 | Positions later to requeue a missed item |
| `audio_player` | — | Command to play audio; URL appended as last argument |
| `audio_autoplay` | false | Auto-play a vocabulary reading's audio the first time it comes up each session |

The token can also be supplied via the `WANIKANI_API_TOKEN` environment variable or the `--token` flag. Priority: `--token` > env var > config file.

## Usage

```
minaosou                        # start a review session (default)
minaosou study --batch-size 20  # session with a custom batch size
minaosou whoami                 # show account info
minaosou reviews                # show review schedule for the next 24 h
minaosou leeches                # list subjects tracked as leeches (repeated wrong answers)
minaosou leeches --study        # practice all tracked leeches; never submitted to WaniKani
minaosou forecast               # project the daily review load your pace leads to
minaosou init                   # (re)create config file interactively
```

### Forecasting your review load

Your daily review count is not something you set — it is what your lesson pace and your accuracy add up to. `minaosou forecast` works out what they add up to *for you*.

It reads your own record from WaniKani rather than applying a rule of thumb. Every item that has burned has a finished review count, so the mean of those is what an item has really cost you, ladder and mistakes and all. A per-review miss rate is then fitted to reproduce that cost, and the SRS is walked forward from it — using the stage intervals WaniKani reports, not hard-coded ones — to give the load your current pace settles at:

```
What an item has cost you, lesson to burned
  reviews per item             17.4   (median 13; the gap is the leech tail)
  days per item                 225   (median)
  miss rate per review        23.3%   (answer counts bracket it at 15.5%-25.3%)

At 10 lessons/day, that settles at

  reviews per day               175
  days from lesson to burn      265
  items in circulation        2,651   (92 of them below Guru 1)
```

The projection is shown next to what your account holds today, so you can see whether the load is still climbing towards it or drifting down, and next to a sensitivity table for a miss rate above and below your own.

`--reviews-per-day N` asks the question from the other end — given a load you are willing to sit, what would have to give:

```
To hold 100 reviews/day instead, either

  · 5.7 lessons/day at today's miss rate, or
  · a miss rate of 8.4% (from 23.3%), keeping 10 lessons/day
```

`--lessons N` projects a pace you are considering rather than your measured one, and `--days N` sets the window that measured pace is taken over (default 30). The window matters: a long one averages across any stretch where you were not doing lessons, and reports a pace you are no longer on.

Your history is cached in `~/.config/minaosou/forecast_cache.json`, so only the first run reads it in full (~29 API requests); later runs ask WaniKani for what has changed since and typically cost three. This matters because WaniKani allows 60 requests per minute per account, which a few full sweeps would exhaust. `--refresh` re-reads everything from scratch. The cache holds no credentials — just a fingerprint of your token, so that pointing minaosou at a different account discards it rather than reporting the wrong history.

Two caveats the command states itself: burned items were learned at earlier levels, so they are the easier half of your history and the projection errs optimistic; and WaniKani records misses per question, never per review, so the item-level miss rate is bracketed rather than known exactly — the fitted rate is the one consistent with what your items actually cost.

### Leech tracking

Every wrong answer is recorded locally in `~/.config/minaosou/leeches.json`, since WaniKani's own SRS stage can't tell "just leveled up" from "keeps regressing across sessions." `minaosou leeches` lists tracked subjects sorted by how often they've been missed.

`minaosou leeches --study` starts a normal-looking review session built from that list instead of WaniKani's "available reviews" — worst leeches first, batched by `batch_size` — but it never calls WaniKani's review API. Instead, each practice round replaces a leech's tracked counts: answer it cleanly and it's retired — no longer shown in `minaosou leeches` or drilled by `--study` — while missing it again resets its count to just that round's mistakes rather than piling on older history.

Retiring a leech doesn't erase its record, though: if it ever comes up wrong again in a *real* WaniKani review, it's immediately un-retired and weighted higher than a fresh leech with the same miss count, so a relapsed item gets prioritized for practice sooner than one you're missing for the first time.

### Submission reliability

Every WaniKani API call retries automatically on transient failures — connection drops, timeouts, HTTP 429/5xx — with exponential backoff (up to 4 retries, capped at 8s per attempt). It never retries on a 4xx like an expired token, since that can't be fixed by trying again.

If a review submission still fails after those retries (e.g. a longer outage), it's saved to `~/.config/minaosou/pending_reviews.json` instead of being dropped. The next time you run `minaosou study`, pending reviews are resubmitted first, using their original answer time; anything still stuck stays pending for the run after that. Since the assignment is still "available" on WaniKani until its review actually lands, a pending item is also excluded from that session's fresh batch, so you're never asked to re-answer something you already answered.

## TUI keybindings

### During a review

| Key | Action |
|---|---|
| `Enter` | Submit answer |
| `Ctrl-o` | Override — mark current answer as correct |
| `Ctrl-r` | Requeue — skip for now, no wrong-answer penalty |
| `Ctrl-a` | All-info overlay (level, SRS stage, components, meanings, readings, mnemonics) |
| `Ctrl-p` | Play pronunciation audio (vocabulary, requires `audio_player`) |
| `Ctrl-u` | User info overlay |
| `Ctrl-v` | Review schedule overlay |
| `Esc` / `Ctrl-q` | Quit |

### Wrong-answer screen

| Key | Action |
|---|---|
| `Enter` | Requeue (counts as wrong) |
| `Ctrl-o` | Override as correct |
| `Ctrl-r` | Requeue without penalty |
| `↑` / `↓` | Scroll (if the mnemonic/hints don't fit) |

### Overlays (all-info, user, reviews)

| Key | Action |
|---|---|
| `↑` / `k` | Scroll up |
| `↓` / `j` | Scroll down |
| `Ctrl-a` / `Ctrl-u` / `Ctrl-v` / `Esc` | Close overlay |

### Done screen

| Key | Action |
|---|---|
| `Ctrl-s` | Submit batch results to WaniKani (just "finish" in `leeches --study`, never submitted) |
| `Ctrl-n` | Start next batch (if more reviews are available) |
| `↑↓` / `j` / `k` | Scroll submission details |
| `Esc` / `Ctrl-q` | Quit |

## License

GPL-3.0-only — see [LICENSE](LICENSE).
