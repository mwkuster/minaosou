# kroki

A minimal terminal client for [WaniKani](https://www.wanikani.com/) — do your kanji and vocabulary reviews without leaving the command line.

```
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
- Per-item level, SRS stage, mnemonics, and component breakdowns (Ctrl-a) — kanji info lists vocabulary that uses the kanji; vocabulary info shows the readings of each component kanji
- Review schedule for the next 24 hours (Ctrl-v)
- Optional pronunciation audio via an external player (Ctrl-p), with an opt-in auto-play on a reading question's first appearance (`audio_autoplay`)
- Submits results back to WaniKani at the end of each batch; post-submit list shows the resulting SRS stage per item, with incorrect items highlighted
- Configurable batch size (0 = all available reviews)
- Wrong-answer screen auto-surfaces the relevant mnemonic, flags visually-similar-kanji mix-ups by name, and — for vocabulary — shows the component kanji and their meanings/readings; scrollable if it doesn't fit
- End-of-session accuracy breakdown by subject type and SRS stage
- Cross-session leech tracking (`kroki leeches`): lists subjects you keep getting wrong across sessions, and `kroki leeches --study` lets you drill them in a dedicated practice session that is never submitted to WaniKani

## Installation

Requires GHC and Cabal (tested with GHC 9.6, Cabal 3.x).

```bash
git clone https://github.com/mwkuster/kroki
cd kroki
cabal build
cabal install
```

Or run directly without installing:

```bash
cabal run kroki -- [command]
```

## Configuration

Run the interactive setup wizard once to create `~/.config/kroki/config`:

```bash
kroki init
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
| `batch_size` | 10 | Reviews per session; `0` = all available. Also applies to `kroki leeches --study` |
| `requeue_after` | 7 | Positions later to requeue a missed item |
| `audio_player` | — | Command to play audio; URL appended as last argument |
| `audio_autoplay` | false | Auto-play a vocabulary reading's audio the first time it comes up each session |

The token can also be supplied via the `WANIKANI_API_TOKEN` environment variable or the `--token` flag. Priority: `--token` > env var > config file.

## Usage

```
kroki                        # start a review session (default)
kroki study --batch-size 20  # session with a custom batch size
kroki whoami                 # show account info
kroki reviews                # show review schedule for the next 24 h
kroki leeches                # list subjects tracked as leeches (repeated wrong answers)
kroki leeches --study        # practice all tracked leeches; never submitted to WaniKani
kroki init                   # (re)create config file interactively
```

### Leech tracking

Every wrong answer is recorded locally in `~/.config/kroki/leeches.json`, since WaniKani's own SRS stage can't tell "just leveled up" from "keeps regressing across sessions." `kroki leeches` lists tracked subjects sorted by how often they've been missed.

`kroki leeches --study` starts a normal-looking review session built from that list instead of WaniKani's "available reviews" — worst leeches first, batched by `batch_size` — but it never calls WaniKani's review API. Instead, each practice round replaces a leech's tracked counts: answer it cleanly and it drops off the list entirely (graduated); miss it again and its count resets to just that round's mistakes rather than piling on older history.

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
