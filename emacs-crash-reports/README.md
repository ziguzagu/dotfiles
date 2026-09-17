# Emacs NS scroll crash — collected reports

Supporting material for the draft bug report in
`../emacs-ns-scroll-crash-report.md`.

Ten crashes, all at `-[EmacsView copyRect:to:]` called from `ns_scroll_run`,
on macOS 26 (Tahoe), across two Emacs versions and five binaries — including
one built on GitHub's `macos-26` runner and one **stock, unpatched** build from
emacsformacosx.com.

| # | when | version | build | fault | address | file |
|---|---|---|---|---|---|---|
| 1 | 2026-08-28 16:12 | 31.1 | local | read (0x1), unmapped | 0x172640010 | excerpt |
| 2 | 2026-08-28 16:33 | 31.1 | local | read (0x1), unmapped | 0x175618010 | excerpt |
| 3 | 2026-08-28 16:38 | 31.1 | local | read (0x1), unmapped | 0x16af4c010 | excerpt |
| 4 | 2026-09-08 17:48 | 31.1 | local | write (0x2), `r--` mapping | 0x173424010 | excerpt |
| 5 | 2026-09-11 15:54 | 31.1 | local | read (0x1), unmapped | 0x1759b4010 | `Emacs-2026-09-11-155434.ips` |
| 6 | 2026-09-16 11:33 | 30.2 | local | read (0x1), unmapped | 0x16b20c010 | `Emacs-2026-09-16-113341.ips` |
| 7 | 2026-09-17 10:02 | 30.2 | local | write (0x2), `r--` mapping | 0x13308c010 | `Emacs-2026-09-17-100237.ips` |
| 8 | 2026-09-17 13:28 | 31.1 (emacs-31 tip) | emacs-plus prebuilt | read (0x1), unmapped | 0x136894010 | `Emacs-2026-09-17-132843.ips` |
| 9 | 2026-09-17 13:31 | 31.1 (emacs-31 tip) | emacs-plus prebuilt | write (0x2), `r--` mapping | 0x1329cc010 | `Emacs-2026-09-17-133141.ips` |
| 10 | 2026-09-17 17:11 | 31.1 | **vanilla, emacsformacosx** | write (0x2), dyld private mem | 0x133e93810 | `Emacs-arm64-11-2026-09-17-171124.ips` |

Crashes 8 and 9 killed the build-environment theory: that binary is the
`emacs-plus-app` cask — compiled by the tap's CI on a GitHub `macos-26` runner,
shipping its own copies of every dylib inside `Emacs.app/Contents/Frameworks`,
with **zero** references to `/opt/homebrew`.

Crash 10 is the most important one for reporting upstream.  That binary is
plain GNU Emacs 31.1 from emacsformacosx.com — **no emacs-plus patches at all**,
so the "this is not a vanilla build" caveat no longer applies.  It also carries
`LC_BUILD_VERSION minos 11.0 / sdk 12.0` and reports
`NS appkit-2487.70 Version 14.8.7`, i.e. it was compiled on macOS 14 against the
macOS 12 SDK.  It crashes the same way on macOS 26.

## Files

- `Emacs-2026-09-*.ips`, `Emacs-arm64-11-2026-09-17-171124.ips` — original crash
  reports, copied verbatim from `~/Library/Logs/DiagnosticReports/`.
- `excerpts-2026-08-28-and-09-08.md` — crashes 1-4.  These were rotated out of
  `DiagnosticReports` before being saved, so only excerpts survive (header,
  exception, VM region info, crashing thread, binary UUID).  Not original files.

## Why these ten are one issue

- Identical crash site in every one.
- Two different Emacs versions across five separate binaries (31.1 UUIDs
  `1024a80c…`, `59ca43ca…`, `bccbd2b5…` and `caba95c6…`; 30.2 UUID `5ffa8ac4…`).
- Crash 2 has no `markdown-ts-mode` and no Markdown tree-sitter grammar loaded,
  which ruled out the original tree-sitter theory.
- Crash 6 has `consult-imenu` and `Fread_from_minibuffer` on the stack — the
  clearest reproduction context obtained so far.

## The fault address pattern — held for 9, broken by the 10th

Crashes 1-9 all faulted at an address ending in `0x010`, and where the address
landed inside a mapped region it was exactly 16 bytes past that region's start,
in a read-only (`r--`) file mapping.  Crashes 4 and 9 even showed the identical
`GAP OF 0x3548000 BYTES` before the faulting mapping.  That regularity was the
basis for suspecting a base pointer that lands at the start of some unrelated
mapping.

Crash 10 does not fit.  It faulted at `0x133e93810` — 79888 bytes (`0x13810`)
into a 208K **dyld private memory** region, not 16 bytes into a file mapping:

```
0x133e93810 is in 0x133e80000-0x133eb4000;  bytes after start: 79888
      mapped file          12514c000-130888000  [183.2M] r--/rwx SM=COW
      GAP OF 0x35f8000 BYTES
--->  dyld private memory  133e80000-133eb4000  [  208K] r--/rwx SM=PRV
```

Note that crash 10 is the only one on a binary *without* `fix-ns-scroll-crash`,
which clamps the top edge in `ns_scroll_run` and `copyRect:to:`.  Whether that
explains the different offset has not been established — it is one untested
possibility, not a conclusion.  Either way, the "+16" framing should be
presented as covering 9 of 10, not all of them.

## Incidental observation

The four write faults (4, 7, 9, 10) go `deliver_fatal_thread_signal` →
`_sigtramp` directly, while all six read faults (1, 2, 3, 5, 6, 8) pass through
`handle_sigsegv` first.  The split has held for all ten.

## The second machine (control, as of 2026-09-17)

A second Mac managed from this same dotfiles repo is being kept deliberately
untouched as a control: still on `emacs-plus@30`, no cask migration, no
`defaults write`.  Emacs is used there only occasionally and has not crashed,
but no deliberate reproduction attempt has been made yet, so that is not
evidence of absence.

Everything that could plausibly matter is identical:

| | crashing machine | control machine |
|---|---|---|
| macOS | 26.6.2 (25G83) | 26.6.2 (25G83) |
| hw.model | Mac15,12 | Mac16,10 |
| GPU | **Apple M3**, 10 cores, Metal 4 | **Apple M4**, 10 cores, Metal 4 |
| Emacs | 30.2, emacs-plus@30 | 30.2, emacs-plus@30 |
| minos / sdk | 26.0 / 26.5 | 26.0 / 26.5 |
| binary UUID | `5ffa8ac4…` | `0c34639b…` (built separately) |
| display | DELL P2725QE, 5120x2880, UI 2560x1440 @ 100.00Hz, Main, Mirror Off, sole display | **identical, verbatim** |

`system_profiler SPDisplaysDataType` returns character-for-character the same
block on both.

**Correction (2026-09-17):** this was first written up as "display geometry is
therefore ruled out".  That is backwards.  Two machines sharing the *same*
display configuration cannot test that configuration — it is the same condition
run twice.  The control machine isolates the SoC/GPU generation and nothing
else.  The crashing machine's reports load `AGXMetalG15G_C0` (the M3 driver); a
crash on the control machine would name a different driver and give a direct
comparison on that axis alone.

## macOS update timeline — the best correlation found so far

`system_profiler SPInstallHistoryDataType` on the crashing machine:

```
26.5      2026-05-27
26.5.1    2026-06-05
26.5.2    2026-07-14     <- new monitor arrived 2026-06-17; two months, no crashes
26.6      2026-08-14  *
26.6.1    2026-08-20  *
                          <- crash 1 on 2026-08-28, running 26.6.1
26.6.2    2026-08-29
```

Emacs had been running on macOS 26 since 2025-12-26 (26.2) without any of this.
The crashes begin two weeks after 26.6 and eight days after 26.6.1.

This is the only identified variable that is independent of both the Emacs build
and the Emacs configuration, which is what the other nine ruled-out hypotheses
demand.  It reframes the issue from "macOS 26" to **"macOS 26.6 or later"**.

Caveat, stated plainly: there is a 14-day gap between the 26.6 install and the
first observed crash.  Whether that is because the trigger was simply not hit,
or because 26.6.1 rather than 26.6 is the relevant release, is not established.
Downgrading macOS to test is not practical.

## The display mode is unusual — untested hypothesis

The DELL P2725QE is a 27" 4K panel, native **3840x2160**.  macOS reports:

```
Resolution:    5120 x 2880
UI Looks like: 2560 x 1440 @ 100.00Hz
```

A 5120x2880 framebuffer on a 3840x2160 panel means this is a **scaled** HiDPI
mode: macOS renders into an oversized intermediate backing store and resamples
it down to the panel every frame.  The default for this monitor would be
"looks like 1920x1080" — native 3840x2160 at a clean 2x, no resampling.

Given that the crash is in a scroll blit against the view's pixel buffer, a
non-integer scaled backing store is a plausible contributing condition, and it
would also explain why so few people appear to hit this: most 4K users run the
default mode.

Both machines are configured this way, so neither tests it.  The cheap
experiment is to set the crashing machine back to the default scaling and try
the usual triggers.  Not yet done.

## What has been ruled out

| hypothesis | killed by |
|---|---|
| `markdown-ts-mode` / tree-sitter | crash 2 — no Markdown grammar loaded at all |
| an Emacs 31 regression | crashes 6, 7 — 30.2 fails identically |
| local build environment / Homebrew library versions | crashes 8, 9 — CI-built binary with zero `/opt/homebrew` linkage |
| emacs-plus patches | crash 10 — stock upstream build, no patches |
| `minos 26.0` deployment target / linked-on-or-after AppKit gating | crash 10 — `minos 11.0`, built on macOS 14 against the macOS 12 SDK |
| bug#80268 (Tahoe scrolling lag) | its fix is compiled into the affected builds; setting both defaults explicitly changed nothing |
| bug#81585 as filed | that bug needs `pixel-scroll-precision-mode` + a header-line toggle; neither is in use here.  It was reported 2026-08-09 and closed 2026-08-12, before the first crash here |
| `fix-ns-scroll-crash` being the cause or the cure | present in crashes 1-9 and does not prevent them; absent in crash 10, which crashes anyway |

Display geometry is **not** ruled out — see the correction above.

What remains, in rough order of promise:

1. **The scaled HiDPI display mode** (5120x2880 backing store on a 3840x2160
   panel).  Untested, cheap to test, and would explain the apparent rarity.
2. Something about this machine or its M3 GPU.  The control machine settles this
   one, and only this one.
3. macOS 26 itself, with no further qualifier.

Every *build-side* variable has now been changed, in both directions, without
affecting the outcome.  The remaining levers are all runtime/environment.
