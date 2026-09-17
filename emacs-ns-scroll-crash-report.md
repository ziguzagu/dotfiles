# Draft bug report (updated 2026-09-17, after crash 10 — the vanilla build)

**To:** bug-gnu-emacs@gnu.org
**Subject:** 30.2, 31.1; macOS 26 (Tahoe): crash in ns_scroll_run / -[EmacsView copyRect:to:] while scrolling

---

On macOS 26 (Tahoe) Emacs dies during scrolling, always at the same two frames:
`-[EmacsView copyRect:to:]` called from `ns_scroll_run`.  I have ten crashes
over three weeks, on **two different Emacs versions** — 30.2 and 31.1 — across
five separate binaries.  Scrolling on this machine was stable on Emacs 30 before
the macOS upgrade.

The most recent crash is on a **stock, unpatched** build: GNU Emacs 31.1 as
distributed by emacsformacosx.com.  Seven of the earlier crashes were on
binaries I compiled myself and two on a prebuilt binary from the emacs-plus tap,
so I worked through those possibilities first; the vanilla build removes them
all.  The relevant history, since it may save you asking:

- Crashes 1-7 were on Emacs I built locally.  I suspected my own build
  environment — specifically the Homebrew library versions I link against.
- Crashes 8-9 were on the emacs-plus `emacs-plus-app` cask, compiled by that
  tap's CI on a GitHub `macos-26` runner, bundling its own copies of every
  dependency and containing no reference to `/opt/homebrew`.  That ruled out my
  build environment.
- Crash 10 is vanilla upstream Emacs with no third-party patches at all, and
  additionally has a completely different deployment target (see below).  That
  ruled out both the emacs-plus patches and the macOS 26 SDK.

This is **not** bug#80268.  That bug covers the Tahoe scrolling *lag* and input
handling; its fix is compiled into the affected builds (verified below) and I
still get these crashes.  I could not find an existing report for this
signature.

For completeness, the patches on the non-vanilla builds were:

- 30.2: `fix-macos-tahoe-scrolling` (= the bug#80268 fix, backported),
  `fix-ns-x-colors`, `fix-window-role`, `round-undecorated-frame`,
  `system-appearance`
- 31.1 (local and emacs-plus prebuilt): `fix-ns-x-colors`, `system-appearance`,
  `round-undecorated-frame`, `fix-ns-scroll-crash` (= Alan Third's v2 patch from
  bug#81585)

None of these touch the scroll-copy path except `fix-ns-scroll-crash`, which is
*supposed* to prevent exactly this crash and demonstrably does not.  The vanilla
build has none of them and crashes the same way.

## Environment

```
GNU Emacs 31.1 (build 2, aarch64-apple-darwin23.6.0,
                NS appkit-2487.70 Version 14.8.7 (Build 23J520))
 of 2026-08-25
   <- vanilla, emacsformacosx.com, crash 10

GNU Emacs 31.1
Development version a360712c9d27 on HEAD branch; build date 2026-09-08.
   <- emacs-plus prebuilt, GitHub macos-26 runner, crashes 8-9

GNU Emacs 31.1 (build 1, aarch64-apple-darwin25.6.0,
                NS appkit-2685.70 Version 26.6.2 (Build 25G83))
 of 2026-09-01
   <- locally built, crashes 4-5

GNU Emacs 30.2 (build 1, aarch64-apple-darwin25.6.0,
                NS appkit-2685.70 Version 26.6.2 (Build 25G83))
 of 2026-09-11
   <- locally built, crashes 6-7
```

The vanilla build is configured as:

```
--with-ns --with-modules --with-x-toolkit=no
'CFLAGS=-DFD_SETSIZE=10000 -DDARWIN_UNLIMITED_SELECT'
LDFLAGS=-headerpad_max_install_names
```

The two locally built ones were configured with (differing only in
prefix/infodir):

```
--disable-dependency-tracking --disable-silent-rules
--enable-locallisppath=/opt/homebrew/share/emacs/site-lisp
--with-native-compilation=aot --with-xml2 --with-gnutls
--without-compress-install --without-dbus --without-imagemagick
--with-modules --with-rsvg --with-webp --with-ns
--disable-ns-self-contained
```

**Deployment target is not the factor either.**  Crashes 1-9 were on binaries
with `LC_BUILD_VERSION minos 26.0 / sdk 26.5`, so I suspected macOS 26's
linked-on-or-after AppKit behaviour.  The vanilla build in crash 10 has
`minos 11.0 / sdk 12.0` and was compiled on macOS 14 — as its `appkit-2487.70`
version string shows — and it still crashes on macOS 26.

- macOS 26.6.2 (25G83); the three oldest crashes were on 26.6.1 (25G76)
- Mac15,12 (Apple M3), single external display (5120x2880)

## This looks like a macOS 26.6 regression, not a macOS 26 one

I had been describing this as "on macOS 26", but my own update history is more
specific than that.  I have been on macOS 26 since 2025-12-26 and saw none of
this until late August:

```
26.5      2026-05-27
26.5.1    2026-06-05
26.5.2    2026-07-14
26.6      2026-08-14   <---
26.6.1    2026-08-20   <---
                           first crash 2026-08-28, running 26.6.1
26.6.2    2026-08-29
```

Eight months of Emacs 30 on macOS 26.0 through 26.5.2 with no scroll crashes at
all; they start two weeks after 26.6 lands.  This is the only variable I have
found that is independent of both the Emacs build and my configuration, which is
what everything above demands.

Two caveats.  There is a 14-day gap between the 26.6 install and my first
observed crash, and I cannot say whether 26.6 or 26.6.1 is the relevant release
— I may simply not have hit the trigger in between.  And I obviously cannot
downgrade macOS to confirm.

If anyone else is seeing this, the question worth asking is not "are you on
macOS 26" but "were you on 26.5 or earlier, and did it start at 26.6".

Possibly relevant settings: `scroll-conservatively` 0, `scroll-margin` 0,
`scroll-step` 1, `pixel-scroll-precision-mode` **not** enabled, no header-line,
`doom-modeline` for the mode-line.

## When it happens

Emacs dies with "Abort trap: 6" while scrolling.  It is intermittent and I
cannot reproduce it on demand, but it is clearly tied to scrolling, and by far
the most common triggers are:

- scrolling right after jumping somewhere via `imenu`
- scrolling a buffer while an `isearch` (`C-s`) session has it filtered

One of the crashes captured that context directly.  Crash 6 happened inside a
`consult-imenu` session — that is, while a minibuffer completion read was
active and the source buffer was being scrolled by consult's live preview:

```
-[EmacsView copyRect:to:] + 360
ns_scroll_run + 392
update_window + 5020
update_window_tree + 116
update_frame + 208
redisplay_internal + 5404
read_char + 2904
read_key_sequence + 1096
command_loop_1 + 704
internal_condition_case + 92
command_loop_2 + 52
internal_catch + 88
command_loop + 132
recursive_edit_1 + 168
Fread_from_minibuffer + 3360          <-- minibuffer read
exec_byte_code + 1992
Ffuncall + 324
Fapply + 596
  ... (elided) ...
F636f6e73756c742d696d656e75_consult_imenu_0 + 92   <-- consult-imenu
Ffuncall + 324
Ffuncall_interactively + 68
Ffuncall + 324
Fcall_interactively + 1120
...
```

So the crashing redisplay is happening in a **recursive edit**, driven by
scrolling a window other than the one holding point.  That may be why this
shows up so much more often than during plain `C-n` scrolling.

## The ten crashes

| # | when | version | build | fault | code | address |
|---|---|---|---|---|---|---|
| 1 | 2026-08-28 16:12 | 31.1 | local | KERN_INVALID_ADDRESS | 0x1 (read) | 0x172640010 |
| 2 | 2026-08-28 16:33 | 31.1 | local | KERN_INVALID_ADDRESS | 0x1 (read) | 0x175618010 |
| 3 | 2026-08-28 16:38 | 31.1 | local | KERN_INVALID_ADDRESS | 0x1 (read) | 0x16af4c010 |
| 4 | 2026-09-08 17:48 | 31.1 | local | KERN_PROTECTION_FAILURE | 0x2 (write) | 0x173424010 |
| 5 | 2026-09-11 15:54 | 31.1 | local | KERN_INVALID_ADDRESS | 0x1 (read) | 0x1759b4010 |
| 6 | 2026-09-16 11:33 | 30.2 | local | KERN_INVALID_ADDRESS | 0x1 (read) | 0x16b20c010 |
| 7 | 2026-09-17 10:02 | 30.2 | local | KERN_PROTECTION_FAILURE | 0x2 (write) | 0x13308c010 |
| 8 | 2026-09-17 13:28 | 31.1 | emacs-plus prebuilt | KERN_INVALID_ADDRESS | 0x1 (read) | 0x136894010 |
| 9 | 2026-09-17 13:31 | 31.1 | emacs-plus prebuilt | KERN_PROTECTION_FAILURE | 0x2 (write) | 0x1329cc010 |
| 10 | 2026-09-17 17:11 | 31.1 | **vanilla** | KERN_PROTECTION_FAILURE | 0x2 (write) | 0x133e93810 |

Five separate binaries are involved: 31.1 built 2026-08-28 (UUID `1024a80c…`,
crashes 1-3), 31.1 rebuilt 2026-09-01 (`59ca43ca…`, crashes 4-5), 30.2 built
2026-09-11 (`5ffa8ac4…`, crashes 6-7), the emacs-plus prebuilt emacs-31 branch
tip of 2026-09-08 (`bccbd2b5…`, crashes 8-9), and the vanilla emacsformacosx
31.1 of 2026-08-25 (`caba95c6…`, crash 10).

Process uptime before the crash ranges from 12 seconds to 49 hours, but that
range is misleading: the long ones are mostly idle time with Emacs open and
untouched.  Measured against actual scrolling, the rate is much higher and
fairly consistent — the three crashes on 2026-08-28 came within a single 26
minute session of concentrated use, and crash 9 came three minutes after
relaunching from crash 8.

Incidentally, the four write faults go `deliver_fatal_thread_signal` →
`_sigtramp` directly, while all six read faults pass through `handle_sigsegv`
first.  That split has held for all ten.

## The faulting address

In crashes 1-9 the fault address always ended in `0x010`, and in the three cases
where it landed inside a mapped region the crash reporter put it exactly 16
bytes past that region's start, in a **read-only** (`r--`) file mapping — which
is not something Emacs should ever be writing into:

```
crash 4 (2026-09-08):
0x173424010 is in 0x173424000-0x173c1c000; bytes after start: 16
      mapped file  16f744000-16fedc000  [ 7776K] r--/rwx SM=COW
      GAP OF 0x3548000 BYTES
--->  mapped file  173424000-173c1c000  [ 8160K] r--/rwx SM=COW

crash 7 (2026-09-17):
0x13308c010 is in 0x13308c000-0x137b28000; bytes after start: 16
      mapped file  13308c000-137b28000  [ 74.6M] r--/rwx SM=COW

crash 9 (2026-09-17, emacs-plus prebuilt):
0x1329cc010 is in 0x1329cc000-0x137468000; bytes after start: 16
      mapped file  12f248000-12f484000  [ 2288K] r--/rwx SM=COW
      GAP OF 0x3548000 BYTES
--->  mapped file  1329cc000-137468000  [ 74.6M] r--/rwx SM=COW
```

Crashes 4 and 9 — different binaries, nine days apart — even show the identical
preceding `GAP OF 0x3548000 BYTES`.  The other six of those nine faulted at an
address in no region at all.

**Crash 10 breaks that pattern**, and I would rather flag it than have it
noticed later.  It faulted 79888 bytes (`0x13810`) into a 208K *dyld private
memory* region, not 16 bytes into a file mapping:

```
crash 10 (2026-09-17, vanilla build):
0x133e93810 is in 0x133e80000-0x133eb4000; bytes after start: 79888
      mapped file          12514c000-130888000  [183.2M] r--/rwx SM=COW
      GAP OF 0x35f8000 BYTES
--->  dyld private memory  133e80000-133eb4000  [  208K] r--/rwx SM=PRV
```

Crash 10 is also the only one on a binary *without* `fix-ns-scroll-crash`, whose
clamps change the arithmetic in both `ns_scroll_run` and `copyRect:to:`.  I do
not know whether that explains the different offset, and I am not going to claim
it does.

Across all ten, the destination consistently lands in memory that Emacs has no
business writing to, rather than at a plausible-but-slightly-wrong offset within
its own buffer.  That reads to me more like a bad base pointer than like an
off-by-N in the rectangle arithmetic, but I have not instrumented a build to
confirm the actual coordinates, so this is inference from the fault signature
only.

Crashes 7 and 10 also carried a Kernel Triage section that the others did not:

```
Kernel Triage:
VM - (arg = 0x0) Waiting on busy page was interrupted
VM - (arg = 0x0) Fault was interrupted   (x2-4)
```

## Relationship to bug#80268

I confirmed the bug#80268 fix is compiled into both binaries:

```
$ strings /opt/homebrew/opt/emacs-plus@30/Emacs.app/Contents/MacOS/Emacs \
    | grep -c NSEventConcurrentProcessingEnabled
1
$ strings /opt/homebrew/opt/emacs-plus@31/Emacs.app/Contents/MacOS/Emacs \
    | grep -c NSEventConcurrentProcessingEnabled
1
```

30.2 has it via the emacs-plus backport; 31.1 ships it upstream.  I additionally
set both defaults explicitly for the app domain:

```
defaults write org.gnu.Emacs NSEventConcurrentProcessingEnabled -bool NO
defaults write org.gnu.Emacs NSApplicationUpdateCycleEnabled   -bool NO
```

That made no difference, which is expected since the binaries already register
those defaults themselves.  Crashes 5, 6 and 7 all happened with the settings in
place.

## What I ruled out

- **Not tree-sitter or markdown-ts-mode.**  My first theory, and wrong.  There
  are no tree-sitter frames in any backtrace; crash 2 has no `markdown-ts-mode`
  and no Markdown grammar loaded at all; and crashes 6 and 7 were not in
  Markdown buffers.
- **Not variable line heights from heading faces.**  My theme does not scale
  `outline-N` or Markdown heading faces.
- **Not an Emacs 31 regression.**  30.2 crashes identically, which is why I am
  reporting against both.
- **Not fixed by the bug#81585 patch.**  Crashes 1-9 are on builds carrying Alan
  Third's v2 patch from that bug, which clamps the top edge in `ns_scroll_run`
  (`to_y < y`) and in `copyRect:to:` (`dest.y < 0`).  Neither clamps the bottom
  edge, and the write faults are consistent with running past the end of the
  destination rather than above its start.  Crash 10, without the patch, crashes
  too — so the patch neither causes nor prevents this.
- **Not my build environment.**  This was my own leading theory for a while,
  since every crashing binary up to crash 7 was compiled on this machine
  against the same Homebrew dependency tree, and the pre-2026 Emacs 30.2 build
  had run on macOS 26 for months without incident.  Crashes 8 and 9 are on a
  binary built on a GitHub `macos-26` runner that links none of this machine's
  libraries.  Same crash.
- **Not the emacs-plus patches.**  Crash 10 is stock upstream Emacs.
- **Not the macOS 26 deployment target.**  My next theory after the above: every
  binary through crash 9 had `minos 26.0 / sdk 26.5`, so macOS 26's
  linked-on-or-after AppKit gating looked like the remaining candidate.  The
  crash-10 binary has `minos 11.0 / sdk 12.0` and was built on macOS 14.  Same
  crash.
- **Not the bug#81585 reproduction.**  That bug needs
  `pixel-scroll-precision-mode` plus toggling a header-line off while a line is
  partially scrolled.  I use neither, and my first crash was on 2026-08-28,
  after that bug was closed.  Same crash site, different route to it.

## Display configuration — a condition I have not yet tested

I should flag one thing about my setup that may matter, and may explain why this
does not seem to be widely reported.

My monitor is a DELL P2725QE, a 27" panel whose native resolution is 3840x2160.
macOS reports:

```
Resolution:    5120 x 2880
UI Looks like: 2560 x 1440 @ 100.00Hz
```

A 5120x2880 framebuffer on a 3840x2160 panel means this is a **scaled** HiDPI
mode — macOS renders into an oversized intermediate backing store and resamples
it to the panel.  The default for this display would be "looks like 1920x1080",
i.e. native 3840x2160 at a clean 2x with no resampling.

Since the crash is a blit against the view's pixel buffer, an oversized
non-integer backing store seems worth considering.  I have not yet tried the
default scaling; I will, and will report back either way.

I also have a second Mac on the same macOS build (26.6.2 25G83) whose
`system_profiler SPDisplaysDataType` output is character-for-character identical
— so it does *not* test this hypothesis.  It differs only in SoC (M4 there, M3
here; my reports load `AGXMetalG15G_C0`).  I have not yet made a serious
reproduction attempt on it.

## No Lisp-level workaround

For completeness: there is no way I could find to turn this code path off from
Lisp.  The redisplay debug variables are not defined in a normal build:

```
inhibit-try-window-id         boundp=nil
inhibit-try-window-reusing    boundp=nil
inhibit-try-cursor-movement   boundp=nil
```

So on macOS 26 there is currently no configuration that avoids the crash short
of not using the GUI.

## Attachments

I have the original `.ips` files for crashes 5 through 10 and can attach them —
crash 10 (the vanilla build) is probably the one worth starting from.  Crashes
1-4 were rotated out of `~/Library/Logs/DiagnosticReports/` before I saved them;
for those I have excerpts (header, exception, VM region info, crashing thread,
binary UUID) rather than the original files.

I am happy to run an instrumented build, apply a candidate patch, or collect
anything else that would help.
