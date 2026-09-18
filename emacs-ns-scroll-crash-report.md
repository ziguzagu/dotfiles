# Draft bug report (rewritten 2026-09-18, after finding the trigger)

**To:** bug-gnu-emacs@gnu.org
**Subject:** 30.2, 31.1; macOS 26.6: ns_scroll_run writes out of bounds when the frame is taller than the monitor workarea

> **NOT YET SENT.**  Before sending, verify the recipe below actually
> reproduces on demand — see "Status of this report" at the end.

---

On macOS 26.6 and later, a frame that is taller than its monitor's workarea
makes Emacs write outside the view's pixel buffer while scrolling, and die with
`EXC_BAD_ACCESS` in `-[EmacsView copyRect:to:]`, called from `ns_scroll_run`.

I hit this ten times over three weeks before finding the cause.  My own
configuration was at fault — I was sizing the frame from `display-pixel-height`,
which includes the menu bar — and I have fixed it on my side.  I am reporting it
anyway because the failure mode is an out-of-bounds write reachable from
ordinary Lisp, and because it appeared only after macOS 26.6: the same
configuration ran for eight months on macOS 26.0 through 26.5.2 without a single
crash.

## Reproduction

```elisp
;; Ask for a frame taller than the space the window server will give it.
;; display-pixel-height counts the whole screen including the menu bar;
;; frame-monitor-workarea does not.  On my display the difference is 31px,
;; and the title bar adds more on top of that.
(set-frame-size (selected-frame)
                (frame-width)
                (floor (display-pixel-height) (frame-char-height)))
```

Then scroll.  It is not immediate — it needs the scroll-run optimization to
actually fire — but under concentrated use it took minutes, not hours.  The two
things that triggered it most often for me:

- scrolling right after jumping somewhere via `imenu`
- scrolling a buffer while an `isearch` session has it filtered

Both scroll a window other than the one holding point, from inside a recursive
edit.  One crash caught that directly, with `Fread_from_minibuffer` and
`consult-imenu` on the stack below `redisplay_internal`.

Conversely, sizing the frame from `frame-monitor-workarea` instead — and
subtracting the frame's own chrome, `frame-outer-height` minus
`frame-text-height`, before dividing into rows — stopped both the crashes and a
persistent scroll stutter that had come with them.

## Backtrace

Identical in all ten crashes, modulo symbol offsets:

```
0   libsystem_kernel.dylib   __pthread_kill + 8
...
8   Emacs                    deliver_fatal_thread_signal + 128
9   libsystem_platform.dylib _sigtramp + 56
10  Emacs                    -[EmacsView copyRect:to:] + 336
11  Emacs                    ns_scroll_run + 420
12  Emacs                    update_window + 3696
13  Emacs                    update_window_tree + 120
14  Emacs                    update_frame + 140
15  Emacs                    redisplay_internal + 5360
16  Emacs                    read_char + 2620
17  Emacs                    read_key_sequence + 1388
18  Emacs                    command_loop_1 + 848
...
```

Four of the ten were `KERN_PROTECTION_FAILURE` — writes into a **read-only**
(`r--`) file mapping, which is the part I think matters most.  The other six
were `KERN_INVALID_ADDRESS` reads from unmapped memory.

In seven of the ten the fault address ends in `0x010`, and where it landed
inside a mapped region the crash reporter puts it exactly 16 bytes past that
region's start:

```
0x173424010 is in 0x173424000-0x173c1c000; bytes after start: 16
      mapped file  173424000-173c1c000  [ 8160K] r--/rwx SM=COW
```

Two different binaries, nine days apart, even show the identical preceding
`GAP OF 0x3548000 BYTES`.  This does not look like heap corruption to me; it
looks like a destination pointer that lands at the base of an unrelated mapping
and faults almost immediately.  I have not instrumented a build to confirm the
coordinates, so that is inference from the fault signature only.

## Relationship to bug#81585

Same crash site, different route to it.

bug#81585 (Corwin Kerr, 2026-08-09, closed 2026-08-12) needs
`pixel-scroll-precision-mode` plus toggling a header-line off while a line is
partially scrolled.  I use neither, and my first crash was on 2026-08-28, after
that bug was closed.

Alan Third's v2 patch from that thread clamps the **top** edge, in
`ns_scroll_run`:

```c
if (to_y < y) { int d = y - to_y; height -= d; to_y += d; from_y += d; }
```

and in `copyRect:to:`:

```c
if (dest.y < 0) { int d = dest.y; dest.y = 0;
                  srcRect.origin.y -= d; srcRect.size.height += d; }
```

Neither clamps the bottom edge or the height against the actual buffer.  My
write faults are consistent with running past the *end* of the destination
rather than above its start, so the existing clamps do not cover this case.
Eight of my ten crashes were on builds carrying that patch; the other two were
without it and crashed the same way.  So the patch neither causes nor prevents
this.

## Why I think this is worth fixing rather than "configure it properly"

I accept that my frame height was wrong, and I am not asking for my
configuration to be supported.  But:

- `set-frame-size` is a documented public function.  Passing it a large height
  should be clamped or signal an error, not produce an out-of-bounds write.
- The same configuration was harmless on macOS 26.0 through 26.5.2.  Whatever
  the window server used to do with an oversized frame, macOS 26.6 stopped
  doing, and the NS port has no check of its own.  That makes this a latent
  assumption that will start biting other people as 26.6 spreads.
- Other ports do not do this.  I had been running that same frame calculation
  for years.

## macOS timeline

```
26.2      2025-12-26     <- on macOS 26 from here
...
26.5.2    2026-07-14     <- eight months, same config, no crashes
26.6      2026-08-14  *
26.6.1    2026-08-20  *
                            first crash 2026-08-28, running 26.6.1
26.6.2    2026-08-29
```

Caveat: there is a 14-day gap between the 26.6 install and my first observed
crash, and I cannot say whether 26.6 or 26.6.1 is the relevant release — I may
simply not have hit the trigger in between.  I cannot downgrade to check.

## Not specific to a version, a build, or a patch set

Ten crashes, on five separate binaries, across two Emacs versions:

| # | when | version | build | fault |
|---|---|---|---|---|
| 1-3 | 2026-08-28 | 31.1 | built locally | read |
| 4-5 | 2026-09-08, 09-11 | 31.1 | built locally | write, read |
| 6-7 | 2026-09-16, 09-17 | 30.2 | built locally | read, write |
| 8-9 | 2026-09-17 | 31.1 | emacs-plus CI (GitHub `macos-26` runner) | read, write |
| 10 | 2026-09-17 | 31.1 | **vanilla, emacsformacosx.com** | write |

Crash 10 is stock upstream Emacs with no third-party patches, built on macOS 14
against the macOS 12 SDK (`LC_BUILD_VERSION minos 11.0 / sdk 12.0`,
`NS appkit-2487.70 Version 14.8.7`).  Crashes 1-9 were on builds with
`minos 26.0 / sdk 26.5`.  So neither the deployment target nor the SDK is the
factor.

Crashes 1-9 come from emacs-plus (the third-party Homebrew tap
d12frosted/emacs-plus).  Patches on those builds: `fix-ns-x-colors`,
`system-appearance`, `round-undecorated-frame`, `fix-ns-scroll-crash` (the
bug#81585 v2 patch), plus `fix-macos-tahoe-scrolling` on 30.2.  None touch the
scroll-copy path except `fix-ns-scroll-crash`.

This is also **not** bug#80268.  That covers Tahoe scrolling lag and input
handling; its fix is compiled into the affected builds, and setting
`NSEventConcurrentProcessingEnabled` and `NSApplicationUpdateCycleEnabled` to NO
for the app domain explicitly changed nothing.

## Environment

- macOS 26.6.2 (25G83); the three oldest crashes were on 26.6.1 (25G76)
- Mac15,12 (Apple M3), single external display
- DELL P2725QE, native 3840x2160, driven in a scaled HiDPI mode:
  `Resolution: 5120 x 2880`, `UI Looks like: 2560 x 1440 @ 100.00Hz`.
  Mentioned only for completeness — from the application's side this is
  indistinguishable from a native 5K panel at 2560x1440 @2x, since the
  downscale to the physical 3840x2160 happens in the window server.  What may
  matter is simply that the logical height is 1440 rather than the 1080 of this
  monitor's default mode, so the frame has more rows.
- `display-pixel-height` 1440, monitor workarea height 1409 — a 31px menu bar
- `pixel-scroll-precision-mode` **not** enabled, no header-line,
  `scroll-conservatively` 0, `scroll-margin` 0, `scroll-step` 1

## No Lisp-level workaround for the code path itself

The redisplay debug variables are not defined in a normal build, so there is no
way to turn the scroll-run optimization off from Lisp:

```
inhibit-try-window-id         boundp=nil
inhibit-try-window-reusing    boundp=nil
inhibit-try-cursor-movement   boundp=nil
```

Fixing the frame height is the only avenue a user has.

## Attachments

Original `.ips` files for crashes 5 through 10; crash 10 (the vanilla build) is
probably the one to start from.  Crashes 1-4 were rotated out of
`~/Library/Logs/DiagnosticReports/` before I saved them, so for those I have
excerpts rather than the originals.

I am happy to run an instrumented build, test a candidate patch, or collect
anything else useful.

---

## Status of this report — not part of the message

**Do not send until the recipe is verified.**  The causal claim currently rests
on removing the oversized frame and observing that both the crashes and the
stutter stopped.  That is strong, but it is one direction only.

Before sending, put the old sizing back deliberately and confirm it crashes
again.  If it does, the recipe above is real and this becomes a report with a
reproduction, which is what makes it actionable.  If it does not, the cause is
not yet established and this draft needs to go back to being the
"intermittent, cannot reproduce" version.

The stutter is the quicker signal — it came back immediately on every frame
redraw, rather than needing a crash to land.

Also unverified: whether the display mode matters at all.  Note that "scaled vs
not" is the wrong axis — the app cannot tell the difference, the downscale is
the window server's business.  If the default mode turns out to avoid the crash,
the reason would be the smaller logical height (1080 vs 1440), i.e. fewer rows
and a smaller buffer, not the absence of scaling.
