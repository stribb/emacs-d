# Emacs Upgrade Strategy

## Why

We run `emacs-plus@31` (Emacs 31.0.50, bleeding-edge HEAD build) from the
`d12frosted/emacs-plus` Homebrew tap to work around rendering bugs in the
stable Homebrew Emacs. This means Emacs links dynamically against several
native C libraries whose ABI can change between versions. An unsupervised
`brew upgrade` that bumps one of these deps without rebuilding Emacs can
cause segfaults, broken tree-sitter grammars, or native-comp failures.

## Pinned Homebrew formulas

Three formulas are pinned with `brew pin` to prevent `brew upgrade` from
touching them:

| Formula | Why it's pinned |
|---|---|
| `tree-sitter` | ABI version gates grammar loading; a bump breaks all compiled grammars |
| `libgccjit` | Tightly coupled to gcc; mismatch breaks native compilation |
| `gcc` | Must stay in sync with libgccjit |

`emacs-plus@31` itself is **not** pinned — it's a HEAD-only formula, so
`brew upgrade` won't touch it anyway, and pinning it prevents `brew reinstall`
from working.

Other deps (gnutls, librsvg, little-cms2, webp) have stable ABIs and are
safe to let float.

Check current pins: `brew list --pinned`

## Upgrade scripts

### `~/bin/upgrade-emacs` — native deps + Emacs rebuild

Unpins tree-sitter/libgccjit/gcc, upgrades them, rebuilds emacs-plus@31
against the new deps, re-pins, copies the new Emacs.app to ~/Applications,
and cleans stale eln-cache dirs. Run every ~4 weeks.

### `~/bin/upgrade-emacs-packages` — Elisp packages

Runs `straight-pull-all`, `straight-rebuild-all`, and `straight-freeze-versions`
in batch mode. Commit `straight/versions/default.el` afterward. Run every
~4 weeks, offset 2 weeks from the native upgrade.

## Schedule

Two repeating Google Tasks, 4-weekly, 2 weeks out of phase:

1. **Upgrade Emacs native deps** → run `upgrade-emacs`
2. **Upgrade Emacs packages** → run `upgrade-emacs-packages`

Upgrading native deps and Elisp packages separately means that if something
breaks, you know which layer caused it.
