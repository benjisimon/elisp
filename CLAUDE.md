# elisp

Ben's shared Emacs code, loaded from `load-path` by the private
`emacs.d` repo.

**This repo is public.** Code here is meant to be shareable. No config,
no private settings, no machine-specific values — those belong in
`emacs.d`.

## Comments

A comment says what problem the code solves, tersely. Nothing else.

Never write a comment that:

- describes *how* the problem is solved
- records other approaches that were considered
- explains unrelated code, or another package's bugs
- restates what the code does

The code is the source of truth. A comment that describes, say, how a
buffer name gets built is stale the moment the code changes.

Docstrings documenting inputs and outputs, per standard elisp
convention, are always worth writing.

If a piece of code is complex enough to want an inline comment, that is
the signal to pull it out into its own named function with a docstring
and call it.

## Files

```elisp
;;; bs-foo.el --- Additions to foo  -*- lexical-binding: t; -*-
...
(provide 'bs-foo)
```

No `;;; Commentary:`, `;;; Code:`, or `;;; bs-foo.el ends here`
scaffolding. Two spaces after a sentence period; `--` for an em dash.
