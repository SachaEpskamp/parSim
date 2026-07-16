# CRAN comments for parSim 0.4.0

This is a version update of the existing CRAN package parSim. The current
CRAN release is 0.3.1.

## Summary of the release

This release collects a number of correctness fixes and API harmonizations;
see NEWS.md for the full list. Highlights:

* Results are returned in deterministic expanded-design order (sorted by the
  `id` column) rather than in the randomized order produced by load
  balancing. The `id`, `error` and `message` output columns are documented.
* `parSim_dt()`'s API is harmonized with `parSim()`: `replications` and
  `progress` are now the primary argument names, with the previous `reps` and
  `progressbar` accepted but deprecated with a warning.
* Added a `seed` argument giving reproducible results that are identical for
  any value of `nCores`.
* Design conditions passed through `...` whose names collide with a function
  argument now warn instead of silently becoming a crossed design factor.
* Clear, early errors for degenerate designs and invalid input.
* Added a regression test suite under `tests/` (plain `stopifnot()` scripts,
  no new dependencies).
* Added Adela-Maria Isvoranu as a contributor.

Because this release contains the deprecations and behaviour changes listed
above, the version is bumped to 0.4.0 rather than a patch release.

## Test environments

* Local: macOS Sonoma 14.2.1 (aarch64-apple-darwin20), R 4.5.3 (2026-03-11)

## R CMD check --as-cran results

0 ERRORs, 0 WARNINGs, 2 NOTEs.

**NOTE 1 — flagged URL in the vignette:**

```
Found the following (possibly) invalid URLs:
  URL: https://www.surf.nl/en/lisa-computing-cluster-extra-computing-power-for-research
    From: inst/doc/supercomputer.html
    Status: 403
    Message: Forbidden
```

This link (to the Dutch Lisa computing cluster, used as the illustrative
example in the supercomputer vignette) returns 403 to automated requests. It
has been present in the vignette since it was written.

**NOTE 2 — local test machine:**

```
* checking HTML version of manual ... NOTE
Skipping checking HTML validation: 'tidy' doesn't look like recent enough
HTML Tidy.
```

macOS ships an outdated HTML Tidy, so this check is skipped locally; it is
expected to pass on the CRAN build machines.
