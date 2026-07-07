# parSim 0.3.1 (development)

## Breaking changes / deprecations

* `parSim_dt()` now takes `replications` and `progress` as its primary
  arguments (harmonizing its API with `parSim()`). The old `reps` and
  `progressbar` arguments are deprecated: they are still accepted but issue a
  deprecation warning.
* The output columns of `parSim_dt()` have been renamed to match `parSim()`:
  `rep` -> `replication` and `errorMessage` -> `message`. The `message` column
  is now `NA` (rather than `""`) for successful rows.
* `parSim_dt(write = TRUE)` now writes to a temporary file when `name` is
  missing (instead of erroring), reports the path with a message, and returns
  the results invisibly instead of `NULL`.
* In both `parSim()` and `parSim_dt()`, design conditions passed via `...`
  whose names collide with a function argument (e.g. `replications`, `nCores`,
  `progress`) now trigger a warning, since they would otherwise silently become
  a crossed design factor.

## New features

* New `seed` argument in `parSim()` and `parSim_dt()`: one L'Ecuyer-CMRG RNG
  substream is derived per simulation condition, so a seeded simulation is
  fully reproducible and gives identical results for any value of `nCores`.
  The caller's RNG state and RNG kind are restored afterwards.

## Bug fixes

* `export`/`env` now also work when running sequentially (`nCores = 1`): the
  simulation expression (and `exclude`) are evaluated with the caller's
  environment as enclosure, so objects local to a calling function are found
  without being in the global environment. Previously this only worked in
  parallel runs (the completion of the fix for GitHub issue #10), making
  "works with 8 cores, fails with 1 core" a confusing failure mode.

* BREAKING BUG FIX: the `exclude` argument of `parSim_dt()` did the opposite of
  its documentation -- it *kept* only the rows matching the exclusion
  expressions instead of removing them. Matching rows are now removed. Multiple
  expressions are combined with OR (any match removes the row); previously they
  were combined with AND.

* `parSim_dt()` no longer adds a spurious `rn` column when the simulation
  expression returns a data frame (row names are dropped), so list and
  data-frame results now produce identical output schemas.
* Removed `LazyData: true` from DESCRIPTION (the package has no data
  directory), fixing an R CMD check NOTE.

# parSim 0.3.0

## Breaking changes

* The primary argument names have been restored to their original names from
  version 0.1.x: `nCores` (was `cores`), `write` (was `save`), and `name`. The
  new-style arguments `cores` and `save` are still accepted via `...` but will
  produce a deprecation warning and will be removed in a future release.

## New features

* Restored `parSim_dt()`, the data.table-based variant of `parSim()`. The
  parallel backend has been updated to use `parabar` (replacing `snow` and
  `pbapply`), while retaining `data.table` internals for efficient data
  handling.

* Added `env` argument to both `parSim()` and `parSim_dt()`, defaulting to
 `parent.frame()`. This fixes an issue where the `export` argument could not
  find variables defined in the caller's environment when `parSim()` was called
  from within a function (GitHub issue #10).

## Bug fixes

* Fixed "no visible binding for global variable" NOTE from R CMD check in
  `parSim_dt()`.

# parSim 0.2.0

* Parallelization backend changed from `snow` to `parabar`.
* Progress bar support via `parabar`.
* Added `configure_bar()` function (re-exported from `parabar`).
* Added `replications` argument (deprecating `reps`).
* Internal refactoring to use `dplyr` for result handling.

# parSim 0.1.5

* CRAN release.
* Added `parSim_dt()` function using `data.table` internals.

# parSim 0.1.4

* Added `exclude` argument to exclude simulation conditions.
* Added `nCores` argument for parallel execution.
* Added `write` and `name` arguments for saving results to file.
