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
