# Imports.
#' @importFrom dplyr %>% filter bind_rows left_join
#' @importFrom parabar get_option set_option start_backend stop_backend
#' @importFrom parabar export evaluate par_lapply BarFactory
#' @importFrom utils modifyList write.table


# Re-exported functions for convenience.

#' @title Configure The Progress Bar
#'
#' @description
#' This function can be used to conveniently configure the progress bar. It is
#' exported for convenience from the [`parabar::parabar`] package. Please
#' consult its documentation at [`parabar::configure_bar`] for more information
#' on how to use it.
#'
#' @param type A character string specifying the type of progress bar to be used
#'  with compatible [`backends`][`parabar::Backend`]. Possible values are
#' `"modern"` and `"basic"`. The default value is `"modern"`.
#'
#' @param ... A list of named arguments used to configure the progress bar. See
#' the **Details** section for more information.
#'
#' @importFrom parabar configure_bar
#' @name configure_bar
#'
#' @export
parabar::configure_bar


#' @title
#' Parallel Simulation Studies
#'
#' @description
#' Perform flexible simulation studies using one or multiple computer cores. The
#' package is set up to be usable on high-performance clusters in addition to
#' being run locally (i.e., see the package vignettes for more information).
#'
#' @aliases parSim-package
#'
#' @keywords internal
"_PACKAGE"
