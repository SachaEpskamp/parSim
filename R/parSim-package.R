# Imports.
#' @importFrom dplyr %>% filter bind_rows left_join
#' @importFrom parabar get_option set_option start_backend stop_backend
#' @importFrom parabar export evaluate par_lapply BarFactory
#' @importFrom rlang !!! as_quosures


# Re-exported functions for convenience.

#' @importFrom parabar configure_bar
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
