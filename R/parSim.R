#' @title
#' Parallel Simulator
#'
#' @description
#' The function [`parSim::parSim`] takes a set of conditions and an `R`
#' [`base::expression`] and returns a [`base::data.frame`] with simulation
#' results. The [`parSim::parSim`] function is based on [`dplyr::dplyr`]
#' functions for handling the results, and the [`parabar::parabar`] package for
#' handling parallelization and progress tracking.
#'
#' @param ... Any number of `R` vectors representing the simulation conditions.
#' For example, if you want to vary the sample size between `{100, 250, 1000}`
#' and a regression slope between `{0, 0.5, 1}`, you can assign the first two
#' arguments as `sample_size = c(100, 250, 1000)` and `beta = c(0, 0.5, 1)`.
#'
#' @param expression An `R` expression that uses the simulation conditions as
#' variable names. For example, if the `...` arguments are used to define
#' `sample_size = c(100, 250, 1000)`, then, within the context of `expression`,
#' you can access the `sample_size` variable, which may hold a value of `100`,
#' `250`, or `1000` depending on the simulation condition. The expression must
#' collect and output the results as a named [`base::list`] or a
#' [`base::data.frame`]. See the the *Examples* section for more details.
#'
#' @param replications An integer representing the number of times each
#' condition will be replicated. Please ensure that an adequate number of
#' simulations is used (i.e., the more the better). Defaults to `1`.
#'
#' @param exclude A list of logical calls indicating the simulation conditions
#' to exclude cases, written as formula. For example, `exclude = sample_size ==
#' 100 | beta == 0` will exclude all cases where `sample_size` is equal to `100`
#' or `beta` is equal to `0`. Similarly, `exclude = sample_size == 100 & beta ==
#' 0` will exclude all cases where `sample_size` is equal to `100` and `beta` is
#' equal to `0`. Defaults to `NULL` (i.e., no simulation conditions are
#' excluded).
#'
#' @param exports A character string containing the names of the objects to be
#' exported to the parallel backend. This argument is only relevant when using
#' parallel execution (i.e., `cores > 1`). Defaults to `NULL` (i.e., indicating
#' that no objects are to be exported).
#'
#' @param packages A character vector containing the names of the packages to be
#' loaded on the parallel backend. For example `packages = c("bootnet",
#' "qgraph")`. This argument is only relevant when using parallel execution
#' (i.e., `cores > 1`). Defaults to `NULL` (i.e., indicating that no packages
#' are to be loaded).
#'
#' @param save A logical or string value controlling whether and where to save
#' the results to a text file. If `save = TRUE` the results are saved to a
#' temporary file (i.e., created using [`base::tempfile`]). If `save` is a
#' string, the results are saved to the specified file. If `save = FALSE` the
#' results are not saved. Upon saving the results, a message is printed to the
#' console indicating the location of the saved file. Defaults to `FALSE`.
#'
#' @param cores An integer value indicating the number of cores to use for
#' parallel execution. Setting this argument to `1` implies that the simulation
#' conditions are run sequentially. Using a value greater than `1` implies that
#' the simulation conditions are run in parallel using a [`parabar::parabar`]
#' backend with the specified number of cores. Defaults to `1`.
#'
#' @param progress A logical value indicating whether to show a progress bar
#' while running the simulation. Defaults to `TRUE`.
#'
#' @details
#' The `R` [`base::expression`] should use object names assigned as conditions,
#' and should return a named list with single values, or a [`base::data.frame`].
#' If you want to output more than one row of results per condition, you may
#' return a [`base::data.frame`] with multiple rows. When running the simulation
#' conditions in parallel, note that all packages needed should be loaded via
#' the `packages` argument and all external objects used within the `expression`
#' should be exported via the `export` argument.
#'
#' @return
#' The [parSim::parSim()] function returns a [`base::data.frame`] with the
#' results of every iteration as a row.
#'
#' @examples
#' # Determine a function to evaluate for each simulation condition.
#' bias <- function(x, y) {
#'     # Perform some computation.
#'     result <- abs(x - y)
#'
#'     # Return the result.
#'     return(result)
#' }
#'
#' # Run the simulation.
#' results <- parSim(
#'     # The simulation conditions.
#'     sample_size = c(50, 100, 250),
#'     beta = c(0, 0.5, 1),
#'     sigma = c(0.25, 0.5, 1),
#'
#'     # The expression to evaluate for each simulation condition.
#'     expression = {
#'         # Generate the data.
#'         x <- rnorm(sample_size)
#'         y <- beta * x + rnorm(sample_size, sigma)
#'
#'         # Fit the model.
#'         fit <- lm(y ~ x)
#'
#'         # Compute the relevant quantities.
#'         beta_estimate <- coef(fit)[2]
#'         r_squared <- summary(fit)$r.squared
#'         bias <- bias(beta, beta_estimate)
#'
#'         # Return in a compatible format.
#'         list(
#'             beta_estimate = beta_estimate,
#'             r_squared = r_squared,
#'             bias = bias
#'         )
#'     },
#'
#'     # The number of replications.
#'     replications = 100,
#'
#'     # The conditions to exclude.
#'     exclude = sample_size == 50 | beta <= 0.5,
#'
#'     # The variables to export.
#'     exports = c("bias"),
#'
#'     # No packages are required for export.
#'     packages = NULL,
#'
#'     # Do not save the results.
#'     save = FALSE,
#'
#'     # Execute the simulation on a single core.
#'     cores = 1,
#'
#'     # Show the progress bar.
#'     progress = TRUE
#' )
#'
#' # Print the head of the results.
#' head(results)
#'
#' # Configure the progress bar.
#' configure_bar(
#'     type = "modern",
#'     format = "[:bar] [:percent] [:elapsed]",
#'     show_after = 0.15
#' )
#'
#' # Run the simulation again with more cores and the updated progress bar.
#' results <- parSim(
#'     # The simulation conditions.
#'     sample_size = c(50, 100, 250),
#'     beta = c(0, 0.5, 1),
#'     sigma = c(0.25, 0.5, 1),
#'
#'     # The expression to evaluate for each simulation condition.
#'     expression = {
#'         # Generate the data.
#'         x <- rnorm(sample_size)
#'         y <- beta * x + rnorm(sample_size, sigma)
#'
#'         # Fit the model.
#'         fit <- lm(y ~ x)
#'
#'         # Compute the relevant quantities.
#'         beta_estimate <- coef(fit)[2]
#'         r_squared <- summary(fit)$r.squared
#'         bias <- bias(beta, beta_estimate)
#'
#'         # Return in a compatible format.
#'         list(
#'             beta_estimate = beta_estimate,
#'             r_squared = r_squared,
#'             bias = bias
#'         )
#'     },
#'
#'     # The number of replications.
#'     replications = 1000,
#'
#'     # The conditions to exclude.
#'     exclude = sample_size == 50 | beta <= 0.5,
#'
#'     # The variables to export.
#'     exports = c("bias"),
#'
#'     # No packages are required for export.
#'     packages = NULL,
#'
#'     # Save the results to a temporary file.
#'     save = TRUE,
#'
#'     # Execute the simulation in parallel.
#'     cores = 2,
#'
#'     # Show the progress bar.
#'     progress = TRUE
#' )
#'
#' # Print the tail of the results.
#' tail(results)
#'
#' @seealso
#' [`bootnet::bootnet`], [parabar::par_lapply], and [parabar::configure_bar].
#'
#' @export
parSim <- function(
    # Simulation design.
    ...,

    # The expression to evaluate for each condition.
    expression,

    # The number replications for each condition.
    replications = 1,

    # An unquoted logical expression to exclude cases.
    exclude = NULL,

    # A character vector of objects to be exported to the cluster.
    exports = NULL,

    # A character vector of packages to be loaded on the cluster.
    packages = NULL,

    # Whether to write the results to a file and where.
    save = FALSE,

    # Number of cores for parallel execution.
    cores = 1,

    # Whether to show a progress bar.
    progress = TRUE
) {
    # Expand all conditions into a simulation design.
    design <- do.call(
        what = expand.grid,
        args = c(
            # The provided conditions.
            list(...),

            # The replications.
            list(
                replication = seq_len(replications),
                stringsAsFactors = FALSE
            )
        )
    )

    # Capture the exclusion expression.
    exclude <- substitute(exclude)

    # If the user wants to exclude certain conditions.
    if (!is.null(exclude)) {
        # Dispose of the excluded conditions.
        design <- design[!eval(exclude, design), ]
    }

    # Get the total number of conditions.
    n_conditions <- nrow(design)

    # Compute the sequence of conditions.
    conditions <- seq_len(n_conditions)

    # Randomize the order of the conditions.
    if (n_conditions > 1) {
        # Randomize.
        design <- design[sample(conditions), ]
    }

    # Attach an ID to each condition.
    design$id <- conditions

    # Capture the call and substitute any symbols in the current environment.
    expr <- substitute(expression)

    # Prepare the task function.
    task <- function(condition) {
        # Record the condition ID.
        id = design$id[condition]

        # Simulate the current condition and return.
        tryCatch(
            # Expression to try.
            expr = {
                # Run the actual simulation and return.
                result <- eval(expr, envir = design[condition, ])

                # Coerce to data frame.
                result <- as.data.frame(result)

                # Prepare the task output.
                result$id <- id
                result$error <- FALSE
                result$message <- NA

                # Return the result.
                return(result)
            },

            # Catch any errors.
            error = function(e) {
                # Return the parsed error.
                list(
                    id = id,
                    error = TRUE,
                    message = e$message
                )
            }
        )
    }

    # Execute the task in parallel if requested.
    if (cores > 1) {
        # Get the user's progress tracking preference.
        user_progress <- parabar::get_option("progress_track")

        # Sync the progress tracking.
        parabar::set_option("progress_track", progress)

        # Restore on exit (i.e., per `CRAN` policy).
        on.exit({
            # Set the progress tracking to the user's preference.
            parabar::set_option("progress_track", user_progress)
        })

        # Determine the backend type.
        backend_type <- if (progress) "async" else "sync"

        # Start a `parabar` backend.
        backend <- parabar::start_backend(
            # The number of cores.
            cores = cores,

            # The cluster type.
            cluster_type = "psock",

            # The backend type.
            backend_type = backend_type
        )

        # On function exit free the resources.
        on.exit({
            # Forcefully stop the backend.
            parabar::stop_backend(backend)
        })

        # Prepare required cluster exports.
        variables <- c("design", "expr")

        # Add any additional user exports.
        variables <- c(variables, exports, "packages")

        # Export objects to the cluster.
        parabar::export(
            # Where to export.
            backend = backend,

            # What to export.
            variables = variables,

            # From where to export (i.e., current function's environment).
            environment = environment()
        )

        # Load any required packages if provided (i.e., to keep the task neat).
        if (!is.null(packages)) {
            # Load the packages.
            parabar::evaluate(backend, {
                # Load each package.
                lapply(packages, library, character.only = TRUE)
            })
        }

        # Execute the task in parallel.
        results <- parabar::par_lapply(
            # The parallel backend.
            backend = backend,

            # The sequence of conditions.
            x = conditions,

            # The task.
            fun = task
        )
    }

    # If parallel execution is not requested.
    if (cores < 2) {
        # If progress tracking is requested.
        if (progress) {
            # # Get the type of progress bar to use.
            bar_type <- parabar::get_option("progress_bar_type")

            # Extract the bar configuration for the corresponding type.
            bar_config <- parabar::get_option("progress_bar_config")[[bar_type]]

            # Create a bar factory.
            bar_factory <- parabar::BarFactory$new()

            # Get the corresponding bar type.
            bar <- bar_factory$get(bar_type)

            # Create the progress bar.
            do.call(
                # The `Bar` method to call.
                bar$create,

                # Update the list accordingly.
                utils::modifyList(
                    list(total = n_conditions, initial = 0), bar_config
                )
            )

            # Prepare the task storage.
            results <- vector("list", n_conditions)

            # Execute the task.
            for (condition in conditions) {
                # Execute the task.
                results[[condition]] <- task(condition)

                # Update the progress bar.
                bar$update(condition)
            }

            # Terminate the progress bar.
            bar$terminate()
        } else {
            # Otherwise, execute sequentially without progress tracking.
            results <- parabar::par_lapply(NULL, conditions, task)
        }
    }

    # Bind them all.
    results <- dplyr::bind_rows(results)

    # Left join the results to the design by ID.
    output <- design %>%
        dplyr::left_join(results, by = "id")

    # If the user wants to save the results but did not provide a path.
    if (is.logical(save) && save) {
        # Generate a temporary file at the appropriate `OS` location.
        save <- tempfile(pattern = "parSim", fileext = ".txt")
    }

    # If the user wants to save the results.
    if (is.character(save)) {
        # Trim whitespace.
        save <- trimws(save)

        # If the filename provided is obviously invalid.
        if (save == "") {
            # Throw.
            stop("Invalid file name for 'save' argument.", call. = FALSE)
        }

        # Otherwise write the results.
        utils::write.table(x = output, file = save, row.names = FALSE)

        # Inform the user about the file location.
        message(paste0("Saved results at location: '", save, "'."))
    }

    # Return the output.
    return(output)
}
