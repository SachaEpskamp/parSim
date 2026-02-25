# Parallel simulator function.
parSim <- function(
    # Simulation design.
    ...,

    # The expression to evaluate for each condition.
    expression,

    # The number replications for each condition.
    replications = 1,

    # Deprecated:
    reps,

    # An unquoted logical expression to exclude cases.
    exclude = NULL,

    # A character vector of objects to be exported to the cluster.
    export = NULL,

    # A character vector of packages to be loaded on the cluster.
    packages = NULL,

    # Whether to write the results to a file.
    write = FALSE,

    # Name of the file (without .txt extension).
    name,

    # Number of cores for parallel execution.
    nCores = 1,

    # Whether to show a progress bar.
    progress = TRUE,

    # Environment from which to export variables.
    env = parent.frame()
) {

  # Check deprecated 'reps' argument:
  if (!missing(reps)){
    warning("'reps' argument is deprecated, use 'replications' instead.", call. = FALSE)
    replications <- reps
  }

  # Capture dots and check for deprecated new-style argument names:
  dots <- list(...)

  if ("cores" %in% names(dots)){
    warning("'cores' argument is deprecated and will be removed soon, use 'nCores' instead.", call. = FALSE)
    nCores <- dots[["cores"]]
    dots[["cores"]] <- NULL
  }

  # Determine save path:
  save_path <- NULL

  if ("save" %in% names(dots)){
    warning("'save' argument is deprecated and will be removed soon, use 'write' and 'name' instead.", call. = FALSE)
    save_val <- dots[["save"]]
    dots[["save"]] <- NULL
    if (is.character(save_val)){
      save_path <- save_val
    } else if (isTRUE(save_val)) {
      save_path <- tempfile(pattern = "parSim", fileext = ".txt")
    }
  } else if (isTRUE(write)) {
    if (!missing(name)) {
      save_path <- paste0(name, ".txt")
    } else {
      save_path <- tempfile(pattern = "parSim", fileext = ".txt")
    }
  }

    # Expand all conditions into a simulation design.
    design <- do.call(
        what = expand.grid,
        args = c(
            # The provided conditions.
            dots,

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
    if (nCores > 1) {
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
            cores = nCores,

            # The cluster type.
            cluster_type = "psock",

            # The backend type.
            backend_type = backend_type
        )

        # On function exit free the resources.
        on.exit({
            # Forcefully stop the backend.
            parabar::stop_backend(backend)
        }, add = TRUE)

        # Export internal variables to the cluster.
        parabar::export(
            backend = backend,
            variables = c("design", "expr", "packages"),
            environment = environment()
        )

        # Export user variables from the caller's environment.
        if (!is.null(export)) {
            parabar::export(
                backend = backend,
                variables = export,
                environment = env
            )
        }

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
    if (nCores < 2) {
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

    # Save results if requested.
    if (!is.null(save_path)) {
        save_path <- trimws(save_path)

        if (save_path == "") {
            stop("Invalid file name for 'name' argument.", call. = FALSE)
        }

        utils::write.table(x = output, file = save_path, row.names = FALSE)
        message(paste0("Saved results at location: '", save_path, "'."))
    }

    # Return the output.
    return(output)
}
