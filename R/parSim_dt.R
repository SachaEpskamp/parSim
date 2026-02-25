parSim_dt <- function(
    ..., # Simulation conditions
    expression, # R expression ending in data.frame of results
    reps = 1,
    write = FALSE, # if TRUE, results are written instead returned as data frame
    name,
    nCores = 1,
    export, # character string of global objects to export to the cluster.
    exclude, # List with dplyr calls to exclude cases. Written as formula
    debug = FALSE,
    progressbar = TRUE,
    env = parent.frame()
){

  # Avoid "no visible binding for global variable" NOTEs from R CMD check:
  id <- errorMessage <- NULL

  if (write && missing(name)){
    stop("Provide the argument 'name' if write = TRUE")
  }

  # Collect the conditions:
  dots <- list(...)

  # Expand all conditions:
  AllConditions <- data.table::data.table(do.call(expand.grid, c(dots, list(rep = seq_len(reps), stringsAsFactors = FALSE))))

  # Exclude cases:
  if (!missing(exclude)) {
    AllConditions <- AllConditions[eval(parse(text = paste(exclude, collapse = " & ")))]
  }

  # Randomize:
  totCondition <- nrow(AllConditions)
  if (totCondition > 1) {
    AllConditions <- AllConditions[sample(seq_len(totCondition)), ]
  }

  # Total conditions:
  AllConditions[, id := seq_len(totCondition)]

  # Deparse the expression:
  expr <- as.expression(substitute(expression))

  # Prepare the task function:
  task <- function(i){
    if (debug){
      cat("\nRunning iteration:",i," / ",nrow(AllConditions),"\nTime:",as.character(Sys.time()),"\n")
      print(AllConditions[i,])
    }

    tryRes <- try(eval(expr, envir = AllConditions[i]), silent = TRUE)
    if (inherits(tryRes, "try-error")) {
      return(data.table::data.table(error = TRUE, errorMessage = as.character(tryRes), id = AllConditions$id[i]))
    }

    dt <- data.table::as.data.table(tryRes, keep.rownames = TRUE)
    dt[, `:=`(id = AllConditions$id[i], error = FALSE, errorMessage = '')]
    dt
  }

  if (nCores > 1){
    # Get the user's progress tracking preference.
    user_progress <- parabar::get_option("progress_track")

    # Sync the progress tracking.
    parabar::set_option("progress_track", progressbar)

    # Restore on exit.
    on.exit({
      parabar::set_option("progress_track", user_progress)
    })

    # Determine the backend type.
    backend_type <- if (progressbar) "async" else "sync"

    # Start a parabar backend.
    backend <- parabar::start_backend(
      cores = nCores,
      cluster_type = "psock",
      backend_type = backend_type
    )

    # On function exit free the resources.
    on.exit({
      parabar::stop_backend(backend)
    }, add = TRUE)

    # Export internal variables to the cluster.
    parabar::export(
      backend = backend,
      variables = c("AllConditions", "expr", "debug"),
      environment = environment()
    )

    # Export user variables from the caller's environment.
    if (!missing(export)){
      parabar::export(
        backend = backend,
        variables = export,
        environment = env
      )
    }

    # Ensure data.table is loaded on workers.
    parabar::evaluate(backend, {
      requireNamespace("data.table", quietly = TRUE)
    })

    # Execute the task in parallel.
    Results <- parabar::par_lapply(
      backend = backend,
      x = seq_len(totCondition),
      fun = task
    )

  } else {

    if (progressbar) {
      # Use parabar progress bar for sequential execution.
      bar_type <- parabar::get_option("progress_bar_type")
      bar_config <- parabar::get_option("progress_bar_config")[[bar_type]]
      bar_factory <- parabar::BarFactory$new()
      bar <- bar_factory$get(bar_type)

      do.call(
        bar$create,
        utils::modifyList(
          list(total = totCondition, initial = 0), bar_config
        )
      )

      Results <- vector("list", totCondition)

      for (i in seq_len(totCondition)) {
        Results[[i]] <- task(i)
        bar$update(i)
      }

      bar$terminate()
    } else {
      Results <- lapply(seq_len(totCondition), task)
    }
  }

  # merge the results into a data.table
  Results <- data.table::rbindlist(Results, fill = TRUE)
  Results[, errorMessage := as.character(errorMessage)]

  # left-join results to conditions
  AllResults <- merge(AllConditions, Results, by = "id", all.x = TRUE)

  if (write) {
    txtFile <- paste0(name,".txt")
    data.table::fwrite(AllResults, file = txtFile, sep = "\t", col.names = TRUE, append = FALSE)

    return(NULL)
  } else {
    return(AllResults)
  }
}
