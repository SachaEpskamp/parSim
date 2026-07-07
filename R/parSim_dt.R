# Avoid "no visible binding for global variable" NOTEs from R CMD check for the
# data.table columns assigned by reference below. Declared at namespace level so
# that base::message() remains usable inside the function body (a local dummy
# 'message <- NULL' would mask it):
utils::globalVariables(c("id", "message"))

parSim_dt <- function(
    ..., # Simulation conditions
    expression, # R expression ending in data.frame of results
    replications = 1,
    reps, # Deprecated, use 'replications'.
    write = FALSE, # if TRUE, results are written instead returned as data frame
    name,
    nCores = 1,
    export = NULL, # character string of global objects to export to the cluster.
    exclude, # List with dplyr calls to exclude cases. Written as formula
    debug = FALSE,
    progress = TRUE,
    progressbar, # Deprecated, use 'progress'.
    env = parent.frame(),
    seed = NULL # Seed for reproducible results (identical for any nCores).
){

  # Check deprecated 'reps' argument:
  if (!missing(reps)){
    warning("'reps' argument is deprecated, use 'replications' instead.", call. = FALSE)
    replications <- reps
  }

  # Check deprecated 'progressbar' argument:
  if (!missing(progressbar)){
    warning("'progressbar' argument is deprecated, use 'progress' instead.", call. = FALSE)
    if (missing(progress)) progress <- progressbar
  }

  # Validate nCores:
  nCores <- as.integer(nCores)
  if (length(nCores) != 1 || is.na(nCores) || nCores < 1){
    stop("'nCores' must be a single integer >= 1.", call. = FALSE)
  }

  # Collect the conditions:
  dots <- list(...)

  # Guard against design conditions that collide with argument names of
  # parSim()/parSim_dt() (e.g. passing replications = 100 to the function
  # that spells it differently would silently become a crossed design factor):
  reservedArgs <- c("replications","reps","progress","progressbar","nCores","cores",
                    "write","save","name","export","packages","exclude","expression",
                    "env","debug","seed")
  clash <- intersect(names(dots), reservedArgs)
  if (length(clash) > 0){
    warning("Design condition(s) ", paste0("'", clash, "'", collapse = ", "),
            " have the same name as a parSim/parSim_dt argument -- did you mean to pass them as arguments?",
            call. = FALSE)
  }

  # Expand all conditions:
  AllConditions <- data.table::data.table(do.call(expand.grid, c(dots, list(replication = seq_len(replications), stringsAsFactors = FALSE))))

  # Exclude cases: each element of 'exclude' is a logical expression; any row
  # matching at least one of them is REMOVED (elements are combined with OR,
  # each wrapped in parentheses so operator precedence cannot leak across):
  if (!missing(exclude)) {
    keep <- !eval(parse(text = paste0("(", paste(exclude, collapse = ") | ("), ")")),
                  envir = AllConditions, enclos = env)
    AllConditions <- AllConditions[keep]
  }

  totCondition <- nrow(AllConditions)

  # Reproducibility (see parSim.R): save/restore the caller's RNG state and
  # seed with L'Ecuyer-CMRG so the shuffle and the per-row streams below are
  # deterministic for any nCores:
  if (!is.null(seed)) {
    haveSeed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
    oldSeed <- if (haveSeed) get(".Random.seed", envir = globalenv()) else NULL
    oldKind <- RNGkind()
    on.exit({
      RNGkind(oldKind[1], normal.kind = oldKind[2], sample.kind = oldKind[3])
      if (!is.null(oldSeed)) {
        assign(".Random.seed", oldSeed, envir = globalenv())
      } else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
        rm(".Random.seed", envir = globalenv())
      }
    }, add = TRUE)
    set.seed(seed, kind = "L'Ecuyer-CMRG")
  }

  # Randomize:
  if (totCondition > 1) {
    AllConditions <- AllConditions[sample(seq_len(totCondition)), ]
  }

  # Total conditions:
  AllConditions[, id := seq_len(totCondition)]

  # One RNG substream per design row (indexed by id):
  streams <- NULL
  if (!is.null(seed)) {
    streams <- vector("list", totCondition)
    sstate <- get(".Random.seed", envir = globalenv())
    for (i in seq_len(totCondition)) {
      sstate <- parallel::nextRNGStream(sstate)
      streams[[i]] <- sstate
    }
  }

  # Deparse the expression:
  expr <- as.expression(substitute(expression))

  # Enclosure for symbol lookup (see parSim.R): caller's environment
  # sequentially, worker's global environment in parallel:
  enclosEnv <- if (nCores > 1) globalenv() else env

  # Prepare the task function:
  task <- function(i){
    # Use this condition's own RNG stream:
    if (!is.null(seed)) {
      assign(".Random.seed", streams[[AllConditions$id[i]]], envir = globalenv())
    }
    if (debug){
      cat("\nRunning iteration:",i," / ",nrow(AllConditions),"\nTime:",as.character(Sys.time()),"\n")
      print(AllConditions[i,])
    }

    tryRes <- try(eval(expr, envir = AllConditions[i], enclos = enclosEnv), silent = TRUE)
    if (inherits(tryRes, "try-error")) {
      return(data.table::data.table(error = TRUE, message = as.character(tryRes), id = AllConditions$id[i]))
    }

    dt <- data.table::as.data.table(tryRes)
    dt[, `:=`(id = AllConditions$id[i], error = FALSE, message = NA_character_)]
    dt
  }

  if (nCores > 1){
    # Get the user's progress tracking preference.
    user_progress <- parabar::get_option("progress_track")

    # Sync the progress tracking.
    parabar::set_option("progress_track", progress)

    # Restore on exit.
    on.exit({
      parabar::set_option("progress_track", user_progress)
    })

    # Determine the backend type.
    backend_type <- if (progress) "async" else "sync"

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
      variables = c("AllConditions", "expr", "debug", "enclosEnv", "streams", "seed"),
      environment = environment()
    )

    # Export user variables from the caller's environment.
    if (!is.null(export)){
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

    if (progress) {
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
  Results[, message := as.character(message)]

  # left-join results to conditions
  AllResults <- merge(AllConditions, Results, by = "id", all.x = TRUE)

  if (write) {
    txtFile <- if (!missing(name)) paste0(name, ".txt") else tempfile(pattern = "parSim", fileext = ".txt")
    data.table::fwrite(AllResults, file = txtFile, sep = "\t", col.names = TRUE, append = FALSE)
    message(paste0("Saved results at location: '", txtFile, "'."))

    return(invisible(AllResults))
  } else {
    return(AllResults)
  }
}
