# Regression tests for parSim (plain stopifnot-style, no test framework).
# Parallel (nCores=2) tests are skipped on CRAN; set NOT_CRAN=true to run
# everything.
library(parSim)
options(warn = 1)

ok <- 0L; fail <- 0L; fails <- character(0)
check <- function(label, expr){
  res <- tryCatch(isTRUE(expr), error = function(e) structure(FALSE, msg = conditionMessage(e)))
  if (isTRUE(res)) { ok <<- ok + 1L; cat("PASS:", label, "\n") }
  else { fail <<- fail + 1L; fails <<- c(fails, label)
         cat("FAIL:", label, if (!is.null(attr(res,"msg"))) paste0(" [", attr(res,"msg"), "]") else "", "\n") }
}
quiet <- function(expr) suppressWarnings(suppressMessages(expr))
not_cran <- identical(tolower(Sys.getenv("NOT_CRAN")), "true")
check_par <- function(label, expr) if (not_cran) check(label, expr) else cat("SKIP (CRAN):", label, "\n")


## ================= BASELINE =================
check("B1 parSim sequential basic run", {
  set.seed(1)
  r <- quiet(parSim(a = 1:2, b = c(10, 20),
                    expression = { list(s = a + b) },
                    replications = 3, nCores = 1, progress = FALSE))
  is.data.frame(r) && nrow(r) == 12 && all(r$s == r$a + r$b) && !any(r$error) })

check("B2 parSim exclude drops matching rows", {
  r <- quiet(parSim(a = 1:3, expression = { list(x = a) },
                    exclude = a == 2, nCores = 1, progress = FALSE))
  !any(r$a == 2) && nrow(r) == 2 })

check("B3 parSim error capturing", {
  r <- quiet(parSim(a = 1:2, expression = { if (a == 2) stop("boom"); list(x = a) },
                    nCores = 1, progress = FALSE))
  sum(r$error) == 1 && r$message[r$a == 2] == "boom" && !r$error[r$a == 1] })

check("B4 parSim_dt basic run", {
  r <- quiet(parSim_dt(a = 1:2, expression = { list(s = a * 2) },
                       reps = 2, nCores = 1, progressbar = FALSE))
  data.table::is.data.table(r) && nrow(r) == 4 && all(r$s == r$a * 2) })

check("B5 parSim multi-row results", {
  r <- quiet(parSim(a = 1:2, expression = { data.frame(x = c(a, a + 10)) },
                    nCores = 1, progress = FALSE))
  nrow(r) == 4 })

check("B6 parSim deprecated reps still works with warning", {
  w <- NULL
  r <- withCallingHandlers(
    quiet(parSim(a = 1, expression = list(x = 1), reps = 2, nCores = 1, progress = FALSE)),
    warning = function(cond){ w <<- c(w, conditionMessage(cond)); invokeRestart("muffleWarning") })
  nrow(r) == 2 })



## ================= item01 =================

# Item 1: parSim_dt exclude must REMOVE matching rows (OR-combined)
check("T1a parSim_dt exclude removes matching rows", {
  r <- quiet(parSim_dt(a = 1:3, expression = { list(x = 1) },
                       exclude = "a == 1", nCores = 1, progressbar = FALSE))
  !any(r$a == 1) && nrow(r) == 2 })

check("T1b parSim_dt multiple exclusions combine with OR", {
  r <- quiet(parSim_dt(a = 1:3, b = 1:2, expression = { list(x = 1) },
                       exclude = c("a == 1", "b == 2"), nCores = 1, progressbar = FALSE))
  !any(r$a == 1) && !any(r$b == 2) && nrow(r) == 2 })

check("T1c parSim_dt exclusion with operator precedence guarded", {
  r <- quiet(parSim_dt(a = 1:4, expression = { list(x = 1) },
                       exclude = c("a == 1 | a == 2", "a == 4"),
                       nCores = 1, progressbar = FALSE))
  identical(sort(unique(r$a)), 3L) || identical(sort(unique(r$a)), 3) })

check("T1d parSim unquoted exclude unchanged (regression)", {
  r <- quiet(parSim(a = 1:3, b = 1:2, expression = { list(x = 1) },
                    exclude = a == 1 | b == 2, nCores = 1, progress = FALSE))
  !any(r$a == 1) && !any(r$b == 2) && nrow(r) == 2 })


## ================= item02 =================

# Item 2: export/env must work identically in sequential and parallel runs
f_seq <- function(){
  localconst <- 42
  helper <- function(x) x * 10
  parSim(a = 1:2, expression = { list(v = helper(a) + localconst) },
         export = c("localconst","helper"), nCores = 1, progress = FALSE)
}
check("T2a parSim nCores=1 finds caller-local objects", {
  r <- quiet(f_seq())
  !any(r$error) && all(sort(r$v) == c(52, 62)) })

f_seq_dt <- function(){
  localconst <- 42
  helper <- function(x) x * 10
  parSim_dt(a = 1:2, expression = { list(v = helper(a) + localconst) },
            export = c("localconst","helper"), nCores = 1, progressbar = FALSE)
}
check("T2b parSim_dt nCores=1 finds caller-local objects", {
  r <- quiet(f_seq_dt())
  !any(r$error) && all(sort(r$v) == c(52, 62)) })

check("T2c parSim exclude can reference caller-local constants", {
  g <- function(){
    cutoff <- 2
    parSim(a = 1:3, expression = list(x = a), exclude = a >= cutoff,
           nCores = 1, progress = FALSE)
  }
  r <- quiet(g())
  nrow(r) == 1 && r$a == 1 })

check("T2d parSim_dt exclude can reference caller-local constants", {
  g <- function(){
    cutoff <- 2
    parSim_dt(a = 1:3, expression = list(x = a), exclude = "a >= cutoff",
              nCores = 1, progressbar = FALSE)
  }
  r <- quiet(g())
  nrow(r) == 1 && r$a == 1 })

f_par <- function(){
  localconst <- 42
  helper <- function(x) x * 10
  parSim(a = 1:2, expression = { list(v = helper(a) + localconst) },
         export = c("localconst","helper"), nCores = 2, progress = FALSE)
}
check_par("T2e parSim nCores=2 matches sequential result", {
  r <- quiet(f_par())
  !any(r$error) && all(sort(r$v) == c(52, 62)) })

f_par_dt <- function(){
  localconst <- 42
  helper <- function(x) x * 10
  parSim_dt(a = 1:2, expression = { list(v = helper(a) + localconst) },
            export = c("localconst","helper"), nCores = 2, progressbar = FALSE)
}
check_par("T2f parSim_dt nCores=2 matches sequential result", {
  r <- quiet(f_par_dt())
  !any(r$error) && all(sort(r$v) == c(52, 62)) })


## ================= item03 =================

# Item 3: seed argument — full reproducibility across nCores
sortdf <- function(r) { r <- as.data.frame(r); r[order(r$id), , drop = FALSE] }

check("T3a same seed twice gives identical results (nCores=1)", {
  r1 <- quiet(parSim(a = 1:3, expression = list(x = rnorm(1)),
                     replications = 2, nCores = 1, progress = FALSE, seed = 1))
  r2 <- quiet(parSim(a = 1:3, expression = list(x = rnorm(1)),
                     replications = 2, nCores = 1, progress = FALSE, seed = 1))
  identical(sortdf(r1)$x, sortdf(r2)$x) })

check("T3b different seeds give different results", {
  r1 <- quiet(parSim(a = 1:3, expression = list(x = rnorm(1)),
                     nCores = 1, progress = FALSE, seed = 1))
  r2 <- quiet(parSim(a = 1:3, expression = list(x = rnorm(1)),
                     nCores = 1, progress = FALSE, seed = 2))
  !identical(sortdf(r1)$x, sortdf(r2)$x) })

check_par("T3c nCores=1 equals nCores=2 for the same seed (parSim)", {
  r1 <- quiet(parSim(a = 1:3, expression = list(x = rnorm(1)),
                     replications = 2, nCores = 1, progress = FALSE, seed = 7))
  r2 <- quiet(parSim(a = 1:3, expression = list(x = rnorm(1)),
                     replications = 2, nCores = 2, progress = FALSE, seed = 7))
  isTRUE(all.equal(sortdf(r1)$x, sortdf(r2)$x)) })

check_par("T3d nCores=1 equals nCores=2 for the same seed (parSim_dt)", {
  r1 <- quiet(parSim_dt(a = 1:3, expression = list(x = rnorm(1)),
                        reps = 2, nCores = 1, progressbar = FALSE, seed = 7))
  r2 <- quiet(parSim_dt(a = 1:3, expression = list(x = rnorm(1)),
                        reps = 2, nCores = 2, progressbar = FALSE, seed = 7))
  isTRUE(all.equal(sortdf(r1)$x, sortdf(r2)$x)) })

check("T3e parSim and parSim_dt reproducible independently", {
  r1 <- quiet(parSim_dt(a = 1:2, expression = list(x = rnorm(1)),
                        nCores = 1, progressbar = FALSE, seed = 3))
  r2 <- quiet(parSim_dt(a = 1:2, expression = list(x = rnorm(1)),
                        nCores = 1, progressbar = FALSE, seed = 3))
  identical(sortdf(r1)$x, sortdf(r2)$x) })

check("T3f caller RNG state and kind restored after seeded run", {
  set.seed(123); baseline <- runif(3)
  set.seed(123)
  kindBefore <- RNGkind()
  invisible(quiet(parSim(a = 1:2, expression = list(x = rnorm(1)),
                         nCores = 1, progress = FALSE, seed = 99)))
  after <- runif(3)
  identical(baseline, after) && identical(kindBefore, RNGkind()) })

check("T3g seed=NULL leaves behavior unseeded (two runs differ)", {
  r1 <- quiet(parSim(a = 1, expression = list(x = rnorm(1)), nCores = 1, progress = FALSE))
  r2 <- quiet(parSim(a = 1, expression = list(x = rnorm(1)), nCores = 1, progress = FALSE))
  !identical(r1$x, r2$x) })


## ================= item04 =================

# Item 4: no spurious rn column; identical schema for list vs data.frame results
check("T4a data.frame result has no rn column", {
  r <- quiet(parSim_dt(a = 1:2, expression = { data.frame(x = 1:2) },
                       nCores = 1, progressbar = FALSE))
  !("rn" %in% names(r)) && nrow(r) == 4 })

check("T4b list and data.frame results give identical columns", {
  r1 <- quiet(parSim_dt(a = 1, expression = { list(x = 1) },
                        nCores = 1, progressbar = FALSE))
  r2 <- quiet(parSim_dt(a = 1, expression = { data.frame(x = 1) },
                        nCores = 1, progressbar = FALSE))
  identical(sort(names(r1)), sort(names(r2))) })


## ================= item05 =================

# Item 5: parSim_dt API harmonization
check("T5a parSim_dt replications is primary; reps deprecated with warning", {
  w <- NULL
  r1 <- quiet(parSim_dt(a = 1, expression = list(x = 1), replications = 3,
                        nCores = 1, progress = FALSE))
  r2 <- withCallingHandlers(
    suppressMessages(parSim_dt(a = 1, expression = list(x = 1), reps = 3,
                    nCores = 1, progress = FALSE)),
    warning = function(cond){ w <<- c(w, conditionMessage(cond)); invokeRestart("muffleWarning") })
  nrow(r1) == 3 && nrow(r2) == 3 && any(grepl("deprecated", w)) })

check("T5b parSim_dt output columns: replication and message (NA on success)", {
  r <- quiet(parSim_dt(a = 1, expression = list(x = 1), replications = 2,
                       nCores = 1, progress = FALSE))
  all(c("replication","message","error","id") %in% names(r)) &&
    !("rep" %in% names(r)) && !("errorMessage" %in% names(r)) &&
    all(is.na(r$message)) })

check("T5c parSim_dt error rows get message text", {
  r <- quiet(parSim_dt(a = 1:2, expression = { if (a == 2) stop("kaboom"); list(x = a) },
                       nCores = 1, progress = FALSE))
  sum(r$error) == 1 && grepl("kaboom", r$message[r$error]) && all(is.na(r$message[!r$error])) })

check("T5d parSim_dt progressbar deprecated alias still works", {
  w <- NULL
  r <- withCallingHandlers(
    suppressMessages(parSim_dt(a = 1, expression = list(x = 1), nCores = 1, progressbar = FALSE)),
    warning = function(cond){ w <<- c(w, conditionMessage(cond)); invokeRestart("muffleWarning") })
  nrow(r) == 1 && any(grepl("deprecated", w)) })

check("T5e parSim_dt write=TRUE without name uses tempfile and returns results", {
  owd <- setwd(tempdir()); on.exit(setwd(owd), add = TRUE)
  r <- quiet(parSim_dt(a = 1:2, expression = list(x = a), write = TRUE,
                       nCores = 1, progress = FALSE))
  data.table::is.data.table(r) && nrow(r) == 2 })

check("T5f parSim_dt write=TRUE with name writes name.txt and returns results", {
  owd <- setwd(tempdir()); on.exit(setwd(owd), add = TRUE)
  nm <- paste0("parsimtest", as.integer(runif(1, 1, 1e6)))
  r <- quiet(parSim_dt(a = 1:2, expression = list(x = a), write = TRUE, name = nm,
                       nCores = 1, progress = FALSE))
  wrote_ok <- file.exists(paste0(nm, ".txt")) && data.table::is.data.table(r)
  unlink(paste0(nm, ".txt"))
  wrote_ok })

check("T5g reserved-name warning fires in both functions", {
  w1 <- NULL
  invisible(withCallingHandlers(
    suppressMessages(parSim_dt(a = 1, save = c(1, 2), expression = list(x = 1), nCores = 1, progress = FALSE)),
    warning = function(cond){ w1 <<- c(w1, conditionMessage(cond)); invokeRestart("muffleWarning") }))
  w2 <- NULL
  invisible(withCallingHandlers(
    suppressMessages(parSim(a = 1, debug = c(TRUE, FALSE), expression = list(x = 1), nCores = 1, progress = FALSE)),
    warning = function(cond){ w2 <<- c(w2, conditionMessage(cond)); invokeRestart("muffleWarning") }))
  any(grepl("same name as a parSim", w1)) && any(grepl("same name as a parSim", w2)) })


## ================= item06 =================

# Item 6: deprecated-arg shim only fires for scalars; nCores validated
check("T6a vector 'cores' stays a design condition (no deprecation)", {
  w <- NULL
  r <- withCallingHandlers(
    suppressMessages(parSim(cores = c(2, 4), n = 1:2, expression = list(x = cores),
                            nCores = 1, progress = FALSE)),
    warning = function(cond){ w <<- c(w, conditionMessage(cond)); invokeRestart("muffleWarning") })
  "cores" %in% names(r) && nrow(r) == 4 && all(r$x == r$cores) &&
    !any(grepl("deprecated", w)) && any(grepl("same name as a parSim", w)) })

check("T6b scalar 'cores' still deprecates to nCores", {
  w <- NULL
  r <- withCallingHandlers(
    suppressMessages(parSim(a = 1, expression = list(x = 1), cores = 1, progress = FALSE)),
    warning = function(cond){ w <<- c(w, conditionMessage(cond)); invokeRestart("muffleWarning") })
  nrow(r) == 1 && any(grepl("deprecated", w)) && !("cores" %in% names(r)) })

check("T6c invalid nCores errors informatively (both functions)", {
  e1 <- tryCatch({ parSim(a = 1, expression = list(x = 1), nCores = c(1, 2), progress = FALSE); NULL },
                 error = function(e) conditionMessage(e))
  e2 <- tryCatch({ parSim_dt(a = 1, expression = list(x = 1), nCores = 0, progress = FALSE); NULL },
                 error = function(e) conditionMessage(e))
  !is.null(e1) && grepl("nCores", e1) && !is.null(e2) && grepl("nCores", e2) })


## ================= item07 =================

# Item 7: early, clear errors for degenerate designs
check("T7a all-excluded design errors clearly (parSim)", {
  e <- tryCatch({ parSim(a = 1:2, expression = list(x = 1), exclude = a > 0,
                         nCores = 1, progress = FALSE); NULL },
                error = function(e) conditionMessage(e))
  !is.null(e) && grepl("0 rows", e) })

check("T7b all-excluded design errors clearly (parSim_dt)", {
  e <- tryCatch({ parSim_dt(a = 1:2, expression = list(x = 1), exclude = "a > 0",
                            nCores = 1, progress = FALSE); NULL },
                error = function(e) conditionMessage(e))
  !is.null(e) && grepl("0 rows", e) })

check("T7c zero-length condition errors naming the condition", {
  e1 <- tryCatch({ parSim(a = integer(0), expression = list(x = 1),
                          nCores = 1, progress = FALSE); NULL },
                 error = function(e) conditionMessage(e))
  e2 <- tryCatch({ parSim_dt(a = integer(0), expression = list(x = 1),
                             nCores = 1, progress = FALSE); NULL },
                 error = function(e) conditionMessage(e))
  !is.null(e1) && grepl("'a'", e1) && !is.null(e2) && grepl("'a'", e2) })

check("T7d replications < 1 errors", {
  e <- tryCatch({ parSim(a = 1, expression = list(x = 1), replications = 0,
                         nCores = 1, progress = FALSE); NULL },
                error = function(e) conditionMessage(e))
  !is.null(e) && grepl("replications", e) })


## ================= item09 =================

# Item 9: deterministic output order (sorted by id = expanded-design order)
check("T9a parSim output sorted by id in expanded-design order", {
  r <- quiet(parSim(a = 1:2, b = c(10, 20), expression = list(x = a),
                    nCores = 1, progress = FALSE))
  identical(r$id, 1:4) && identical(r$a, rep(1:2, 2)) && identical(r$b, rep(c(10, 20), each = 2)) })

check("T9b parSim_dt output sorted by id in expanded-design order", {
  r <- quiet(parSim_dt(a = 1:2, b = c(10, 20), expression = list(x = a),
                       nCores = 1, progress = FALSE))
  identical(r$id, 1:4) && identical(r$a, rep(1:2, 2)) && identical(r$b, rep(c(10, 20), each = 2)) })

check_par("T9c order deterministic also in parallel", {
  r <- quiet(parSim(a = 1:3, expression = list(x = a), replications = 2,
                    nCores = 2, progress = FALSE, seed = 5))
  identical(r$id, 1:6) && !is.unsorted(r$id) })

check_par("T9d seeded results stay identical across nCores after reordering", {
  sortdf2 <- function(r){ r <- as.data.frame(r); r[order(r$id), , drop = FALSE] }
  r1 <- quiet(parSim(a = 1:3, expression = list(x = rnorm(1)),
                     nCores = 1, progress = FALSE, seed = 11))
  r2 <- quiet(parSim(a = 1:3, expression = list(x = rnorm(1)),
                     nCores = 2, progress = FALSE, seed = 11))
  isTRUE(all.equal(sortdf2(r1)$x, sortdf2(r2)$x)) &&
    identical(sortdf2(r1)$a, sortdf2(r2)$a) })


## ================= item10 =================

# Item 10: reserved/duplicate result-column handling
check("T10a reserved column renamed to id_result with one warning (parSim)", {
  w <- NULL
  r <- withCallingHandlers(
    suppressMessages(parSim(a = 1:2, expression = list(id = 999, x = a),
                            replications = 3, nCores = 1, progress = FALSE)),
    warning = function(cond){ w <<- c(w, conditionMessage(cond)); invokeRestart("muffleWarning") })
  all(r$id_result == 999) && identical(r$id, 1:6) &&
    sum(grepl("reserved column", w)) == 1 })

check("T10b reserved column renamed with one warning (parSim_dt)", {
  w <- NULL
  r <- withCallingHandlers(
    suppressMessages(parSim_dt(a = 1:2, expression = list(error = "oops", x = a),
                               replications = 2, nCores = 1, progress = FALSE)),
    warning = function(cond){ w <<- c(w, conditionMessage(cond)); invokeRestart("muffleWarning") })
  all(r$error_result == "oops") && all(r$error == FALSE) &&
    sum(grepl("reserved column", w)) == 1 })

check("T10c design-name duplicate warns and suffixes (parSim)", {
  w <- NULL
  r <- withCallingHandlers(
    suppressMessages(parSim(a = 1:2, expression = list(a = 99), nCores = 1, progress = FALSE)),
    warning = function(cond){ w <<- c(w, conditionMessage(cond)); invokeRestart("muffleWarning") })
  all(c("a.x","a.y") %in% names(r)) && all(r$a.y == 99) &&
    any(grepl("same name as design condition", w)) })

check("T10d design-name duplicate warns and suffixes (parSim_dt)", {
  w <- NULL
  r <- withCallingHandlers(
    suppressMessages(parSim_dt(a = 1:2, expression = list(a = 99), nCores = 1, progress = FALSE)),
    warning = function(cond){ w <<- c(w, conditionMessage(cond)); invokeRestart("muffleWarning") })
  all(c("a.x","a.y") %in% names(r)) && any(grepl("same name as design condition", w)) })


## ================= item11minor =================

# Minor observations: packages parity
check("T11a parSim_dt accepts packages argument (sequential no-op)", {
  r <- quiet(parSim_dt(a = 1, expression = list(x = 1), packages = "stats",
                       nCores = 1, progress = FALSE))
  nrow(r) == 1 })

check_par("T11b parSim_dt packages loads on workers (parallel)", {
  r <- quiet(parSim_dt(a = 1:2, expression = { list(x = median(c(a, a + 2))) },
                       packages = "stats", nCores = 2, progress = FALSE))
  !any(r$error) && all(r$x == r$a + 1) })


cat("\n==== RESULT:", ok, "passed,", fail, "failed ====\n")
if (fail > 0) stop("parSim regression tests failed: ", paste(fails, collapse = "; "))
