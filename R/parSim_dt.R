parSim_dt <- function(
    ..., # Simulation conditions
    expression, # R expression ending in data.frame of results
    reps = 1,
    write = FALSE, # if TRUE, results are written instead returned as data frame
    name,
    nCores = 1,
    export, # character string of global objects to export to the cluster.
    exclude, # List with dplyr calls to exclude cases. Written as formula
    debug=FALSE,
    progressbar = TRUE
){
  
  
  if (write && missing(name)){
    stop("Provide the argument 'name' if write = TRUE")
  }
  
  if (progressbar){
    PAR_FUN <- pbapply::pblapply
  } else {
    if (nCores > 1){
      PAR_FUN <- snow::parLapply      
    } else {
      PAR_FUN <- lapply
    }
    
  }
  
  # Collect the condiitions:
  dots <- list(...)
  
  # Expand all conditions:
  AllConditions <- data.table(do.call(expand.grid, c(dots, list(rep = seq_len(reps), stringsAsFactors = FALSE))))
  
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
  
  if (nCores > 1){
    nClust <- nCores - 1
    
    
    ######################
    ## use Socket clusters
    if (Sys.getenv("RSTUDIO") == "1" && !nzchar(Sys.getenv("RSTUDIO_TERM")) && 
        Sys.info()["sysname"] == "Darwin" && gsub("\\..*","",getRversion()) == "4") {
      snow::setDefaultClusterOptions(setup_strategy = "sequential")
    }
    
    if (!debug){
      cl <- snow::makeSOCKcluster(nClust)  
    } else {
      cl <- snow::makeSOCKcluster(nClust, outfile = "clusterLOG.txt")
    }
    
    #     # Start clusters:
    #     cl <- makeCluster(getOption("cl.cores", nCores))
    #     
    # Export the sim conditions:
    snow::clusterExport(cl, c("AllConditions","expr","debug"), envir = environment())
    snow::clusterEvalQ(cl, requireNamespace("data.table", quietly = TRUE))
  
    # Export global objects:
    if (!missing(export)){
      snow::clusterExport(cl, export)  
    }
    
    # Run the loop:
    Results <- PAR_FUN(seq_len(totCondition), function(i){
      
      if (debug){
        cat("\nRunning iteration:",i," / ",nrow(AllConditions),"\nTime:",as.character(Sys.time()),"\n")
        print(AllConditions[i,])
      }
      
      tryRes <- try(eval(expr, envir = AllConditions[i]), silent = TRUE)
      if (is(tryRes, "try-error")) {
        return(data.table(error = TRUE, errorMessage = as.character(tryRes), id = AllConditions$id[i]))
      }
      
      df <- as.data.table(tryRes, keep.rownames = TRUE)
      df[, `:=` (id = AllConditions$id[i], error = FALSE, errorMessage = '')]
      df
    }, cl = cl)
    
    # Stop the cluster:
    snow::stopCluster(cl)
    
  } else {
    
    # Run the loop:
    Results <- PAR_FUN(seq_len(totCondition), function(i){
      
      if (debug){
        cat("\nRunning iteration:",i," / ",nrow(AllConditions),"\nTime:",as.character(Sys.time()),"\n")
        print(AllConditions[i,])
      }
      
      tryRes <- try(eval(expr, envir = AllConditions[i]), silent = TRUE)
      if (is(tryRes, "try-error")) {
        return(data.table(error = TRUE, errorMessage = as.character(tryRes), id = AllConditions$id[i]))
      }
      
      df <- as.data.table(tryRes, keep.rownames = TRUE)
      df[, `:=` (id = AllConditions$id[i], error = FALSE, errorMessage = '')]
      df
    })
  }
  
  # merge the results into a data.table
  Results <- rbindlist(Results, fill = TRUE)
  Results[, errorMessage := as.character(errorMessage)]
  
  AllResults <- merge(AllConditions, Results, by = "id", all.x = TRUE)
  
  if (write) {
    txtFile <- paste0(name,".txt")
    fwrite(AllResults, file = txtFile, sep = "\t", col.names = TRUE, row.names = FALSE, append = FALSE)
    
    return(NULL)
  } else {
    return(AllResults)
  }
}
