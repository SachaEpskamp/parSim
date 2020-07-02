parSim <- function(
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
      PAR_FUN <- parallel::parLapply      
    } else {
      PAR_FUN <- lapply
    }

  }
  
  # Collect the condiitions:
  dots <- list(...)
  
  # Expand all conditions:
  AllConditions <- do.call(expand.grid,c(dots,list(rep=seq_len(reps),stringsAsFactors=FALSE)))
  
  # Exclude cases:
  if (!missing(exclude)){
    AllConditions <- AllConditions %>% dplyr::filter_(.dots = exclude)
  }
  
  
  # Randomize:
  totCondition <- nrow(AllConditions)
  if (totCondition > 1){
    AllConditions <- AllConditions[sample(seq_len(totCondition)),]
  }
  
  # Total conditions:
  
  AllConditions$id <- seq_len(totCondition)
  
  # Deparse the expression:
  expr <- as.expression(substitute(expression))
 
  if (nCores > 1){
    nClust <- nCores - 1
    
    
    ######################
    ## use Socket clusters
    if (Sys.getenv("RSTUDIO") == "1" && !nzchar(Sys.getenv("RSTUDIO_TERM")) && 
        Sys.info()["sysname"] == "Darwin" && gsub("\\..*","",getRversion()) == "4") {
      parallel:::setDefaultClusterOptions(setup_strategy = "sequential")
    }
    
    if (!debug){
      cl <- parallel::makePSOCKcluster(nClust)  
    } else {
      cl <- parallel::makePSOCKcluster(nClust, outfile = "clusterLOG.txt")
    }
    
    #     # Start clusters:
    #     cl <- makeCluster(getOption("cl.cores", nCores))
    #     
    # Export the sim conditions:
    parallel::clusterExport(cl, c("AllConditions","expr","debug"), envir = environment())
    
    # Export global objects:
    if (!missing(export)){
      parallel::clusterExport(cl, export)  
    }
    
    # Run the loop:
    Results <- PAR_FUN(seq_len(totCondition), function(i){
      
      if (debug){
        cat("\nRunning iteration:",i," / ",nrow(AllConditions),"\nTime:",as.character(Sys.time()),"\n")
        print(AllConditions[i,])
      }
      
      tryRes <- try(df <- eval(expr, envir = AllConditions[i,]))
      if (is(tryRes,"try-error")){
        if (debug){
          browser()
        }
        return(list(error = TRUE, errorMessage = as.character(tryRes), id = AllConditions$id[i]))
      }
      df <- as.data.frame(df)
      df$id <- AllConditions$id[i]
      df$error <- FALSE
      df$errorMessage <- ''
      df
    }, cl = cl)
    
    # Stop the cluster:
    stopCluster(cl)
  } else {
    
    # Run the loop:
    Results <- PAR_FUN(seq_len(totCondition), function(i){
      
      if (debug){
        cat("\nRunning iteration:",i," / ",nrow(AllConditions),"\nTime:",as.character(Sys.time()),"\n")
        print(AllConditions[i,])
      }
      
      
      tryRes <- try(df <- eval(expr, envir = AllConditions[i,]))
      if (is(tryRes,"try-error")){
        return(list(error = TRUE, errorMessage = as.character(tryRes), id = AllConditions$id[i]))
      }
      
      df <- as.data.frame(df)
      df$id <- AllConditions$id[i]
      df$error <- FALSE
      df$errorMessage <- ''
      df
    })
  }
  
  # rbind the list:
  Results <- dplyr::bind_rows(Results)
  Results$errorMessage <- as.character(Results$errorMessage)
  
  # Left join the results to the conditions:
  AllResults <- AllConditions %>% dplyr::left_join(Results, by = "id")
  
  if (write){
    txtFile <- paste0(name,".txt")
    # if (!file.exists(txtFile)){
    write.table(AllResults, file = txtFile, col.names=TRUE,
                row.names = FALSE, append=FALSE)   
    #     } else {
    #       write.table(AllResults, file = txtFile, col.names=FALSE,
    #                   row.names = FALSE, append=TRUE)      
    #     }
    
    return(NULL)
  } else {
    return(AllResults)
  }
}
