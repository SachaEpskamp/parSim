#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly=TRUE)
if (length(args)==0){
  args <- 1
}
# These lines have to be on top!

# Install the package:
# library("devtools")
# install_github("sachaepskamp/parSim")
library("parSim")

# Some function we might use:
bias <- function(x,y){abs(x-y)}

# Run the simulation:
Results <- parSim(
  # Any number of conditions:
  sampleSize = c(50, 100, 250),
  beta = c(0, 0.5, 1),
  sigma = c(0.25, 0.5, 1),
  
  # Number of repititions?
  reps = 5,
  
  # Parallel?
  nCores = 16,
  
  # Write to file?
  write = TRUE,
  
  # Name of file:
  name = paste0("Simulation_v2_",args[1]),
  
  # Export objects (only needed when nCores > 1):
  export = c("bias"),
  
  # R expression:
  expression = {
    # Load all R packages in the expression if needed
    # library(...)
    
    # Want to debug? Enter browser() and run the function. Only works with nCores = 1!
    # browser()
    
    # Enter whatever codes you want. I can use the conditions as objects.
    X <- rnorm(sampleSize)
    Y <- beta * X + rnorm(sampleSize, sigma)
    fit <- lm(Y ~ X)
    betaEst <- coef(fit)[2]
    Rsquared <- summary(fit)$r.squared
    
    # Make a data frame with one row to return results (multple rows is also possible but not reccomended):
    data.frame(
      betaEst = betaEst,
      bias = bias(beta,betaEst),
      Rsquared = Rsquared
    )
  }
)

