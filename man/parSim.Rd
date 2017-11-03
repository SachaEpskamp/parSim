\name{parSim}
\alias{parSim}
\alias{parSim-package}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Parallel Simulator
}
\description{
Takes a set of conditions and an R expression and returns a data frame with simulation results. See details. 
}
\usage{
parSim(..., expression, reps = 1, write = FALSE, name = "parSim", nCores = 1, 
        export, exclude, debug = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{
Vectors indicating any number of conditions. For example, if you want to vary sample size between N = 100, 250, and 1000, and a regression slope between beta = 0, 0.5, and 1, you can assign as first two arguments \code{sampleSize = c(100, 250, 1000), beta = c(0, 0.5, 1)}. 
}
  \item{expression}{
An R expression that uses the conditions as object names. For example, if the conditions \code{sampleSize = c(100, 250, 1000)} is used, then in the R expression you can use the object \code{sampleSize}, which may be 100, 250 or 1000 depending on the simulation condition.
}
  \item{reps}{
Number of times each condition has to be replicated.
}
  \item{write}{
Logical, should the results be written to a file instead of returned as a dataframe?
}
  \item{name}{
Name of the file if \code{write = TRUE}
}
  \item{nCores}{
Number of cores to use. NOTE: Only setting \code{nCores} to 1 allows you to see a progress bar and to use \code{browser()} in the R expression for debugging.
}
  \item{export}{
A character string of objects to be exported. Only needed if \code{nCores} > 1.
}
  \item{exclude}{
A list with logical calls to exclude cases. Written as formula.
}
  \item{debug}{
Allows for some debugging controls and output. Not recommended. 
}
}
\details{
The R expression should use object names assigned as conditions, and should return a list with single values, or a data frame. If you want to output more than one row of results per condition, you may return a data frame with multiple rows. When using multiple cores, note that all packages should be loaded in the R expression, all objects needed should be exported using the export object, and you will not see a progress bar. 
}
\value{
A data frame with every simulation as a row.
}

\author{
Sacha Epskamp <mail@sachaepskamp.com>
}

\examples{
# Some function we might use:
bias <- function(x,y){abs(x-y)}

# Run the simulation:
Results <- parSim(
  # Any number of conditions:
  sampleSize = c(50, 100, 250),
  beta = c(0, 0.5, 1),
  sigma = c(0.25, 0.5, 1),
  
  # Number of repititions?
  reps = 50,
  
  # Parallel?
  nCores = 1,
  
  # Write to file?
  write = FALSE,
  
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
    
    # Make a data frame with one row to return results (multple rows is 
    # also possible but not reccomended):
    data.frame(
      betaEst = betaEst,
      bias = bias(beta,betaEst),
      Rsquared = Rsquared
    )
  }
)

# Analyze the results:
library("ggplot2")
library("tidyr")

# We want to plot bias and R-squared. Let's first make it long format:
Long <- gather(Results,metric,value,bias:Rsquared)

# Make factors with nice labels:
Long$sigmaFac <- factor(Long$sigma,levels = c(0.25,0.5,1), 
    labels = c("Sigma: 0.025", "Sigma: 0.5", "Sigma: 1"))

# Now let's plot:
g <- ggplot(Long, aes(x = factor(sampleSize), y = value, fill = factor(beta)))  +
  facet_grid(metric ~ sigmaFac, scales = "free") + 
  geom_boxplot() + 
  theme_bw() + 
  xlab("Sample size") + 
  ylab("") + 
  scale_fill_discrete("Beta")
print(g)
}
