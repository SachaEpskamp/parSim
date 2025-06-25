% Documentation for the `parSim::parSim` function.

\name{parSim}
\alias{parSim}
\title{Parallel Simulator}

\usage{
parSim(
    ...,
    expression,
    replications = 1,
    reps,
    exclude = NULL,
    export = NULL,
    packages = NULL,
    save = FALSE,
    write,
    name,
    cores = 1,
    nCores,
    progress = TRUE
)
}

\arguments{
    \item{...}{
        Any number of \code{R} vectors representing the simulation conditions.
        For example, if you want to vary the sample size between \verb{\{100,
        250, 1000\}} and a regression slope between \verb{\{0, 0.5, 1\}}, you
        can assign the first two arguments as \code{sample_size = c(100, 250,
        1000)} and \code{beta = c(0, 0.5, 1)}.
    }

    \item{expression}{
        An \code{R} expression that uses the simulation conditions as variable
        names. For example, if the \code{...} arguments are used to define
        \code{sample_size = c(100, 250, 1000)}, then, within the context of
        \code{expression}, you can access the \code{sample_size} variable, which
        may hold a value of \code{100}, \code{250}, or \code{1000} depending on
        the simulation condition. The expression must collect and output the
        results as a named \code{\link[base:list]{base::list}} or a
        \code{\link[base:data.frame]{base::data.frame}}. See the the
        \emph{Examples} section for more details.
    }

    \item{replications}{
        An integer representing the number of times each condition will be
        replicated. Please ensure that an adequate number of simulations is used
        (i.e., the more the better). Defaults to \code{1}.
    }
    
       \item{reps}{
        Deprecated alternative to 'replications'
    }

    \item{exclude}{
        A list of logical calls indicating the simulation conditions to exclude
        cases, written as formula. For example, \code{exclude = sample_size ==
        100 | beta == 0} will exclude all cases where \code{sample_size} is
        equal to \code{100} or \code{beta} is equal to \code{0}. Similarly,
        \code{exclude = sample_size == 100 & beta == 0} will exclude all cases
        where \code{sample_size} is equal to \code{100} and \code{beta} is equal
        to \code{0}. Defaults to \code{NULL} (i.e., no simulation conditions are
        excluded).
    }

    \item{export}{
        A character string containing the names of the objects to be exported to
        the parallel backend. This argument is only relevant when using parallel
        execution (i.e., \code{cores > 1}). Defaults to \code{NULL} (i.e.,
        indicating that no objects are to be exported).
    }

    \item{packages}{
        A character vector containing the names of the packages to be loaded on
        the parallel backend. For example \code{packages = c("bootnet",
        "qgraph")}. This argument is only relevant when using parallel execution
        (i.e., \code{cores > 1}). Defaults to \code{NULL} (i.e., indicating that
        no packages are to be loaded).
    }

    \item{save}{
        A logical or string value controlling whether and where to save the
        results to a text file. If \code{save = TRUE} the results are saved to a
        temporary file (i.e., created using
        \code{\link[base:tempfile]{base::tempfile}}). If \code{save} is a
        string, the results are saved to the specified file. If \code{save =
        FALSE} the results are not saved. Upon saving the results, a message is
        printed to the console indicating the location of the saved file.
        Defaults to \code{FALSE}.
    }

    \item{cores}{
        An integer value indicating the number of cores to use for parallel
        execution. Setting this argument to \code{1} implies that the simulation
        conditions are run sequentially. Using a value greater than \code{1}
        implies that the simulation conditions are run in parallel using a
        \code{\link[parabar:parabar-package]{parabar::parabar}} backend with the
        specified number of cores. Defaults to \code{1}.
    }
    
        \item{nCores}{
       Deprecated alternative to 'cores'
    }


    \item{progress}{
        A logical value indicating whether to show a progress bar while running
        the simulation. Defaults to \code{TRUE}.
    }
      \item{write}{
DEPRECATED: Logical, should the results be written to a file instead of returned as a dataframe?
}
  \item{name}{
DEPRECATED: Name of the file if \code{write = TRUE}
}
}

\value{
    The \code{\link{parSim}} function returns a
    \code{\link[base:data.frame]{base::data.frame}} with the results of every
    iteration as a row.
}

\description{
    The function \code{\link{parSim}} takes a set of conditions and an \code{R}
    \code{\link[base:expression]{base::expression}} and returns a
    \code{\link[base:data.frame]{base::data.frame}} with simulation results. The
    \code{\link{parSim}} function is based on
    \code{\link[dplyr:dplyr-package]{dplyr::dplyr}} functions for handling the
    results, and the \code{\link[parabar:parabar-package]{parabar::parabar}}
    package for handling parallelization and progress tracking.
}

\details{
    The \code{R} \code{\link[base:expression]{base::expression}} should use
    object names assigned as conditions, and should return a named list with
    single values, or a \code{\link[base:data.frame]{base::data.frame}}. If you
    want to output more than one row of results per condition, you may return a
    \code{\link[base:data.frame]{base::data.frame}} with multiple rows. When
    running the simulation conditions in parallel, note that all packages needed
    should be loaded via the \code{packages} argument and all external objects
    used within the \code{expression} should be exported via the \code{export}
    argument.
}

\examples{
# Determine a function to evaluate for each simulation condition.
bias <- function(x, y) {
    # Perform some computation.
    result <- abs(x - y)

    # Return the result.
    return(result)
}

# Run the simulation.
results <- parSim(
    # The simulation conditions.
    sample_size = c(50, 100, 250),
    beta = c(0, 0.5, 1),
    sigma = c(0.25, 0.5, 1),

    # The expression to evaluate for each simulation condition.
    expression = {
        # Generate the data.
        x <- rnorm(sample_size)
        y <- beta * x + rnorm(sample_size, sigma)

        # Fit the model.
        fit <- lm(y ~ x)

        # Compute the relevant quantities.
        beta_estimate <- coef(fit)[2]
        r_squared <- summary(fit)$r.squared
        bias <- bias(beta, beta_estimate)

        # Return in a compatible format.
        list(
            beta_estimate = beta_estimate,
            r_squared = r_squared,
            bias = bias
        )
    },

    # The number of replications.
    replications = 100,

    # The conditions to exclude.
    exclude = sample_size == 50 | beta <= 0.5,

    # The variables to export.
    export = c("bias"),

    # No packages are required for export.
    packages = NULL,

    # Do not save the results.
    save = FALSE,

    # Execute the simulation on a single core.
    cores = 1,

    # Show the progress bar.
    progress = TRUE
)

# Print the head of the results.
head(results)

# Configure the progress bar.
configure_bar(
    type = "modern",
    format = "[:bar] [:percent] [:elapsed]",
    show_after = 0.15
)

# Run the simulation again with more cores and the updated progress bar.
results <- parSim(
    # The simulation conditions.
    sample_size = c(50, 100, 250),
    beta = c(0, 0.5, 1),
    sigma = c(0.25, 0.5, 1),

    # The expression to evaluate for each simulation condition.
    expression = {
        # Generate the data.
        x <- rnorm(sample_size)
        y <- beta * x + rnorm(sample_size, sigma)

        # Fit the model.
        fit <- lm(y ~ x)

        # Compute the relevant quantities.
        beta_estimate <- coef(fit)[2]
        r_squared <- summary(fit)$r.squared
        bias <- bias(beta, beta_estimate)

        # Return in a compatible format.
        list(
            beta_estimate = beta_estimate,
            r_squared = r_squared,
            bias = bias
        )
    },

    # The number of replications.
    replications = 1000,

    # The conditions to exclude.
    exclude = sample_size == 50 | beta <= 0.5,

    # The variables to export.
    export = c("bias"),

    # No packages are required for export.
    packages = NULL,

    # Save the results to a temporary file.
    save = TRUE,

    # Execute the simulation in parallel.
    cores = 2,

    # Show the progress bar.
    progress = TRUE
)

# Print the tail of the results.
tail(results)
}

\seealso{
    \code{\link[bootnet:bootnet]{bootnet::bootnet}},
    \link[parabar:par_lapply]{parabar::par_lapply}, and
    \link[parabar:configure_bar]{parabar::configure_bar}.
}
