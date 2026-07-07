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
    write = FALSE,
    name,
    nCores = 1,
    progress = TRUE,
    seed = NULL,
    env = parent.frame()
)
}

\arguments{
    \item{...}{
        Any number of \code{R} vectors representing the simulation conditions.
        For example, if you want to vary the sample size between \verb{\{100,
        250, 1000\}} and a regression slope between \verb{\{0, 0.5, 1\}}, you
        can assign the first two arguments as \code{sample_size = c(100, 250,
        1000)} and \code{beta = c(0, 0.5, 1)}. Condition names should not
        collide with argument names of \code{parSim} or \code{parSim_dt}; a
        warning is given if they do. The columns \code{id}, \code{error},
        \code{message} and \code{replication} are reserved for the output.
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
        execution (i.e., \code{nCores > 1}). Defaults to \code{NULL} (i.e.,
        indicating that no objects are to be exported).
    }

    \item{packages}{
        A character vector containing the names of the packages to be loaded on
        the parallel backend. For example \code{packages = c("bootnet",
        "qgraph")}. This argument is only relevant when using parallel execution
        (i.e., \code{nCores > 1}). Defaults to \code{NULL} (i.e., indicating that
        no packages are to be loaded).
    }

    \item{write}{
        Logical, should the results be written to a file? If \code{write = TRUE}
        and \code{name} is not provided, results are saved to a temporary file
        (i.e., created using \code{\link[base:tempfile]{base::tempfile}}). If
        \code{write = TRUE} and \code{name} is provided, results are saved to
        \code{name.txt}. If \code{write = FALSE} the results are not saved.
        Upon saving the results, a message is printed to the console indicating
        the location of the saved file. Defaults to \code{FALSE}.
    }

    \item{name}{
        A character string specifying the base name of the file to save the
        results to (a \code{.txt} extension is appended automatically). Only
        used when \code{write = TRUE}.
    }

    \item{nCores}{
        An integer value indicating the number of cores to use for parallel
        execution. Setting this argument to \code{1} implies that the simulation
        conditions are run sequentially. Using a value greater than \code{1}
        implies that the simulation conditions are run in parallel using a
        \code{\link[parabar:parabar-package]{parabar::parabar}} backend with the
        specified number of cores. Defaults to \code{1}.
    }

    \item{progress}{
        A logical value indicating whether to show a progress bar while running
        the simulation. Defaults to \code{TRUE}.
    }

    \item{seed}{
        Optional integer. When supplied, one L'Ecuyer-CMRG random-number
        substream is derived per simulation condition (per design row), making
        the results fully reproducible \emph{and identical for any value of
        \code{nCores}}. The caller's RNG state is restored afterwards.
        Defaults to \code{NULL} (no seeding).
    }

    \item{env}{
        The environment from which to export variables specified in the
        \code{export} argument, and in which unresolved symbols of
        \code{expression} and \code{exclude} are looked up when running
        sequentially. Defaults to \code{\link[base:sys.parent]{parent.frame()}}
        (i.e., the caller's environment). This is relevant when calling
        \code{parSim} from within a function, where variables to export may not
        be in the global environment; \code{export}/\code{env} behave
        identically for sequential (\code{nCores = 1}) and parallel runs.
    }
}

\value{
    The \code{\link{parSim}} function returns a
    \code{\link[base:data.frame]{base::data.frame}} with the results of every
    iteration as a row, sorted by the \code{id} column (the row number of the
    condition in the fully expanded design). Besides the simulation conditions,
    the \code{replication} number and the columns returned by
    \code{expression}, the output contains: \code{id} (integer identifying the
    design row), \code{error} (logical; \code{TRUE} if the expression threw an
    error for this condition) and \code{message} (the error message, \code{NA}
    on success). Check \code{any(output$error)} after a run.
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
    write = FALSE,

    # Execute the simulation on a single core.
    nCores = 1,

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
    write = TRUE,

    # Execute the simulation in parallel.
    nCores = 2,

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
