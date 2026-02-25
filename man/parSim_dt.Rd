\name{parSim_dt}
\alias{parSim_dt}

\title{
Parallel Simulator data.table version
}

\description{
The function uses the more efficient data.table in its internal code instead of dplyr, thereby speeding up the function itself and also allowing researchers to use data.table to speed up the code they want to simulate with parSim.
}

\usage{
parSim_dt(
    ...,
    expression,
    reps = 1,
    write = FALSE,
    name,
    nCores = 1,
    export,
    exclude,
    debug = FALSE,
    progressbar = TRUE,
    env = parent.frame()
)
}

\arguments{
    \item{...}{
        Any number of \code{R} vectors representing the simulation conditions.
    }

    \item{expression}{
        An \code{R} expression that uses the simulation conditions as variable
        names. The expression must return a named \code{\link[base:list]{list}}
        or a \code{\link[base:data.frame]{data.frame}}.
    }

    \item{reps}{
        Integer. The number of replications for each condition. Defaults to \code{1}.
    }

    \item{write}{
        Logical, should the results be written to a file instead of returned as
        a data frame? If \code{TRUE}, the \code{name} argument must be provided.
        Defaults to \code{FALSE}.
    }

    \item{name}{
        Name of the file if \code{write = TRUE}. A \code{.txt} extension is
        appended automatically.
    }

    \item{nCores}{
        An integer value indicating the number of cores to use for parallel
        execution. Defaults to \code{1}.
    }

    \item{export}{
        A character vector of global objects to export to the cluster.
    }

    \item{exclude}{
        A character vector of logical expressions to exclude cases. These are
        combined with \code{&} and evaluated in the context of the simulation
        design.
    }

    \item{debug}{
        Logical. If \code{TRUE}, prints iteration details during execution.
        Defaults to \code{FALSE}.
    }

    \item{progressbar}{
        Logical. If \code{TRUE}, shows a progress bar while running the
        simulation. Defaults to \code{TRUE}.
    }

    \item{env}{
        The environment from which to export variables specified in the
        \code{export} argument. Defaults to \code{\link[base:sys.parent]{parent.frame()}}
        (i.e., the caller's environment).
    }
}

\details{
The function uses data.table in its internal code instead of dplyr, thereby
speeding up the simulation and allowing researchers to use data.table in the
code they want to simulate with parSim.
}

\value{
A \code{\link[data.table:data.table]{data.table}} with the results of each
iteration in each row. Returns \code{NULL} if \code{write = TRUE}.
}

\author{
Xinkai Du <xinkai.du.xd@gmail.com>
}

\seealso{
\code{\link{parSim}}
}
