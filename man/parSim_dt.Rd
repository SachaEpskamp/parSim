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
    replications = 1,
    reps,
    write = FALSE,
    name,
    nCores = 1,
    export = NULL,
    exclude,
    debug = FALSE,
    progress = TRUE,
    progressbar,
    env = parent.frame(),
    seed = NULL
)
}

\arguments{
    \item{...}{
        Any number of \code{R} vectors representing the simulation conditions.
    }

    \item{expression}{
        An \code{R} expression that uses the simulation conditions as variable
        names. The expression must return a named \code{\link[base:list]{list}}
        or a \code{\link[base:data.frame]{data.frame}}. Row names of returned
        data frames are dropped; put identifiers in a proper column if needed.
    }

    \item{replications}{
        Integer. The number of replications for each condition. Defaults to \code{1}.
    }

    \item{reps}{
        Deprecated. Alias for \code{replications}, kept for backward
        compatibility; if supplied it overrides \code{replications} and a
        deprecation warning is issued.
    }

    \item{write}{
        Logical, should the results be written to a file? If \code{TRUE}, the
        results are written to \code{paste0(name, ".txt")} when \code{name} is
        provided, or to a temporary file (via
        \code{\link[base:tempfile]{tempfile}}) otherwise, and the path is
        reported with a message. Unlike previous versions, the results are also
        returned (invisibly) rather than \code{NULL}. Defaults to \code{FALSE}.
    }

    \item{name}{
        Name of the file if \code{write = TRUE}. A \code{.txt} extension is
        appended automatically. If omitted, a temporary file is used.
    }

    \item{nCores}{
        An integer value indicating the number of cores to use for parallel
        execution. Defaults to \code{1}.
    }

    \item{export}{
        A character vector of global objects to export to the cluster.
        Defaults to \code{NULL} (nothing exported).
    }

    \item{exclude}{
        A character vector of logical expressions indicating simulation
        conditions to exclude. The expressions are combined with \code{|}
        (OR) and evaluated in the context of the simulation design: any row
        matching at least one expression is removed.
    }

    \item{debug}{
        Logical. If \code{TRUE}, prints iteration details during execution.
        Defaults to \code{FALSE}.
    }

    \item{progress}{
        Logical. If \code{TRUE}, shows a progress bar while running the
        simulation. Defaults to \code{TRUE}.
    }

    \item{progressbar}{
        Deprecated. Alias for \code{progress}, kept for backward
        compatibility; if supplied (and \code{progress} is not) it sets
        \code{progress} and a deprecation warning is issued.
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
        (i.e., the caller's environment). \code{export}/\code{env} behave
        identically for sequential (\code{nCores = 1}) and parallel runs.
    }
}

\details{
The function uses data.table in its internal code instead of dplyr, thereby
speeding up the simulation and allowing researchers to use data.table in the
code they want to simulate with parSim.
}

\value{
A \code{\link[data.table:data.table]{data.table}} with the results of each
iteration in each row, sorted by the \code{id} column (the row number of the
condition in the fully expanded design). Besides the simulation conditions,
the \code{replication} number and the columns returned by \code{expression},
the output contains: \code{id} (integer identifying the design row),
\code{error} (logical; \code{TRUE} if the expression threw an error) and
\code{message} (the error message, \code{NA} on success). Check
\code{any(output$error)} after a run. When \code{write = TRUE} the results
are also written to a file and returned invisibly.
}} with the results of each
iteration in each row, including the columns \code{replication}, \code{id},
\code{error} (logical), and \code{message} (the error message for failed rows,
\code{NA} on success). When \code{write = TRUE} the same data.table is returned
invisibly (in addition to being written to file).
}

\author{
Xinkai Du <xinkai.du.xd@gmail.com>
}

\seealso{
\code{\link{parSim}}
}
