% Documentation for the `parabar::configure_bar` re-export.

\name{configure_bar}
\alias{configure_bar}
\title{Configure The Progress Bar}

\usage{
    configure_bar(type = "modern", ...)
}

\arguments{
    \item{type}{
        A character string specifying the type of progress bar to be used with
        compatible \code{\link[parabar:Backend]{backends}}. Possible values are
        \code{"modern"} and \code{"basic"}. The default value is
        \code{"modern"}.
    }

    \item{...}{
        A list of named arguments used to configure the progress bar. See the
        \strong{Details} section for more information.
    }
}

\description{
    This function can be used to conveniently configure the progress bar. It is
    exported for convenience from the
    \code{\link[parabar:parabar-package]{parabar::parabar}} package. Please
    consult its documentation at
    \code{\link[parabar:configure_bar]{parabar::configure_bar}} for more
    information on how to use it.
}
