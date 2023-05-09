\name{parSim_dt}
\alias{parSim_dt}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Parallel Simulator data.table version
}

\description{
The function uses the more efficient data.table in its intenal code instead of dplyr, thereby speeding up the function itself and also allowing researchers to use data.table  to speed up the code they want to simulate with parSim
}

\usage{
parSim_dt(...)
}

\arguments{
  \item{\dots}{
All the arguments that you can use in \code{\link[parSim]{parSim}}
}}
%- maybe also 'usage' for other objects documented here.

\details{
The function uses data.table in its intenal code instead of dplyr, thereby speeding up the simulation and allowing researchers to use data.table in the code they want to simulate with parSim
}

\value{
A data table with the results of each iteration in each row
}

\author{
Xinkai Du <xinkai.du.xd@gmail.com> 
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[parSim]{parSim}}
}

