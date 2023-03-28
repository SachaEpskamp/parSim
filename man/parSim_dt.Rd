\name{parSim_dt}
\alias{parSim_dt}
\alias{parSim-package}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Parallel Simulator data.table version
}
\description{
parSim function does not support the use of data.table functions in the R expression it takes in. parSim_dt allows you use the more efficient data.table functions to speed up the simulation and returns a data table of simulation results. See details
}
\usage{
parSim_dt(..., expression)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{
All the arguments that you can use in \code{\link[parSim]{parSim}}
}
  \item{expression}{
  
  }
}
\details{

}
\value{
A data table with every simulation as a row
}

\author{
Xinkai Du <xinkai.du.xd@gmail.com> 
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[parSim]{parSim}}
}
