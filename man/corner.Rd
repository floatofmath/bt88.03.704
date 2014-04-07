\name{Utilities}
\alias{corner}
\title{float's utility functions}
\description{
  Some useful littel functions that make life easier.
}
\usage{
   corner(dat,n=6,m=4)
}
\arguments{
  \item{dat}{
    An object with at least two dimensions (e.g. \code{matrix},
    \code{data.frame})
  }
  \item{n}{
    Number of rows that \code{corner} should print
  }
  \item{m}{
    Number of columns that \code{corner} should print
  }
}
\author{
  Florian Klinglmueller \email{float@lefant.net}
}
\examples{
data <- matrix(rnorm(600),ncol=6)
corner(data)
}