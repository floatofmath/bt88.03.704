\name{multiplot}
\alias{multiplot}
\title{Plot several ggplot objects in one device}
\description{
  \code{mulitplot} prints several \pkg{ggplot} plot-objects on one device
}
\usage{
   multiplot(..., plotlist=NULL, cols=1, layout=NULL)
}
\arguments{
  \item{...}{
    Several \code{ggplot} objects that should be placed on the same
    plotting device
  }
  \item{plotlist}{
    Optional - a list of \code{ggplot} objects to be placed on the same
    plotting device
  }
  \item{cols}{
    Number of columns of the layout in which the objects are to be
    plotted. Will be ignored if \option{layout} is present. 
  }
  \item{layout}{
    A matrix specifying the layout. If present, 'cols' is ignored.
  }
}
\details{  
  If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
  then plot 1 will go in the upper left, 2 will go in the upper right, and
  3 will go all the way across the bottom.
}
\references{
  Chang, W. (2012). R graphics cookbook. \emph{O'Reilly Media, Inc.}
}
\note{
  Original code was taken from R Graphics Cookbook courtesy of Winston
  Change
  \url{http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/}
}
\author{
  Florian Klinglmueller \email{float@lefant.net}, Winston Chang
}
\examples{
x <- rnorm(100)
y <- rnorm(100)
d <- data.frame('y'=y,'x'=x,'s'=x+y)
p1 <- qplot(y,x,data=d,)
p2 <- qplot(y,s,data=d)
multiplot(p1,p2,cols=2)
}


    