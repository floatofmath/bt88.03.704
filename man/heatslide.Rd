\name{heatslide}
\alias{heatslide}
\title{Heatmap with dotplot like visualization of effect sizes}
\description{
  Draws a heatmap of a matrix (e.g. gene expression data) together with
  dotplots of a statistic (e.g. log fold change between experimental
  settings) for each line
}
\usage{heatslide(mat,stat,pheno,hcols,lcols,slidetitle='Log (Base 2)
Foldchange')}
\arguments{
  \item{mat}{
    Matrix containing the values to be plotted in the heatmap
  }
  \item{stat}{
    Vector of values that should be plotted by the dotplot beside each
    line of the heatmap
  }
  \item{pheno}{
    Vector giving the group labels for each column of the matrix
  }
  \item{hcols}{
    Colours to be used in the heatmap
  }
  \item{lcols}{
    Colours to indicate the column labels
  }
  \item{slidetitle}{
    Text to be shown below the panel containing the dotplots
  }
}
\details{
  We call the dotplots \sQuote{sliders} because the faintly resemble
  sliders.
}
\author{
  Florian Klinglmueller \email{float@lefant.net}
}
\examples{
    mat <- matrix(rnorm(120),ncol=6)
    stat <- rnorm(20)
    labels <- factor(rep(0,1,each=3))
    hcols <- gray.colors(32)
    lcols <- c('blue','red')

    heatslide(mat,stat,labels,hcols,lcols)
}
    
