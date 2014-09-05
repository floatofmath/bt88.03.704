##' Show upper left corner of a matrix
##'
##' This function prints the upper left corner of a matrix. 
##'
##' 
##' @param dat A matrix or dataframe
##' @param n Number of rows to be shown at most
##' @param m Number of columns to be shown at most
##' @return A matrix or data.frame with \code{n} rows and \code{m} columns
##' @author float
##' 
##' @export
corner <- function(dat,n=6,m=4){
  n <- min(nrow(dat),n)
  m <- min(ncol(dat),m)
  dat[1:n,1:m]
}
