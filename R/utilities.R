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
##' @examples
##' corner(matrix(rnorm(1000),100,100))
##' 
##' @export
corner <- function(dat,n=6,m=4){
    UseMethod("corner",dat)
}

##' @export
corner.default <- function(dat,n=6,m=4){
  n <- min(nrow(dat),n)
  m <- min(ncol(dat),m)
  dat[1:n,1:m]
}

##' @method corner data.table
##' @export
corner.data.table <- function(dat,n=6,m=4){
  n <- min(nrow(dat),n)
  m <- min(ncol(dat),m)
  dat[1:n,1:m,with=F]
}

##' Rearrange columns of a \code{data.table} object, moving a list of columns to the front, while leaving the remaining columns in their original order.
##'
##' @title Move columns to front
##' @param DT \code{data.table} object
##' @param cols names of columns to move to the front
##' @return \code{data.table} with rearranged columns
##'
##' @author Florian Klinglmueller
##' @export
move_to_front <- function(DT,cols){
    all_cols <- names(DT)
    new_cols <- c(cols,all_cols[!(all_cols %in% cols)])
    data.table::setcolorder(DT,new_cols)
    DT
}


##' recycle vector to match the length of another
##'
##' @title recycle
##' @param a vector to be recycled
##' @param b vector to be matched
##' @return vector of length at least \code{b}
##' @export
##' @author Florian Klinglmueller
recycle <- function(a,b){
    if(length(a) < length(b)){
        rep(a,length.out(b))
    } else {
        a
    }
}

