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


##' Modified print function for tbl_df that permits rounding of numeric entries
##'
##' Taken and modified from Richard Scrivens answere to http://stackoverflow.com/questions/34246552/
##'
##' @title Print tbl_df objects 
##' @param x Object to show
##' @param round Number of digits to print after the comma
##' @param n Number of rows to show. If ‘NULL’, the default, will print all rows if less than option ‘dplyr.print_max’. Otherwise, will print ‘dplyr.print_min’
##' @param width Width of text output to generate. This defaults to NULL, which means use ‘getOption("width")’ and only display the columns that fit on one screen. You can also set ‘options(dplyr.width = Inf)’ to override this default and always print all columns.
##' @author float 
print.tbl_df <- function(x,...,round = 3,n=NULL,width=NULL){
    nums <- vapply(x, is.numeric, NA)
    x[nums] <- lapply(x[nums], round, digits = round)
    cat("Source: local data frame ", dim_desc(x), "\n", sep = "")
    cat("\n")
    print(trunc_mat(x, n = n, width = width))
    invisible(x)
}
