# like head but also limited columns
corner <- function(dat,n=6,m=4){
  n <- min(nrow(dat),n)
  m <- min(ncol(dat),m)
  dat[1:n,1:m]
}
