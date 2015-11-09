##' ## which binary numbers (of length 3) have a 1 at the 3rd position
##' out <- contains(3,3)
##' out
##' ## check if true
##' bins <- lapply(out,to.binom,3)
##' bins
##' all(sapply(bins,`[`,3) == 1)
##' sapply(bins,from.binom)
