##' @examples
##'
##' ## setting up some random data
##' data <- matrix(rpois(600,200),ncol=6)
##' dge <- DGEList(counts=data)
##' dge <- estimateDisp(dge)
##'
##' ## a design matrix
##' design <- data.frame('Treatment' = factor(rep(0:1,each=3)))
##' mm <- model.matrix(~0+Treatment,design)
##'
##' ## a contrast matrix
##' colnames(mm) <- c('control','treatment')
##' cm <- makeContrasts('control-treatment',levels=mm)
##'
##' ## fit and test
##' out <- fit.edger(dge,mm,cm)
##' 
##' myTopTags(out,3)
##' 
