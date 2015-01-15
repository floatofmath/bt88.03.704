##' Wrapper script that fits an edgeR model and does linear contrast tests. 
##' 
##' @title edgeR wrapper
##' @param data a DGEList object
##' @param MM a model matrix
##' @param CM a contrast matrix
##' @param ... additional parameters passed to glmLRT
##' @return a DGELRT object
##' @author Florian Klinglmueller 
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
##' topTags(out)
##' 
##' @export fit.edger
fit.edger <- function(data,MM,CM,...){
    require(edgeR)
    fit <- glmFit(data,MM)
    lrt <- glmLRT(fit,contrast=CM,...)
    return(lrt)
}
