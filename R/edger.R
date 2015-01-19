##' Wrapper script that fits an edgeR model and does linear contrast tests. 
##' 
##' @title edgeR wrapper
##' @param data a DGEList object
##' @param MM a model matrix
##' @param CM a contrast matrix
##' @param ... additional parameters passed to glmLRT
##' @return a DGELRT object
##' @author Florian Klinglmueller
##' @template edger-ex
##' @export fit.edger
fit.edger <- function(data,MM,CM,...){
    require(edgeR)
    fit <- glmFit(data,MM)
    lrt <- glmLRT(fit,contrast=CM,...)
    return(lrt)
}

##' Makes a nice table from edger test results
##'
##' @title Top Tags from edgeR analysis
##' @param lrts DGELRT object
##' @param N number of tags to display (if not set will show all values with FDR adjusted p-value below 0.05)
##' @param annodata a data.table object with additionall annotation data
##' @param key index variable shared with annotation data
##' @return a data.table with the top expressed tags
##' @author Florian Klinglmueller
##' @template edger-ex
##' @export myTopTags
##' @import data.table
myTopTags <- function(lrts,N=NULL,annodata=NULL,key='nearest_ref_id'){
    require(data.table)
    table <- as.data.table(lrts$table,keep=T)
    setnames(table,'rn',key)
    setkey(table,PValue)
    table[,QValue:=p.adjust(PValue,method='BH')]
    if(is.null(N)){
        N <- nrow(table[QValue <.05])
    }
    if(!is.null(annodata)){
        setkeyv(table,key)
        setkeyv(annodata,key)
        table <- table[annodata,,nomatch=0]
        setkeyv(table,'PValue')
    }
    table[1:N]
}
