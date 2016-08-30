## Limma wrapper



#' Wrapper script for lmFit, contrast.fit and ebayes
#' 
#' \code{fit.limma} fits a \pkg{limma} model, computes contrasts and applies an
#' emperical bayes procedure to shrink variance estimates
#' 
#' 
#' @param data An expression set
#' @param MM A model matrix
#' @param CM A contrast matrix
#' @author Florian Klinglmueller \email{float@@lefant.net}
#' @examples
#' 
#' data <- matrix(rnorm(600),ncol=6)
#' design <- data.frame('Treatment' = factor(rep(0:1,each=3)))
#' mm <- model.matrix(~0+Treatment,design)
#' colnames(mm) <- c('control','treatment')
#' cm <- makeContrasts('control-treatment',levels=mm)
#' out <- fit.limma(data,mm,cm)
#' 
#' topTable(out)
#'
#' @import limma
#' @export fit.limma
fit.limma <- function(data,MM,CM){
  require(limma)
  m <- lmFit(data,MM)
  cm <- contrasts.fit(m,CM)
  ecm <- eBayes(cm)
  return(ecm)
}


##' Make a heatmap from the results of a limma fit
##'
##' @title Plot Limma
##' @param model \code{MArrayLM} object fit by \code{limma}
##' @param coef \code{integer} index of contrast to sort genes by
##' @param data \code{ExpressionSet} with normalized expression values
##' @param pheno \code{factor} with phenotype labels
##' @param anno \code{ProbeAnnDbBimap} of probe annotations
##' @param top \code{integer} number of genes to plot
##' @param subset \code{integer} vector of column numbers to be plotted
##' @return plots a heatslide plot
##' @export plot_limma
##' @author float
plot_limma <- function(model,coef,data,pheno,anno,top=50,subset=1:length(pheno)){
    tt <- topTable(model,coef=coef,number=nrow(model))
    if(length(top)==1){
        top <- 1:top
        select <- rownames(tt)[top]
    } else {
        select <- top
    }
    heatslide(exprs(data[select,subset]),tt[select,'logFC'],pheno[subset],getAnno(select,anno))
}

##' Make a nice table for multiple limma estimated contrasts
##'
##' @title print limma table
##' @param model Limma fitted model
##' @param coef vector of coefficients for which statst should be included
##' @param data \code{ExpressionSet} with data
##' @param genenames \code{ProbeAnnDbBimap} with long gene descriptions
##' @param symbols \code{ProbeAnnDbBimap} with short gene descriptions
##' @param lookup named vector lookup table that translates contrast defs into language
##' @return \code{data.frame} with annotated statistics
##' @export print_limma
##' @author float
print_limma <- function(model,coef,data,genenames,symbols,lookup=NULL,top=Inf){
    if(length(coef)>1){
        if(top < Inf) warning('top < Inf works only if one coefficient is printed')
        tts <- lapply(coef,function(co) topTable(model,coef=co,number=nrow(model),sort.by='none')[,1:5])
    } else {
        tts  <- list(topTable(model,coef=coef,number=top)[,1:5])
    }
    tt <- do.call('cbind',tts)
    contrast_names <- colnames(model$contrasts)[coef]
    if(!is.null(lookup)){
        contrast_names  <- lookup[contrast_names]
    }
    colnames(tt) <- paste(rep(contrast_names,each=5),colnames(tt),sep='_')
    ids <- rownames(tt)
    mat <- cbind(data.frame(ProbeID = ids,
                            GeneSymbol = unlist(getAnno(ids,symbols)),
                            GeneName = unlist(getAnno(ids,genenames)),
                            tt))
    mat
}


##' count the number of genes signifcant at a certain level
##'
##' @title count significant genes
##' @param model \code{MArrayLM} object fit by \code{limma}
##' @param coef \code{integer} index of contrast to sort genes by
##' @param cutoff significance level
##' @param column which column should the cutoff be applied to 
##' @return number of significant probesets
##' @export count_significant
##' @author float
count_significant <- function(model,coef,cutoff=.05,column='adj.P.Val'){
    sum(sign(cutoff)*topTable(model,coef,Inf)[,column]<=cutoff)
}
