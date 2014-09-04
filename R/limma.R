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
#' @export fit.limma
fit.limma <- function(data,MM,CM){
  m <- lmFit(data,MM)
  cm <- contrasts.fit(m,CM)
  ecm <- eBayes(cm)
  return(ecm)
}
