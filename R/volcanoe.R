##' Makes a volcanoe plot
##'
##' @title Volcanoe plot
##' @param mat annotated data matrix, needs to have columns P.Value, logFC, and GeneSymbol
##' @param highlight How many dots in red
##' @param anno How many dots with annotations
##' @return plots a volcanoe plot
##' @export
##' @author Florian Klinglmueller
volcanoe_plot <- function(mat,highlight=50,anno=10){
    N <- nrow(mat)
    mat <- within(mat,{ Top  <- c(rep(paste0("Top",highlight),highlight),rep("rest",N-highlight))
                        anno <- c(rep(TRUE,anno),rep(FALSE,N-anno)) })
    qplot(logFC,-log10(P.Value),colour=Top,data=mat) +
        scale_colour_manual(values=c('darkgray','red')) +
            geom_text(aes(label=GeneSymbol),subset = plyr::.(anno),vjust=.5,hjust=-.1,size=3.5)
}
    

