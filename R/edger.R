##' Wrapper script that fits an edgeR model and does linear contrast tests. 
##' 
##' @title edgeR wrapper
##' @param data a DGEList object
##' @param MM a model matrix
##' @param CM a contrast matrix if NULL no contrasts will be fit
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

##' Make a link to the projects UCSC Browser page
##'
##' @title UCSC Browser link
##' @param locus  transcript location in the form "loc:start-end"
##' @param project  url string giving the project page at ucsc
##' @return a character string with the url to the tag in the ucsc
##' @author float
##' @export
makeUCSClink <- function(locus,project="&hgsid=201952662_blwe2mLIAlJ066Rb2rEAvZDqZiGz",link='UCSC'){
    locus <- sub(":","%3A",locus)
    paste0("<a href=\"http://genome-euro.ucsc.edu/cgi-bin/hgTracks?db=mm10&position=",locus,project,"\" target=\"_blank\">",link,"</a>")
}

##' Make a heatmap from myTopTags
##'
##' @title TopTag heatmap
##' @param tt data.table results from myTopTags
##' @param groups sample groups to be outlined in the plot
##' @param dataCols columns with data to be shown
##' @param anno columns with tag identifiers
##' @return grid plot of heatmap
##' @author float
##' @export
plotTopTags <- function(tt,groups,dataCols,anno="gene_short_name"){
    heatslide(tt[,dataCols,with=F],tt[['logFC']],groups,genenames=tt[[anno]])
}

##' Apply non-specific filtering to DGEList
##'
##' @title Filter DGE
##' 
##' @param dge DGEList with transcript reads
##' @param mCt minimum count 
##' @param atleast number of samples for which count has to exceed \code{mCT} 
##' @param topM number transcripts with largest MAD to be selected for further processing 
##' @return DGEList with filtered transcripts
##' @author Florian Klinglmueller
filterDGE <- function(dge,mCt=1.5,atleast=2,topM=8000){
    thresh <- rowSums(cpm(deg)>mCt) >= atleast
    dge <- dge[thresh,]
    mad <- rank(-apply(cpm(dge),1,mad)) <= topM
    dge <- dge[mad,]
    dge$samples$lib.size <- colSums(dge$counts)
    dge <- calcNormFactors(filtered)
    dge
}
    
