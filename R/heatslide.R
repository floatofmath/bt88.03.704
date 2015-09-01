#' Heatmap with dotplot like visualization of effect sizes
#' 
#' Draws a heatmap of a matrix (e.g. gene expression data) together with
#' dotplots of a statistic (e.g. log fold change between experimental settings)
#' for each line
#' 
#' We call the dotplots \sQuote{sliders} because the faintly resemble sliders.
#' 
#' @param mat Matrix containing the values to be plotted in the heatmap
#' @param stat Vector of values that should be plotted by the dotplot beside
#' each line of the heatmap
#' @param pheno Vector giving the group labels for each column of the matrix
#' @param genenames Character vector giving genenames (or corresponding annotation)
#' @param hcols Colours to be used in the heatmap
#' @param lcols Colours to indicate the column labels
#' @param scaleValues Scale expression values by either 'row', 'column', or 'none'
#' @param scaler if "linear" use scale and remove mean and divide by sd, if "rank" transform to ranks
#' @param slidetitle Text to be shown below the panel containing the dotplots
#' @param rowSort if \code{TRUE} genes will be sorted according to the test statistic
#' @param colSort if \code{FALSE} chips will be sorted according to the phenotype
#' @author Florian Klinglmueller \email{float@@lefant.net}
#' @examples
#' 
#'     mat <- matrix(rnorm(120),ncol=6)
#'     stat <- rnorm(20)
#'     labels <- factor(rep(0,1,each=3))
#'     hcols <- gray.colors(32)
#'     lcols <- c('blue','red')
#' 
#'     heatslide(mat,stat,labels,hcols,lcols)
#' 
#' @export heatslide
#' @import grid
heatslide <- function(mat,stat,pheno,
                      genenames=NULL,
                      hcols=colorRampPalette(c('blue','white','red'))(32),
                      lcols=rainbow(length(levels(pheno))),
                      scaleValues=c('row','column','none'),
                      scaler='linear',
                      slidetitle='Log (Base 2) Foldchange',
                      rowSort=TRUE,
                      colSort=TRUE){
  require(grid)
  heatpanel <- function(matrix,colors){
    N <- nrow(matrix)
    M <- ncol(matrix)
    
    pushViewport(dataViewport(1:M,0:N,extension=c(.05,0)))
    if(is.data.frame(matrix)){
        matrix <- as.matrix(matrix)
    }
    levs <- length(colors)
    scaleF <- switch(scaler,"linear"=function(x) cut(x,levs,labels=FALSE),"rank"=rank)
    data <- switch(scaleValues[1],
                   "row"=t(apply(matrix,1,scaleF)),
                   "column"=apply(matrix,2,scaleF),
                   "both"=matrix(scaleF(matrix),nrow(matrix),ncol(matrix)))

    data.levels <- as.numeric(data)
    x <- rep(1:M,each=N)
    y <- rep(N:1,M)
    grid.rect(unit(x,'native'),unit(y,'native'),height=unit(1,'native'),width=unit(1,'native'),gp=gpar(fill=colors[data.levels],lwd=draw.grid),just='top')
    xax <- grid.xaxis(at=1:M,label=colnames(matrix),gp=gpar(cex=.7),name='xax',draw=F)
    xax <- editGrob(xax,gPath('labels'),just='right',rot=90)
    grid.draw(xax)
    grid.text(x=0.5,y=-.25,label='Array',gp=gpar(cex=.8))
    grid.text(x=-.2,y=.6,label='Genes',rot=90,gp=gpar(cex=.8))
  }

  slidepanel <- function(statistics,genenames,slidetitle){
    N <- length(statistics)
    pushViewport(dataViewport(as.numeric(statistics),1:N,extension=1/(2*(N-1)),name='plotRegion'))
    grid.rect()
    grid.xaxis(gp=gpar(cex=.5))
    if(!is.null(genenames)){
        grid.text(x=rep(1.1,N),y=unit(N:1,'native'),label=genenames,just='left',gp=gpar(cex=.8))
    }
    grid.text(x=.1,y=unit(0,'npc')-unit(3,'lines'),label=slidetitle,gp=gpar(cex=.6),just='left')
    if(N<2){
        grid.segments(rep(0,N),unit(N:1,'native'),rep(1,N),unit(N:1,'native'),name='lines',gp=gpar(lty=2))
    }
    grid.segments(rep(0,N),unit(c(1:(N-1))+.5,'native'),rep(1,N),unit(c(1:(N-1))+.5,'native'),name='seperators',gp=gpar(lwd=draw.grid))
    if(N<=2){
        grid.segments(rep(0,N),unit((((N:1)-.5)+.5),'native'),rep(1,N),unit((((N:1)-.5)+.5/(1+1)),'native'),name='lines',gp=gpar(lty=2,lwd=draw.grid))
        
        grid.rect(statistics,unit((((N:1)-.5)+.5/(1+1)),'native'),width=unit(1,'mm'),height=1/1,default.units='native',name=paste(1,'S',sep=''),gp=gpar(fill='red'))
    }
    else{
        grid.rect(statistics,unit(N:1,'native'),width=unit(1,'mm'),height=.8,default.units='native',name=paste(1,'S',sep=''),gp=gpar(fill='red'))
    }
    if(sign(min(stat))<sign(max(stat))) grid.lines(x=unit(c(0,0),'native'),y=unit(c(0,1),'npc'))
  }

    multipanel <- function(statistics,genenames,colors,slidetitle){
      if(is.matrix(statistics)){
          M <- ncol(statistics)
          N <- nrow(statistics)
      } else {
          M <- 1
          N <- length(statistics)
          statistics <- as.matrix(statistics,ncol=M,drop=F)
      }
    pushViewport(dataViewport(as.numeric(statistics),1:N,extension=1/(2*(N-1)),name='plotRegion'))
    if(N<2){
      grid.segments(rep(0,N),unit(N:1,'native'),rep(1,N),unit(N:1,'native'),name='lines',gp=gpar(lty=2))
    }
    ## separate sliders
    grid.segments(rep(0,N),unit(c(1:(N-1))+.5,'native'),rep(1,N),unit(c(1:(N-1))+.5,'native'),name='seperators')
    for(level in 1:M){
      if(N<=2){
        grid.segments(rep(0,N),unit((((N:1)-.5)+level/(M+1)),'native'),rep(1,N),unit((((N:1)-.5)+level/(M+1)),'native'),name='lines',gp=gpar(lty=2))
      
        grid.rect(statistics[,level],unit((((N:1)-.5)+level/(M+1)),'native'),width=unit(1,'mm'),height=1/M,default.units='native',name=paste(level,'S',sep=''),gp=gpar(fill=colors[level]))
      }
      else{
        grid.rect(statistics[,level],unit(N:1,'npc'),width=unit(0.5,'npc'),height=.8,default.units='native',name=paste(level,'S',sep=''),gp=gpar(fill=colors[level]))
      }
    }
    if(sign(min(stat))<sign(max(stat))) grid.lines(x=unit(c(0,0),'native'),y=unit(c(0,1),'npc'))
    grid.rect()
    grid.xaxis(gp=gpar(cex=.5))
    if(!is.null(genenames)){
        grid.text(x=rep(1.1,N),y=unit(N:1,'native'),label=genenames,just='left',gp=gpar(cex=.8))
    }
    grid.text(x=.1,y=unit(0,'npc')-unit(3,'lines'),label=slidetitle,gp=gpar(cex=.6),just='left')
  }

  
  legendpanel <- function(levels,colors){
    N <- length(levels)
    pushViewport(plotViewport(c(1,1,0,1)))
    grid.rect(rep(unit(0,'npc')-unit(1,'lines')),unit((1:N)*1.5,'lines'),height=unit(1,'lines'),width=unit(1,'lines'),gp=gpar(fill=colors,cex=.6))
    grid.text(unit(rep(0,N),'lines'),unit((1:N)*1.5,'lines'),label=levels,just='left',gp=gpar(cex=.6))
  }

  keypanel <- function(pheno,colors){
    N <- length(pheno)
    grid.rect(unit(1:N,'native'),unit(rep(1,N),'lines')+unit(rep(1,N),'npc'),width=unit(1,'native'),height=unit(1,'lines'),gp=gpar(fill=colors[pheno]))
  }
  vp1 <- viewport(0,width=.2,just='left')
  vp2 <- viewport(.2,width=.55,just='left')
  vp3 <- viewport(.8,width=.15,just='left')
  grid.newpage()
  vpAll <- plotViewport(margins=c(7,5,5,4))

  pushViewport(vpAll)
  pushViewport(vp1)
  legendpanel(levels(pheno),lcols)
  upViewport(2)
  pushViewport(vp2)
  
  if(rowSort){
      oo <- order(stat,decreasing=TRUE)
      mat <- mat[oo,]
      stat <- stat[oo]
      if(!is.null(genenames)){
          genenames <- genenames[oo]
      }
  }
  if(colSort){
      mat <- mat[,order(pheno)]
      pheno <- sort(pheno)
  }
  draw.grid <- ifelse(nrow(mat)>200,0,1)
  heatpanel(mat,hcols)
  keypanel(pheno,lcols)
  upViewport(2)
  pushViewport(vp3)
  if(is.matrix(stat)){
      multipanel(stat,genenames,lcols,slidetitle)
  } else {
      slidepanel(stat,genenames,slidetitle)
  }
  upViewport(2)
}

        
        
