heatslide <- function(mat,stat,pheno,hcols,lcols,slidetitle='Log (Base 2) Foldchange'){
  require(grid)
  heatpanel <- function(matrix,colors){

    N <- nrow(matrix)
    M <- ncol(matrix)
    pushViewport(dataViewport(1:M,0:N,extension=c(.05,0)))
    data <- as.numeric(matrix)
    levs <- length(colors)
    data.levels <- cut(data,levs)
    x <- rep(1:M,each=N)
    y <- rep(N:1,M)
    grid.rect(unit(x,'native'),unit(y,'native'),height=unit(1,'native'),width=unit(1,'native'),gp=gpar(fill=colors[data.levels]),just='top')
    xax <- grid.xaxis(at=1:M,label=colnames(matrix),gp=gpar(cex=.7),name='xax',draw=F)
    xax <- editGrob(xax,gPath('labels'),just='right',rot=90)
    grid.draw(xax)
#    grid.text(x=0.5,y=-.25,label='Array',gp=gpar(cex=.8))
    grid.text(x=-.2,y=.6,label='Genes',rot=90,gp=gpar(cex=.8))
  }

  slidepanel <- function(matrix,colors,slidetitle){
    M <- ncol(matrix)
    N <- nrow(matrix)
    pushViewport(dataViewport(as.numeric(matrix),1:N,extension=1/(2*(N-1)),name='plotRegion'))
    if(N<2){
      grid.segments(rep(0,N),unit(N:1,'native'),rep(1,N),unit(N:1,'native'),name='lines',gp=gpar(lty=2))
    }
    grid.segments(rep(0,N),unit(c(1:(N-1))+.5,'native'),rep(1,N),unit(c(1:(N-1))+.5,'native'),name='seperators')
    for(level in 1:M){
      if(N<=2){
        grid.segments(rep(0,N),unit((((N:1)-.5)+level/(M+1)),'native'),rep(1,N),unit((((N:1)-.5)+level/(M+1)),'native'),name='lines',gp=gpar(lty=2))
      
        grid.rect(matrix[,level],unit((((N:1)-.5)+level/(M+1)),'native'),width=unit(1,'mm'),height=1/M,default.units='native',name=paste(level,'S',sep=''),gp=gpar(fill=colors[level]))
      }
      else{
        grid.rect(matrix[,level],unit(N:1,'native'),width=unit(1,'mm'),height=.8,default.units='native',name=paste(level,'S',sep=''),gp=gpar(fill=colors[level]))
      }
    }
    if(sign(min(stat))<sign(max(stat))) grid.lines(x=unit(c(0,0),'native'),y=unit(c(0,1),'npc'))
    grid.rect()
    grid.xaxis(gp=gpar(cex=.5))

    grid.text(x=rep(1.1,N),y=unit(N:1,'native'),label=rownames(matrix),just='left',gp=gpar(cex=.5))
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
  vpAll <- plotViewport(margins=c(7,5,5,4))

  pushViewport(vpAll)
  pushViewport(vp1)
  legendpanel(levels(pheno),lcols)
  upViewport(2)
  pushViewport(vp2)
  ## something doesn't work yet

  heatpanel(mat,hcols)
  keypanel(pheno,lcols)
  upViewport(2)
  pushViewport(vp3)
  slidepanel(stat,lcols,slidetitle)
  upViewport()
}

        
        