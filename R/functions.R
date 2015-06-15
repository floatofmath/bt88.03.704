#' Plot several ggplot objects in one device
#' 
#' \code{mulitplot} prints several \pkg{ggplot} plot-objects on one device
#' 
#' If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE), then
#' plot 1 will go in the upper left, 2 will go in the upper right, and 3 will
#' go all the way across the bottom.
#' 
#' @param ...  Several \code{ggplot} objects that should be placed on the same
#' plotting device
#' @param plotlist Optional - a list of \code{ggplot} objects to be placed on
#' the same plotting device
#' @param cols Number of columns of the layout in which the objects are to be
#' plotted. Will be ignored if \option{layout} is present.
#' @param layout A matrix specifying the layout. If present, 'cols' is ignored.
#' @param titles A vector with titles for the individual plots
#' @note Original code was taken from R Graphics Cookbook courtesy of Winston
#' Change
#' \url{http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/}
#' @author Florian Klinglmueller \email{float@@lefant.net}, Winston Chang
#' @references Chang, W. (2012). R graphics cookbook. \emph{O'Reilly Media,
#' Inc.}
#' @examples
#' 
#' x <- rnorm(100)
#' y <- rnorm(100)
#' d <- data.frame('y'=y,'x'=x,'s'=x+y)
#' p1 <- qplot(y,x,data=d,)
#' p2 <- qplot(y,s,data=d)
#' multiplot(p1,p2,cols=2)
#' 
#' @export multiplot
multiplot <- function(..., plotlist=NULL, cols=1, layout=NULL, titles=NULL) {
  require(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }
  # Add titles, you could do this for each plot, but it seems nice to have it here as well
  if(!is.null(titles)){
     if(numPlots != length(titles)){
         stop(paste('Number of titles (',length(titles),') needs to match number of plots (',numPlots,')!',sep=''))
     }
     for(i in 1:numPlots){
         plots[[i]] <- plots[[i]]+ggtitle(titles[i])
     }
 }
         
 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



#' Make a versioned filename
#' 
#' This function returns a filename with a version extension to avoid
#' overwriting older results. The extension may either be the date (with time)
#' or a random hash. The latter may be useful if you want to obscure the date
#' of the data generation.
#' 
#' 
#' @param base character giving the main part of the filename
#' @param ending file ending
#' @param format format for date time version
#' @param hash (defunct) if true a hash is used for versioning the filename
#'
#' @export
vfile <- function(base,ending='Rd',format='%y%m%d',hash=FALSE){
    tstring <- ifelse(hash,,format(Sys.time(),format))
    fname <- paste(base,'_',Sys.info()['nodename'],'_',tstring,'.',ending,sep='')
    fname
}

##' Read random numbers from sytems random devices
##'
##' Based on stackoverflow question http://stackoverflow.com/questions/11505039
##' 
##' @title read random numbers
##' @param n number of random numbers to read
##' @param a start of range
##' @param b end of range
##' @param dev device file location
##' @return numeric vector of leng \code{n} with numbers between \code{a} and \code{b}
##' 
##' @author Spacedman
readRandom <- function(n,a,b,dev="/dev/urandom"){
  size = b-a + 1
  rng = file(dev,"rb") # open connection
  nums = readBin(rng,what="integer",n=n) # read some 8-byte integers 
  close(rng) # close the connection
  return( a + nums %% size ) # reduce range and shift
}

#' Newest (versioned) file
#' 
#' Searches a directory for versioned files with the given base name and
#' returns its newest filename.
#' 
#' 
#' @param base character giving the main part of the filename
#' @param path path to directory to search in
#' @param ending file ending
#' @param format format for date time version
#' @param ostime (defunct) should the time of file generation return by the OS
#' be used (useful for hashed files)?
#'
#' @export
newest_vfile <- function(base,path=getwd(),ending='Rd',format='%y%m%d',ostime=FALSE){
    fnames <- list.files(path,pattern=paste(base,'*',sep=''))
    newest <- which.max(as.Date(sapply(lapply(strsplit(fnames,'_'),tail,n=1),sub,pattern=paste('\\.',ending,sep=''),replacement=''),format))
    fnames[newest]
}    
    
##' modified setwd
##'
##' Does windows to linux and vice versa home directory name
##' translation
##' 
##' If the detected OS is Linux directories starting with '//' will be renamed to '~/' if it is Windows the otherway around, else the directory name stays unchanged 
##'
##' @param dir A character string
##' @seealso \code{\link[base]{setwd}} for the base function
##' @author float
##'
##' @export
setwd <- function(dir){
    sys <- Sys.info()['sysname']
    if(sys == 'Linux'){
        dir <- sub('^//','~/',dir)
    } else if(sys == 'Windows'){
        dir <- sub('^~/','//',dir)
    } 
    base:::setwd(dir)
}

##' List fold (reduce)
##'
##' Reduce for lists
##'
##' @param list 
##' @param fun a binary function taking elements of the list as values
##' @param z initial value, if empty the first element of the list will be used 
##' @return same as output of \code{fun}
##' @author float
##'
##' @export
list_fold <- function(list,fun,z=NULL){
    fun <- match.fun(fun)
    if(is.null(z)){
        if(length(list)==0){
            stop("List of length 0 requires initial value")
        }
        z <- list[[1]]
        list <- tail(list,-1)
    }
    foldr <- function(list,z,fun){
        if(length(list)==0){
            return(z)
        }
        fun(z,foldr(tail(list,-1),list[[1]],fun))
    }
    foldr(list,z,fun)
}

##' Computes a confidence interval for the standard deviation for a
##' standard deviation estimate
##' 
##' @title Standard deviation confidence interval
##' @param sd estimated standard deviation
##' @param n number of observations used for the estimate
##' @param alpha desired coverage probability
##' @return numeric vector with lower and upper bound
##' @author float
##' @export sd_confint
sd_confint <- function(sd,n,alpha){
    c(sd*sqrt((n-1)/qchisq(alpha/2,n-1,lower=FALSE)),sd*sqrt((n-1)/qchisq(alpha/2,n-1)))
}

#' Print system load and user overview
#' 
#' This script uses top to print the system load, memory usage and a list of
#' active users with their proportion of memory and cpu usage.
#' 
#' @export
Rtop <- function(){
    av.load <- as.numeric(gsub(',','',unlist(strsplit(system("top -c -b -n 1| awk '{print $10\";\"$11\";\"$12}'",intern=T)[1],';'))))
names(av.load) <- c('1min','5min','15min')
    mem.used <- as.numeric(unlist(strsplit(system("top -c -b -n 1| awk '{print $3\";\"$5}'",intern=T)[4],';')))
    prop.mem <- mem.used[2]/mem.used[1]
    swap.used <- as.numeric(unlist(strsplit(system("top -c -b -n 1| awk '{print $3\";\"$5}'",intern=T)[5],';')))
    prop.swap <- swap.used[2]/swap.used[1]
    proc <- system("top -c -b -n 1|awk 'BEGIN {skip = 7; lines = 1;}{if (lines <= skip) { lines++ } else { print $2\";\"$9\";\"$10} }'",intern=T)
    proc <- do.call('rbind',lapply(proc,function(e) unlist(strsplit(e,';'))))
    users <- unique(proc[,1])
    cpu <- tapply(as.numeric(proc[,2]),proc[,1],sum)
    mem <- tapply(as.numeric(proc[,3]),proc[,1],sum)
    proc <- cbind(cpu,mem)
    cat('               1min 5min 15min\n')
    cat('Average load: ',av.load,'\n',sep=' ')
    cat('Memomery used:',round(prop.mem,2)*100,'%\n')
    cat('Swap used:    ',round(prop.swap,2)*100,'%\n')
    cat('Active users: \n')
    print(proc)
}

##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##'
##' ##' Wrapper around mclapply to track progress
##' 
##' Based on http://stackoverflow.com/questions/10984556
##' 
##' @param X         a vector (atomic or list) or an expressions vector. Other
##'                  objects (including classed objects) will be coerced by
##'                  ‘as.list’
##' @param FUN       the function to be applied to
##' @param ...       optional arguments to ‘FUN’
##' @param mc.preschedule see mclapply
##' @param mc.set.seed see mclapply
##' @param mc.silent see mclapply
##' @param mc.cores see mclapply
##' @param mc.cleanup see mclapply
##' @param mc.allow.recursive see mclapply
##' @param mc.progress track progress?
##' @param mc.style    style of progress bar (see txtProgressBar)
##'
##' @return A list of the same length as X and named by X. 
##' @author wannymahoots
##' 
##' @examples
##' x <- mclapply2(1:1000, function(i, y) Sys.sleep(0.01))
##' x <- mclapply2(1:3, function(i, y) Sys.sleep(1), mc.cores=1)
##' @author Florian Klinglmueller
mclapply2 <- function(X, FUN, ..., 
    mc.preschedule = TRUE, mc.set.seed = TRUE,
    mc.silent = FALSE, mc.cores = getOption("mc.cores", 2L),
    mc.cleanup = TRUE, mc.allow.recursive = TRUE,
    mc.progress=TRUE, mc.style=3) 
{
    if (!is.vector(X) || is.object(X)) X <- as.list(X)

    if (mc.progress) {
        f <- fifo(tempfile(), open="w+b", blocking=T)
        p <- parallel:::mcfork()
        pb <- txtProgressBar(0, length(X), style=mc.style)
        setTxtProgressBar(pb, 0) 
        progress <- 0
        if (inherits(p, "masterProcess")) {
            while (progress < length(X)) {
                readBin(f, "double")
                progress <- progress + 1
                setTxtProgressBar(pb, progress) 
            }
            cat("\n")
            parallel:::mcexit()
        }
    }
    tryCatch({
        result <- mclapply(X, function(...) {
                res <- FUN(...)
                if (mc.progress) writeBin(1, f)
                res
            }, 
            mc.preschedule = mc.preschedule, mc.set.seed = mc.set.seed,
            mc.silent = mc.silent, mc.cores = mc.cores,
            mc.cleanup = mc.cleanup, mc.allow.recursive = mc.allow.recursive
        )

    }, finally = {
        if (mc.progress) close(f)
    })
    result
}
