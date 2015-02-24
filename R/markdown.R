##' Custom format for statistical statements
##' 
##' @title HTML Statistical Statement
##' @param toc Whether to inclued a table of contents
##' @author float
##' @export html_statement
html_statement <- function(toc = FALSE){
    css <- system.file("statement/statement.css",package="bt88.03.704")
    template <- system.file("statement/statement.html",package="bt88.03.704")
    
    toc <- TRUE
    rmarkdown::html_document(toc=toc,
                             fig_width=6.5,
                             fig_height=4,
                             theme=NULL,
                             css = css,
                             template = template)
}
