##' Custom format for statistical statements
##' 
##' @title HTML Statistical Statement
##' @param toc Whether to inclued a table of contents
##' @param final Whether still in draft (will print more warnings etc.)
##' @author float
##' @export html_statement
##' @importFrom rmarkdown  html_document
html_statement <- function(toc = FALSE, final = FALSE,...){
    css <- system.file("statement/statement.css",package="bt88.03.704")
    template <- system.file("statement/statement.html",package="bt88.03.704")
    rmarkdown::html_document(toc=toc,
                             fig_width=6.5,
                             fig_height=4,
                             fig_caption=TRUE,
                             theme=NULL,
                             css = css,
                             template = template,...)
}
