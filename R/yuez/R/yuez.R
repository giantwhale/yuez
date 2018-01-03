#' A Toolbox for Statistical Analysis in R  
#' 
#' @name yuez
#' @docType package
#' @import Rcpp
#' @useDynLib yuez
NULL

.onUnload <- function(libpath) {
    cat("Unloading Dynlib yuez ...\n")
    library.dynam.unload('yuez', libpath)
}
