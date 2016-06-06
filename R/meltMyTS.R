#' Use \pkg{reshape2}'s \code{melt} on multivariate time series object
#' 
#' \code{meltMyTS} is primarily designed to prepare data to be used with 
#' \code{\link{ggMyTS}} to make plots of time series trends with
#' \code{\link[ggplot2]{ggplot}}.
#'
#' @export
#' @param mv.ts.object A multivariate time series object created with
#'   \code{\link[stats]{ts}}.
#' @param time.var A character string naming the time variable.
#' @param keep.vars An optional character vector with names of variables to
#'   keep. If \code{keep.vars} is not specified then all variables in
#'   \code{mv.ts.object} will be kept. However, if any variables are named then
#'   all other variables will be dropped, except \code{time.var}, which is
#'   always kept.
#'   
#' @return A molten data frame.
#' 
#' @seealso \code{\link{ggMyTS}}, \code{\link[reshape2]{melt.data.frame}} 
#' 
#' @examples
#' # See examples in documentation for ggMyTS.
#' 
meltMyTS <- function(mv.ts.object, time.var, keep.vars){
  if (!requireNamespace("reshape2", quietly = TRUE))
    stop("Please install the 'reshape2' package to use this function.")
  
  if(missing(keep.vars)) {
    melt.dat <- data.frame(mv.ts.object)
  } else {
    if (!(time.var %in% keep.vars)){
      keep.vars <- c(keep.vars, time.var)
    }
    melt.dat <- data.frame(mv.ts.object)[, keep.vars]
  }
  melt.dat <- reshape2::melt(melt.dat, id.vars = time.var)
  colnames(melt.dat)[which(colnames(melt.dat) == time.var)] <- "time"
  melt.dat
}
