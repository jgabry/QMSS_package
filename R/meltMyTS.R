#' Use \pkg{reshape2}'s \code{melt} on multivariate time series object
#' 
#' \code{meltMyTS} is primarily designed to prepare data to be used with  
#' \code{\link[QMSS]{ggMyTS}} (\pkg{QMSS}) to make plots of time series trends
#' with \code{\link[ggplot2]{ggplot}}.
#'
#' @param mv.ts.object a multivariate time series object created with \code{\link[stats]{ts}}.
#' @param time.var A character string naming the time variable.
#' to either side of the cutpoint for which the estimate is calculated.
#' @param keep.vars An optional character vector with names of variables to keep. 
#' Variables not named in \code{keep.vars} will be dropped. If \code{keep.vars} is
#' not specified then all variables in \code{mv.ts.object} will kept.
#' @return A molten data frame.
#' @author Jonah Gabry <jsg2201@@columbia.edu>. See \code{\link[reshape2]{melt}} 
#' in (\pkg{reshape2}) for the author of the original \code{melt} function. 
#' @seealso \code{\link[QMSS]{ggMyTS}}, \code{\link[reshape2]{melt.data.frame}} 
#' @export
#' @examples
#' See examples in documentation for \code{\link[QMSS]{ggMyTS}}.

meltMyTS <- function(mv.ts.object, time.var, keep.vars){
  # mv.ts.object = a multivariate ts object
  # keep.vars = character vector with names of variables to keep 
  # time.var = character string naming the time variable
  require(reshape2)
  if(missing(keep.vars)) {
    melt.dat <- data.frame(mv.ts.object)
  }
  else {
    melt.dat <- data.frame(mv.ts.object)[, keep.vars]
  }
  melt.dat <- melt(melt.dat, id.vars = time.var)
  colnames(melt.dat)[which(colnames(plot.dat) == time.var)] <- "time"
  return(melt.dat)
}
