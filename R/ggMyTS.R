#' Easily plot time series trends with \code{\link[ggplot2]{ggplot}}
#' 
#' \code{ggMyTS} is primarily designed to be used with a data frame created with
#' \code{\link[QMSS]{meltMyTS}} (\pkg{QMSS}).
#'
#' @param df The data frame in which to look for variables to be plotted. Typically 
#' created with \code{\link[QMSS]{meltMyTS}} (\pkg{QMSS})
#' @param varlist A character string or vector naming the variable(s) in \code{df} to plot.
#' @param ... Other options that will be passed to both \code{\link[ggplot2]{geom_line}} 
#' and \code{\link[ggplot2]{geom_point}}. See examples. 
#' @return A \code{ggplot} object. 
#' @author Jonah Gabry <jsg2201@@columbia.edu>. See \code{\link[ggplot2]{ggplot}} 
#' for the author of the \code{ggplot} function. 
#' @seealso \code{\link[QMSS]{meltMyTS}}
#' @export
#' @examples
#' \dontrun{
#' keep.vars <- c("year", "n.confinan", "fulltime")        
#' plot.dat <- meltMyTS(mv.ts.object = by.year.ts,
#'                      time.var = "year", keep.vars = keep.vars)
#' ggMyTS(plot.dat, varlist = c("n.confinan", "fulltime"))
#' ggMyTS(plot.dat, "n.confinan", color = "forestgreen")                      
#' }
#' 
#' 
ggMyTS <- function(df, varlist, ...){
  require(ggplot2)
  # varlist = character vector with names of variables to use
  include <- with(df, variable %in% varlist)
  gg <- ggplot(df[include,], aes(time, value, colour = variable)) 
  gg <- gg + geom_line(size = 1.25, ...) + geom_point(size = 3, ...)
  gg + theme(legend.position = "bottom")
} 
