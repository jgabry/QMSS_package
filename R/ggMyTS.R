#' Easily plot time series trends with \code{\link[ggplot2]{ggplot}}
#' 
#' \code{ggMyTS} is primarily designed to be used with a data frame created with
#' \code{\link[QMSS]{meltMyTS}} (\pkg{QMSS}).
#'
#' @param df The data frame in which to look for variables to be plotted. Typically 
#' created with \code{\link[QMSS]{meltMyTS}} (\pkg{QMSS})
#' @param varlist A character string or vector naming the variable(s) in \code{df} to plot. 
#' If \code{varlist} is not specified, then all variables in \code{df} will be used. 
#' @param line Should lines be plotted? Defaults to \code{TRUE}. 
#' @param point Should points be plotted? Defaults to \code{TRUE}.
#' @param pt.size Size of the points, if \code{point == TRUE}.
#' @param line.size Size of the line(s), if \code{line == TRUE}.
#' @param ... Other options that will be passed to \code{\link[ggplot2]{geom_line}} 
#' and \code{\link[ggplot2]{geom_point}}. See examples. 
#' @note At least one of \code{line} or \code{point} must be \code{TRUE}.
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
#' ggMyTS(plot.dat, "n.confinan", color = "forestgreen", point = F, linetype = 2)                      
#' }
#' 

ggMyTS <- function(df, varlist, line = TRUE, point = TRUE, pt.size = 3, line.size = 1.25, ...){
  require(ggplot2)
  # varlist = character vector with names of variables to use
  if(missing(varlist)){
    gg <- ggplot(df, aes(time, value, colour = variable)) 
  }
  else{
    include <- with(df, variable %in% varlist)
    gg <- ggplot(df[include,], aes(time, value, colour = variable))   
  }
  if(line == FALSE & point == FALSE) {
    stop("At least one of 'line' or 'point' must be TRUE") 
  }
  else{
    if(line == TRUE) gg <- gg + geom_line(size = line.size, aes(color = variable), ...)
    if(point == TRUE) gg <- gg + geom_point(size = pt.size, aes(color = variable), ...)
  }
  
  gg + xlab("") + theme(legend.position = "bottom") + scale_x_continuous(breaks = min(df$time):max(df$time))
} 
