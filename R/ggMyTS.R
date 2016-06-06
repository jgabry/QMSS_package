#' Easily plot time series trends with ggplot
#' 
#' \code{ggMyTS} is primarily designed to be used with a data frame created with
#' \code{\link[QMSS]{meltMyTS}}.
#'
#' @export
#' @param df The data frame in which to look for variables to be plotted. 
#'   Typically created with \code{\link[QMSS]{meltMyTS}} (\pkg{QMSS}).
#' @param varlist A character string or vector naming the variable(s) in
#'   \code{df} to plot. If \code{varlist} is not specified, then all variables
#'   in \code{df} will be used.
#' @param line Should lines be plotted? Defaults to \code{TRUE}.
#' @param point Should points be plotted? Defaults to \code{TRUE}.
#' @param pointsize Size of the points, if \code{point == TRUE}.
#' @param linewidth Width of the line(s), if \code{line == TRUE}.
#' @param ... Other options that will be passed to
#'   \code{\link[ggplot2]{geom_line}} and \code{\link[ggplot2]{geom_point}}. See
#'   examples.
#' 
#' @return A \code{ggplot} object that can be further customized using functions
#'   in the \pkg{ggplot2} package.
#'   
#' @seealso \code{\link[QMSS]{meltMyTS}}
#' 
#' @examples
#' \dontrun{
#' keep.vars <- c("year", "n.confinan", "fulltime")        
#' plot.dat <- meltMyTS(mv.ts.object = by.year.ts,
#'                      time.var = "year", keep.vars = keep.vars)
#' ggMyTS(plot.dat, varlist = c("n.confinan", "fulltime"))
#' ggMyTS(plot.dat, "n.confinan", color = "forestgreen", point = F, linetype = 2)                      
#' }
#' 

ggMyTS <- function(df, varlist, line = TRUE, point = TRUE, pointsize = 3, linewidth = 1.25, ...){
  if (missing(varlist)) {
    include <- with(df, variable %in% varlist)
    df <- df[include, ]
  }
  gg <- ggplot2::ggplot(df, ggplot2::aes_string(x = "time", y = "value", colour = "variable")) 

  if (!line && !point) {
    stop("At least one of 'line' or 'point' must be TRUE") 
  } else {
    if (line) 
      gg <- gg + ggplot2::geom_line(size = linewidth)
    if (point)
      gg <- gg + ggplot2::geom_point(size = pointsize)
  }
  
  gg + 
    ggplot2::xlab("") + 
    ggplot2::theme(legend.position = "bottom") + 
    ggplot2::scale_x_continuous(breaks = min(df$time):max(df$time))
} 
