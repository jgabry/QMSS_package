#' Shortcut to customize a ggplot's x-axis labels
#' 
#' A wrapper for \code{theme(axis.text.x = element_text(angle = 90, vjust = 0.5, ...))}.
#'
#' @param angle Angle of rotation (in degrees). Defaults to 90. 
#' @param vjust Vertical justification (in [0,1]). Defaults to 0.5. 
#' @param ... Other arguments to pass to \code{\link[ggplot2]{element_text}}.
#' @author Jonah Gabry <jsg2201@@columbia.edu>. See \code{\link[ggplot2]{ggplot}} 
#' for the author of the \code{ggplot} function. 
#' @export
#' @examples
#' \dontrun{
#' keep.vars <- c("year", "n.confinan", "fulltime")        
#' plot.dat <- meltMyTS(by.year.ts, "year", keep.vars)
#'  # Rotate 90 degrees with vertical justification of 0.5 (the defaults)
#' ggMyTS(plot.dat, "n.confinan") + custom_xlabs()
#'  # Can also specify values
#' ggMyTS(plot.dat, "n.confinan") + custom_xlabs(angle = 65)
#'  # and additional arguments
#' my_xlabs <- custom_xlabs(angle = 65, color = "blue", face = "bold", family = "serif")
#' ggMyTS(plot.dat, "n.confinan") + my_xlabs
#'                     
#' }
#' 

custom_xlabs <- function(angle = 90, vjust = 0.5, ...) {
  require(ggplot2)
  theme(axis.text.x = element_text(angle = angle, vjust = vjust, ...))
}
