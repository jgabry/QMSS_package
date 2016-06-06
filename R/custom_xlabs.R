#' Shortcut to customize a ggplot's x-axis labels
#' 
#' A wrapper for \code{theme(axis.text.x = element_text(angle = 90, vjust = 0.5,
#' ...))}.
#'
#' @export
#' @param angle Angle of rotation (in degrees). Defaults to 90. 
#' @param vjust Vertical justification (in [0,1]). Defaults to 0.5. 
#' @param ... Other arguments to pass to \code{\link[ggplot2]{element_text}}.
#' 
#' @seealso \code{\link[ggplot2]{theme}}
#' @examples
#' \dontrun{
#' keep.vars <- c("year", "n.confinan", "fulltime")        
#' plot.dat <- meltMyTS(by.year.ts, "year", keep.vars)
#' gg <- ggMyTS(plot.dat, "n.confinan")
#' 
#' # Rotate 90 degrees with vertical justification of 0.5 (the defaults)
#' gg + custom_xlabs()
#' 
#' # Can also specify values
#' gg + custom_xlabs(angle = 65)
#' 
#' # and additional arguments
#' my_xlabs <- custom_xlabs(angle = 65, color = "blue", face = "bold", family = "serif")
#' gg + my_xlabs
#' }
#' 
custom_xlabs <- function(angle = 90, vjust = 0.5, ...) {
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = angle, vjust = vjust, ...))
}
