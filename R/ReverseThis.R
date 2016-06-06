#' Reverse-code a variable
#'
#' @export
#' @param var A numeric or factor variable.
#' 
#' @return A reverse-coded version of \code{var}.
#' 
#' @examples
#' x <- c(1,1,1,2,3,3)
#' ReverseThis(x)
#' 
ReverseThis <- function(var){
  nvar <- as.numeric(var)
  max <- max(nvar, na.rm = TRUE)
  min <- min(nvar, na.rm = TRUE)
  sum <- max + min
  reversed <- sum - nvar
  
  if (!is.factor(var))
    return(reversed)
  
  levs <- levels(var)
  factor(reversed, labels = rev(levs))
}
