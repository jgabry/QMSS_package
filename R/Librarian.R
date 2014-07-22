#' Load multiple packages
#'
#' @param x A character vector of package names. 
#' @details The \code{Librarian} function loads the packages named in \code{x}.  
#' @author Jonah Gabry <jsg2201@@columbia.edu>
#' @seealso \code{\link[base]{library}}
#' @export
#' @examples
#' my.packages <- c("car", "foreign")
#' Librarian(my.packages)

Librarian <- function(x){
  invisible(
    lapply(X = my.packages, 
           FUN = require, 
           character.only = TRUE)
  )
}