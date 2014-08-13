#' Load multiple packages with one command
#'
#' @param pkgs A character vector of package names. 
#' @details The \code{Librarian} function loads the packages named in \code{pkgs}.  
#' @author Jonah Gabry <jsg2201@@columbia.edu>
#' @seealso \code{\link[base]{library}}
#' @export
#' @examples
#' my.packages <- c("car", "foreign")
#' Librarian(my.packages)

Librarian <- function(pkgs){  
  invisible(lapply(
    X = pkgs, 
    FUN = require, 
    character.only = TRUE
  ))
}