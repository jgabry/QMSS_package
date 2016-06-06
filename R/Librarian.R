#' Load multiple packages with one command
#'
#' @export
#' @param pkgs A character vector of package names. 
#' 
#' @seealso \code{\link[base]{library}}
#' 
#' @examples
#' \dontrun{
#' my.packages <- c("car", "foreign")
#' Librarian(my.packages)
#' }
#' 
Librarian <- function(pkgs){  
  invisible(lapply(
    X = pkgs, 
    FUN = require, 
    character.only = TRUE
  ))
}