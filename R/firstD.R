#' Compute first differences by group
#'
#' @param var Variable to be first-differenced.
#' @param group Optional grouping variable (see Details).
#' @param df Optional data frame containing \code{var} and \code{group} (see Details). 
#' @details If \code{df} is specified then \code{group} must also be specified. So it is possible 
#' to specify all three parameters, \code{var} and \code{group} only, or \code{var} by itself. 
#' An example of when one might wish to omit both \code{group} and \code{df} is when using \code{firstD} 
#' in conjunction with  \pkg{plyr}'s \code{\link[plyr]{ddply}} (see Examples). If \code{df} is specified then it 
#' should be sorted by \code{group} and, if necessary, a second variable (e.g. time) that orders the 
#' observations of \code{var} in the appropriate sequence. 
#' @return \code{firstD(var)} returns a first-differenced version of \code{var}. 
#' \code{firstD(var,group)} returns a first-differenced version of \code{var} by \code{group}. 
#' And \code{firstD(var,group,df)} returns a first-differenced version of \code{var} by \code{group}, 
#' where \code{var} and \code{group} are searched for in \code{df}. 
#' @author Jonah Gabry <jsg2201@@columbia.edu>
#' @export
#' @examples
#' # Specifying both group and df
#' df <- data.frame(id = rep(1:3, each = 3), X = rpois(9, 10))
#' df$Xdiff <- firstD(X, id, df)
#' df
#' 
#' # Omitting df
#' id <- rep(1:3, each = 3)
#' X <- rpois(9, 10)
#' Xdiff <- firstD(X, id)
#' 
#' # Omitting group and df 
#' \dontrun{
#' library(plyr)
#' df <- data.frame(id = rep(1:3, each = 3), X = rpois(9, 10), Y = rpois(9, 5))
#' ddply(df, "id", mutate, Xdiff = firstD(X), Ydiff = firstD(Y))
#' }

firstD <- function(var, group, df){
  bad <- (missing(group) & !missing(df))
  if (bad) stop("if df is specified then group must also be specified")
  
  fD <- function(j){ c(NA, diff(j)) }
  
  var.is.alone <- missing(group) & missing(df)
  
  if (var.is.alone) {
    return(fD(var))
  }
  if (missing(df)){
    V <- var
    G <- group
  }
  else{
    V <- df[, deparse(substitute(var))]
    G <- df[, deparse(substitute(group))]
  }
  
  G <- list(G)
  D.var <- by(V, G, fD)
  unlist(D.var)
}