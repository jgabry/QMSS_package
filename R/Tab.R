#' Simple table of counts and percentages
#'
#' @param var A variable.
#' @param digits An integer indicating the number of decimals places to be used for rounding.
#' @param useNA Should \code{NA} values be tabulated? Defaults to \code{"no"}. 
#' @details If \code{useNA = "ifany"} then \code{NA} values will be included if any exist. 
#' If \code{useNA = "always"} then if \code{var} does not contain any \code{NA} values a count, 
#' percentage, and cumulative percentage of 0 will be reported. 
#' @return A table of counts, percentages, and cumulative percentages for levels of \code{var}. 
#' @author Jonah Gabry <jsg2201@@columbia.edu>
#' @seealso \code{\link[stats]{xtabs}}, \code{\link[base]{table}}, \code{\link[base]{prop.table}}
#' @export
#' @examples
#' x <- c(1,1,1,2,3,3,NA,NA)
#' y <- c(1,1,1,2,3,3)
#' Tab(x)
#' Tab(x, useNA = "ifany")
#' Tab(y, useNA = "always")
#' with(GSS_2010, Tab(partyid, digits = 3, useNA = "ifany"))

Tab <- function(var, digits = 2, useNA = c("no", "ifany", "always")) {
  Count <- table(var, useNA = useNA) 
  Pct <- 100*prop.table(Count) 
  Cum.Pct <- cumsum(Pct)
  cbind(Count, Pct = round(Pct, digits), Cum.Pct = round(Cum.Pct, digits)) 
}