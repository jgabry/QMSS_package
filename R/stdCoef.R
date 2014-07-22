#' Compute standardized regression coefficients
#'
#' @param fit A model fit with \code[stats]{lm}.
#' @return Standardized (beta) coefficients from a linear model.
#' @author Jonah Gabry <jsg2201@@columbia.edu>
#' @export
#' @examples
#' fit <- lm(realinc ~ age + educ, data = GSS_2010)
#' stdCoef(fit)

stdCoef <- function(fit){
  sd <- apply(X = fit$model, MARGIN = 2, FUN = sd) 
  coefficients <- fit$coef[-1] 
  std.coefs <-coefficients*(sd[-1]/sd[1])
  cat("Standardized Coefficients for ", deparse(substitute(fit)), "\n") 
  
  return(std.coefs)
}