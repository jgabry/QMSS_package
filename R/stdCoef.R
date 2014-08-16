#' Compute standardized regression coefficients for a linear model
#'
#' @param fit A model fit with \code{\link[stats]{lm}}.
#' @return A named vector containing the standardized (beta) coefficients for \code{fit}.
#' @author Jonah Gabry <jsg2201@@columbia.edu>
#' @export
#' @examples
#' lm.realinc <- lm(realinc ~ age + educ, data = GSS_2010)
#' stdCoef(lm.realinc)

stdCoef <- function(fit){
  fit.mod <- fit$model
  fit.name <- deparse(substitute(fit))
  fit.coef <- fit$coef[-1] 
  
  sd <- apply(fit.mod, MARGIN = 2, sd) 
  std.coef <- fit.coef*(sd[-1]/sd[1])
  
  cat("Standardized Coefficients for", fit.name, "\n") 
  return(std.coef)
}

