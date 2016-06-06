#' Compute standardized regression coefficients for a linear model
#'
#' @export
#' @param fit A model fit with \code{\link[stats]{lm}}.
#' 
#' @return A named vector containing the standardized (beta) coefficients for
#'   \code{fit}.
#' 
#' @examples
#' lm.realinc <- lm(realinc ~ age + educ, data = GSS_2010)
#' stdCoef(lm.realinc)
#'
stdCoef <- function(fit) {
  fit.mod <- fit$model
  fit.coef <- fit$coefficients
  sds <- apply(fit.mod, 2, sd) 
  
  if (names(fit.coef)[1] == "(Intercept)") 
    fit.coef <- fit.coef[-1]
  
  fit.coef * (sds[-1] / sds[1])
}
