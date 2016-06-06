#' Table of odds ratios and confidence intervals for logit models 
#'
#' @export
#' @param fit A logit model fit with \code{\link[stats]{glm}}.
#' @param intercept A logical value indicating whether the intercept should be
#'   included in the output. Defaults to \code{TRUE}. If \code{fit} does not
#'   include an intercept the \code{intercept} argument should be left at the
#'   default value of \code{TRUE}.
#' @param level The desired confidence level.
#' 
#' @return Odds ratios and confidence intervals for the parameters in \code{fit}.
#' 
#' @examples
#' Y <- GSS_2010$realinc
#' Y <- cut(Y,
#'          breaks = c(-Inf, median(Y, na.rm = TRUE), Inf),
#'          labels = c("Low", "High"))
#' X <- with(GSS_2010, cbind(age, educ))
#' fit <- glm(Y ~ X, family = binomial)
#' logitOR(fit)
#' logitOR(fit, intercept = FALSE, level = 0.90)
#'
logitOR <- function(fit, intercept = TRUE, level = 0.95){
  OddsRatio <- exp(coef(fit))
  ci <- exp(confint(fit, level = level))
  out <- cbind(OddsRatio, ci)
  if (!intercept)
    out <- out[-1,, drop=FALSE]
  
  out
}
