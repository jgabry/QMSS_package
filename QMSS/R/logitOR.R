#' Table of odds ratios and confidence intervals for logit models 
#'
#' @param fit A logit model fit with \code{\link{glm}}.
#' @param intercept A logical value indicating whether the intercept should be included in the output. 
#' Defaults to \code{TRUE}. If \code{fit} does not include an intercept the \code{intercept} argument
#' should be kept at the default value of \code{TRUE}. 
#' @param level The desired confidence level to be passed to \code{\link{confint.glm}} (\pkg{MASS}). 
#' @return Odds ratios and confidence intervals for the parameters in \code{fit}.  
#' @author Jonah Gabry <jsg2201@@columbia.edu>
#' @export
#' @examples
#' Y <- GSS_2010$realinc;  X <- with(GSS_2010, cbind(age, educ))  
#' Y <- cut(Y, breaks=c(-Inf, median(Y, na.rm = T), Inf), 
#'             labels=c("Low", "High"))
#' fit <- glm(Y ~ X, family = binomial)
#' logitOR(fit)
#' logitOR(fit, intercept = FALSE, level = 0.90)

logitOR <- function(fit, intercept = TRUE, level = 0.95){
  require(MASS)
  
  OddsRatio <- exp(coef(fit))
  ci <- exp(confint(fit, level = level))
  out <- cbind(OddsRatio, ci)
  
  if (intercept == FALSE) {
    out <- out[-1, ] # remove intercept
    return(out)
  }
  
  return(out)  
}