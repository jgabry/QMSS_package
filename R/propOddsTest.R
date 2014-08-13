#' Test proportional odds assumption for ordinal logit models
#'
#' @param fit An ordinal logit model fit with \code{\link[VGAM]{vglm}} (\pkg{VGAM}) 
#' with \code{family = propodds}.
#' @param relaxed.fit An ordinal logit model fit with \code{\link[VGAM]{vglm}} (\pkg{VGAM}) 
#' with \code{family = cumulative(reverse = TRUE)}
#' @return An object of class "htest".
#' @author Jonah Gabry <jsg2201@@columbia.edu>
#' @seealso \code{\link[VGAM]{vglm}} 
#' @export
#' @examples
#' propOddsTest(fit, relaxed.fit)

propOddsTest <- function(fit, relaxed.fit){ 
  require(VGAM)
  
  fit_name <- deparse(substitute(fit))
  relaxed.fit_name <- deparse(substitute(relaxed.fit))
  model.names <- paste("fit =", fit_name, "| relaxed.fit =", relaxed.fit_name)
  
  # model deviances (deviance =  -2*log-likelihood ratio)
  dev1 <- deviance(fit)
  dev2 <- deviance(relaxed.fit) 
  # degrees of freedom
  df.res1 <- df.residual(fit)
  df.res2 <- df.residual(relaxed.fit) 
  # the chi-squared statistic is the the difference of the deviances
  chi.sq <- dev1 - dev2 
  names(chi.sq) <- "Chi-sq"
  # the df for the chi-square distribution is the difference of the df
  df <- df.res1 - df.res2 
  names(df) <- "df"
  # compute p-value using chi-squared distribution
  p.val <- pchisq(chi.sq, df, lower.tail = F) 
  # alternative hypothesis
  H_a <- "Violation of proportional odds assumption"

  out <- list(statistic = chi.sq, p.value = p.val, parameter = df, 
              method = "Proportional Odds Test", data.name = model.names, 
              alternative = H_a)
  
  class(out) <- "htest"
  return(out)
}
