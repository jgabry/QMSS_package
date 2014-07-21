#' Test proportional odds assumption for ordinal logit models
#'
#' @param fit An ordinal logit model fit with \code{vglm} (\pkg{VGAM}) with \code{family = propodds}
#' @param relaxed.fit An ordinal logit model fit with \code{vglm} (\pkg{VGAM}) with \code{family = cumulative(reverse = TRUE)}
#' @return A list consisting of 
#' \item{chi.sq}{The chi-squared test statistic}
#' \item{df}{The degrees of freedom parameter for the chi-squared distribution}
#' \item{p.value}{The associated p-value}
#' @author Jonah Gabry <jsg2201@@columbia.edu>
#' @seealso \code{\link{vglm}} 
#' @export
#' @examples
#' propOddsTest(fit, relaxed.fit)

propOddsTest <- function(fit, relaxed.fit){ 
  dev1 <- deviance(fit); dev2 <- deviance(relaxed.fit) # model deviances (deviance =  -2*log-likelihood ratio)
  df.res1 <- df.residual(fit); df.res2 <- df.residual(relaxed.fit) # degrees of freedom
  chi.sq <- dev1 - dev2 # the chi-squared statistic is the the difference of the deviances
  df <- df.res1 - df.res2 # the df for the chi-square distribution is the difference of the df
  p.val <- pchisq(chi.sq, df, lower.tail = F) # compute p-value using chi-squared distribution
  return(list(message = "Null hypothesis: no violation of assumption.",
              chi.sq = chi.sq, 
              df = df, 
              p.val = p.val))
}