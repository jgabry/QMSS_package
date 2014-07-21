#' Compute fraction of variance due to u_i
#'
#'
#' @param fit A model fit with \code{\link{lmer}} (\pkg{lme4}).
#' @details Note that in the output from \code{summary(fit)} the Std. Dev. column in the Random Effects section
#' contains what STATA refers to as sigma_u and sigma_e. STATA also reports rho (fraction of variance due to u_i)
#' but this is not given in the R output. The \code{rho} function computes this value.
#' @return A number between 0 and 1. The fraction of variance due to u_i.
#' @author Jonah Gabry <jsg2201@@columbia.edu>
#' @export
#' @examples
#' rho(fit)

rho <- function(fit){
  require(lme4)
  varcor <- VarCorr(fit) # extract the variance components using VarCorr()
  varcor <- as.data.frame(varcor)[, "sdcor"] # get just the std devs we want
  sigma_u <- varcor[1] # get sigma_u
  sigma_e <- varcor[2] # get sigma_e
  rho <- sigma_u^2 / (sigma_u^2 + sigma_e^2) # compute rho (fraction of variance due to u_i)
  rho
}