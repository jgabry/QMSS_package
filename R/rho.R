#' Compute fraction of variance due to u_i after fitting a model with
#' \code{lmer}
#'
#'
#' @param fit A model fit with \code{\link[lme4]{lmer}}.
#' @details Note that in the output from \code{summary(fit)} the Std. Dev.
#'   column in the Random Effects section contains what STATA refers to as
#'   sigma_u and sigma_e. STATA also reports rho (fraction of variance due to
#'   u_i) but this is not given in the R output. The \code{rho} function
#'   computes this value.
#'   
#' @return A number between 0 and 1. The fraction of variance due to u_i.
#' 
#' @export
#' @examples
#' library(lme4)
#' fit <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
#' rho(fit)
#'
rho <- function(fit){
  if (!requireNamespace("lme4", quietly = TRUE))
    stop("Please install the 'lme4' package to use this function.")
  varcor <- lme4::VarCorr(fit)
  varcor <- as.data.frame(varcor)[, "sdcor"]
  sigma_u <- varcor[1] 
  sigma_e <- varcor[2]
  
  # fraction of variance due to u_i
  sigma_u^2 / (sigma_u^2 + sigma_e^2) 
}
