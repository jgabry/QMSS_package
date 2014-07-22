#' Compute sigma_u, sigma_e, and rho after fixed or random effects model
#'
#'
#' @param fit A model fit with \code{\link{plm}} (\pkg{plm}) and 
#' \code{model = "within"} or \code{model = "random"}.  
#' @return A numeric vector containing the named components \itemize{
#' \item{\code{sigma_u}}{ Error due to differences between units.}
#' \item{\code{sigma_e}}{ Error due to differences within units.}
#' \item{\code{rho}}{ Proportion of variance due to unit effects.}
#' } Results are also printed to the console. 
#' @author Jonah Gabry <jsg2201@@columbia.edu>
#' @seealso \code{\link{plm}}
#' @export
#' @examples
#' \dontrun{
#' # Model from plm help page:
#' data("Produc", package = "plm")
#' fit <- plm(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,
#'            data = Produc, index = c("state","year"), model = "random")
#' sigmaRho(fit) 
#' }
#' 

sigmaRho <- function(fit){
  require(plm)
  
  model <- fit$args$model
  
  if (!(model %in% c("within", "random"))) {
    stop("plm's 'model' argument must be set to 'within' or 'random'")
  } 
  
  if (model == "within") {
    sigma_u <- sd(fixef(fit))
    
    rfit <- update(fit, model = "random")
    ercomp <- ercomp(rfit)[["sigma2"]]
    sigma_e <- sqrt(ercomp$idios)
  }
  else {
    ercomp <- ercomp(fit)[["sigma2"]]
    sigma_u <- sqrt(ercomp$id)
    sigma_e <- sqrt(ercomp$idios)
  }
  
  rho <- sigma_u^2/(sigma_u^2 + sigma_e^2)
  
  out <- c(sigma_u = sigma_u, sigma_e = sigma_e, rho = rho)
  
  cat("sigma_u = ", round(sigma_u,5), collapse = "\n")
  cat("sigma_e = ", round(sigma_e,5), collapse = "\n")
  cat("    rho = ", round(rho,5), 
      "(fraction of variance due to u_i)", collapse = "\n")
  
  return(invisible(out))
}