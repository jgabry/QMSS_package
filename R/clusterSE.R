#' Clustered standard errors
#'
#' @export
#' @param fit A model fit with \code{\link[plm]{plm}}.
#' @param cluster.var A character string naming the grouping/cluster variable.
#' @param data A data frame containing \code{cluster.var}. Only needed if 
#'   \code{cluster.var} was not included in \code{index} when the model was fit
#'   using \code{\link[plm]{plm}}. See 'Examples' below.
#'   
#' @return Output from \code{\link[lmtest]{coeftest}} but with clustered
#'   standard errors.
#' 
#' @note \code{clusterSE} does not work with models fit with \code{lm}, however 
#'   a similar model can be fit with \code{\link[plm]{plm}} using the option 
#'   \code{model = "pooling"}. You can then use \code{clusterSE} to compute 
#'   clustered standard errors and retest the coefficients.
#' 
#' @seealso \code{\link[lmtest]{coeftest}}, \code{\link[plm]{plm}}
#' 
#' @examples
#' # Model from plm help page:
#' library(plm)
#' data("Produc")
#' fit <- plm(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,
#'            data = Produc, index = c("state","year"), model = "random")
#' 
#' # don't need data argument since "state" is included in index 
#' clusterSE(fit, cluster.var = "state") 
#' 
#' # need data argument since "state" not included in index 
#' fit2 <- update(fit, index = "year")
#' clusterSE(fit2, cluster.var = "state", data = Produc)
#' 
clusterSE <- function(fit, cluster.var, data){
  if (!requireNamespace("lmtest", quietly = TRUE))
    stop("Please install the 'lmtest' package to use this function.")
  
  # get number of clusters and observations
  if (missing(data) & cluster.var %in% colnames(plm::index(fit))){
    cvar <- plm::index(fit, cluster.var)
    n <- length(unique(cvar))
    N <- length(cvar)
  } else {
    stopifnot(is.data.frame(data))
    row.ids <- as.numeric(rownames(model.frame(fit)))
    n <- length(unique(data[row.ids, cluster.var]))
    N <- length(row.ids)
  }
  
  # compute degrees of freedom and variance-covariance matrix
  df <- (n / (n - 1)) * (N - 1) / fit$df.residual
  vcov <- df * plm::vcovHC(fit, type = "HC0", cluster = "group")
  
  lmtest::coeftest(fit, vcov = vcov)
}
