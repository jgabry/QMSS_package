#' Compute clustered standard errors. 
#'
#' @param fit A model fit with \code{\link[plm]{plm}} (\pkg{plm}).
#' @param cluster.var A character string naming the grouping variable.
#' @param data A data frame containing \code{cluster.var}. \code{data} is only needed 
#' if \code{cluster.var} is not included in \code{index} (see Examples below). 
#' @return Output from \code{\link[lmtest]{coeftest}} (\pkg{lmtest}) but with clustered standard errors. 
#' @author Jonah Gabry <jsg2201@@columbia.edu>
#' @note \code{clusterSE} does not work with models fit with \code{lm}, however a similar model
#' can be fit with \code{\link[plm]{plm}} using the option \code{model = "pooling"}. You can then 
#' use \code{clusterSE} to compute clustered standard errors and retest the coefficients. 
#' @seealso \code{\link[lmtest]{coeftest}}
#' @export
#' @examples
#' \dontrun{
#' # Model from plm help page:
#' data("Produc", package = "plm")
#' fit <- plm(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,
#'            data = Produc, index = c("state","year"), model = "random")
#' 
#' clusterSE(fit, cluster.var = "state") # don't need data argument since "state" is included in index 
#' }
#' 

clusterSE <- function(fit, cluster.var, data){ # note: cluster.var should be entered as character string
  require(plm); require(lmtest)
  
  if (missing(data) & cluster.var %in% colnames(index(fit))){
    cvar <- index(fit, cluster.var)
    n <- length(unique(cvar))
    N <- length(cvar)
  }
  else{
    row.ids <- as.numeric(rownames(model.frame(fit)))
    # 1. get number of clusters (omitting individuals with missingness on "divorce.easier" and/or "divorced")
    n <- length(unique(data[row.ids, cluster.var]))
    # 2. get number of observations (again omitting the same individuals with missingness)
    N <- length(row.ids) 
  }
  
  #3. compute degrees of freedom
  df <- (n/(n - 1)) * (N - 1)/fit$df.residual
  # compute variance-covariance matrix
  vcov <- df*vcovHC(fit, type = "HC0", cluster = "group")
  # retest coefficients  
  coeftest(fit, vcov = vcov)
}