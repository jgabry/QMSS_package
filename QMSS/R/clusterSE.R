#' Compute clustered standard errors. 
#'
#' @param fit A model fit with \code{plm}.
#' @param cluster.var A character string naming the grouping variable.
#' @param data A data frame containing \code{cluster.var}. 
#' @return Output from \code{coeftest} (\pkg{lmtest}) but with clustered standard errors. 
#' @author Jonah Gabry <jsg2201@@columbia.edu>
#' @export
#' @examples
#' clusterSE(fit, cluster.var, data)

clusterSE <- function(fit, cluster.var, data){ # note: cluster.var should be entered as character string
  require(plm); require(lmtest)
  row.ids <- as.numeric(rownames(model.frame(fit)))
  # 1. get number of clusters (omitting individuals with missingness on "divorce.easier" and/or "divorced")
  n <- length(unique(data[row.ids, cluster.var]))
  # 2. get number of observations (again omitting the same individuals with missingness)
  N <- length(row.ids)
  #3. compute degrees of freedom
  df <- (n/(n - 1)) * (N - 1)/fit$df.residual
  # compute variance-covariance matrix
  vcov <- df*vcovHC(fit, type = "HC0", cluster = "group")
  # retest coefficients  
  coeftest(fit, vcov = vcov)
}