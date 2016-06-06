#' Predicted probabilities and confidence intervals from logit or probit model
#'
#' @export
#' @param model A \code{\link[stats]{glm}} model fit with \code{binomial} family
#'   and either a \code{logit} or \code{probit} link function.
#' @param predData A data frame to pass to \code{\link[stats]{predict.glm}} in
#'   which to look for variables with which to predict.
#' @param ci Logical value indicating whether to compute confidence intervals.
#' @param level The confidence level to use if \code{ci} is \code{TRUE}.
#' 
#' @return A data frame with \code{predData} and the associated predicted
#'   probabilities. Confidence intervals are included if argument \code{ci} is
#'   \code{TRUE}.
#'   
#' @examples
#' GSS_2010$Y <- with(GSS_2010, 
#'                    cut(realinc, 
#'                    breaks=c(-Inf, median(realinc, na.rm = T), Inf), 
#'                    labels=c("Low", "High")))
#' logitmodel <- glm(Y ~ age + educ, data = GSS_2010, family = binomial)
#' probitmodel <- glm(Y ~ age + educ, data = GSS_2010, family = binomial(link = "probit"))
#' predData <- data.frame(age = 20, educ = 15)
#' predProb(logitmodel, predData, ci = F)
#' predProb(probitmodel, predData, ci= F)
#' predData <- expand.grid(age = c(20, 50, 80), educ = c(5, 10, 15))
#' predProb(logitmodel, predData, ci = T)
#' predProb(probitmodel, predData, ci= T)
#'
predProb <- function(model,
                     predData,
                     ci = TRUE,
                     level = 0.95) {
  link <- model$family$link
  if (!(link %in% c("logit", "probit")))
    stop("Link function should be 'logit' or 'probit'")
  
  if (!ci) {
    preds <- predict(model, type = "response", newdata = predData)
    preds <- cbind(predData, PredictedProb = preds)
    return(preds)
  }
  temp <- predict(model, newdata = predData, type = "link", se = TRUE)
  fit <- temp$fit
  se <- temp$se.fit
  p <- (1 - level) / 2
  p <- c(p, 1 - p)
  
  fun <- ifelse(link == "probit", "pnorm", "plogis")
  PredictedProb <- do.call(fun, args = list(q = fit))
  ci1 <- do.call(fun, args = list(q = fit + qnorm(p[1]) * se))
  ci2 <- do.call(fun, args = list(q = fit + qnorm(p[2]) * se))
  CI <- cbind(ci1, ci2)
  colnames(CI) <- paste0(paste(100 * p), "%")
  cbind(predData, PredictedProb, CI)
}
