library(QMSS)
context("predProb")

GSS_2010$Y <-
  with(GSS_2010, cut(
    realinc,
    breaks = c(-Inf, median(realinc, na.rm = TRUE), Inf),
    labels = c("Low", "High")
  ))
logitmodel <- glm(Y ~ age + educ, data = GSS_2010,
                  family = binomial(link = "logit"))
probitmodel <- glm(Y ~ age + educ, data = GSS_2010,
                   family = binomial(link = "probit"))
predData <- data.frame(age = 20, educ = 15)
predData2 <- expand.grid(age = c(20, 50, 80), educ = c(5, 10, 15))

test_that("predProb throws error if not a logit or probit model", {
  cauchitmodel <- glm(Y ~ age + educ, data = GSS_2010,
                     family = binomial(link = "cauchit"))
  expect_error(predProb(cauchitmodel, predData), 
               "Link function should be 'logit' or 'probit'")
})

test_that("predProb returns the right values with ci=FALSE", {
  logit_preds <- predProb(logitmodel, predData, ci = FALSE)
  probit_preds <- predProb(probitmodel, predData, ci = FALSE)
  expect_equal_to_reference(c(logit_preds, probit_preds), 
                            file = "predProb-reference-1.rds")
})

test_that("predProb returns the right values with ci=TRUE", {
  logit_preds <- predProb(logitmodel, predData2, ci = TRUE)
  probit_preds <- predProb(probitmodel, predData2, ci = TRUE)
  expect_equal_to_reference(c(logit_preds, probit_preds), 
                            file = "predProb-reference-2.rds")
})
