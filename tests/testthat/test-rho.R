library(QMSS)
library(lme4)
context("rho")

fit <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)

test_that("rho returns expected result", {
  expect_equal(rho(fit), 0.9458069, tol = 0.0001)
})
