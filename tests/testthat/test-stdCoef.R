library(QMSS)
context("stdCoef")

lm.realinc1 <- lm(realinc ~ age + educ, data = GSS_2010)
lm.realinc2 <- lm(realinc ~ 0 + age + educ, data = GSS_2010)

test_that("stdCoef returns expected result", {
  a <- c(age = 0.070138, educ = 0.42284)
  expect_equal(stdCoef(lm.realinc1), a, tol = 0.0001)
})

test_that("stdCoef returns expected result if no intercept", {
  a <- c(age = -0.030305, educ = 0.26485)
  expect_equal(stdCoef(lm.realinc2), a, tol = 0.0001)
})