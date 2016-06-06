library(QMSS)
context("logitOR")

Y <- GSS_2010$realinc
Y <- cut(Y,
         breaks = c(-Inf, median(Y, na.rm = TRUE), Inf),
         labels = c("Low", "High"))
X <- with(GSS_2010, cbind(age, educ))
fit <- glm(Y ~ X, family = binomial)

test_that("logitOR returns the right values", {
  expect_equal_to_reference(logitOR(fit), 
                            file = "logitOR-reference-1.rds")
  expect_equal_to_reference(logitOR(fit, intercept = FALSE, level = 0.90), 
                            file = "logitOR-reference-2.rds")
})
