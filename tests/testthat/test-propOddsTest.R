library(QMSS)
library(VGAM)
context("propOddsTest")

pneumo <- transform(pneumo, let = log(exposure.time))
fit1 <- vglm(cbind(normal, mild, severe) ~ let, 
             family = propodds, data = pneumo)
fit2 <- vglm(cbind(normal, mild, severe) ~ let, 
             family = cumulative(reverse = TRUE), data = pneumo)

test_that("propOddsTest returns expected result", {
  expect_equal_to_reference(propOddsTest(fit1, fit2), 
                            file = "propOddsTest-reference-1.rds")
})
