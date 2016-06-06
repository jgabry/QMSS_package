library(QMSS)
library(plm)
context("sigmaRho")

data("Produc")
fit <- plm(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,
           data = Produc, index = c("state","year"), model = "random")
sigmaRho(fit)

test_that("sigmaRho returns expected result", {
  a <- c(sigma_u = 0.0827, sigma_e = 0.0381, rho = 0.8246)
  expect_equal(sigmaRho(fit), a, tol = 0.0001)
})

test_that("sigmaRho prints to console", {
  expect_output(sigmaRho(fit), "fraction of variance due to u_i")
})
