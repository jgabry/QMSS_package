library(QMSS)
library(plm)
context("sigmaRho")

data("Produc")
fit_random <- plm(
  log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,
  data = Produc,
  index = c("state", "year"),
  model = "random"
)
fit_within <- plm(
  log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,
  data = Produc,
  index = c("state", "year"),
  model = "within"
)

test_that("sigmaRho returns expected result with model='random'", {
  ans <- c(sigma_u = 0.0827, sigma_e = 0.0381, rho = 0.8246)
  expect_equal(sigmaRho(fit_random), ans, tol = 0.0001)
})

test_that("sigmaRho returns expected result with model='within'", {
  ans <- c(sigma_u = 0.0905, sigma_e = 0.0381, rho = 0.8494)
  expect_equal(sigmaRho(fit_within), ans, tol = 0.0001)
})

test_that("sigmaRho prints to console", {
  expect_output(sigmaRho(fit_within), "fraction of variance due to u_i")
  expect_output(sigmaRho(fit_random), "fraction of variance due to u_i")
})
