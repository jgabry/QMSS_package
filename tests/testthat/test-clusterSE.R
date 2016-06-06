library(QMSS)
library(plm)
context("clusterSE")

data("Produc")

test_that("clusterSE returns expected result", {
  fit <- plm(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp, 
             data = Produc, model = "random", index = c("state", "year"))
  expect_equal_to_reference(
    clusterSE(fit, cluster.var = "state"), 
    file = "clusterSE-reference-1.rds"
    )
})

test_that("clusterSE returns expected result with 'data' argument", {
  fit <- plm(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,
             data = Produc, model = "random", index = "year")
  expect_equal_to_reference(
    clusterSE(fit, cluster.var = "state", data = Produc), 
    file = "clusterSE-reference-2.rds"
    )
})

test_that("clusterSE throws error when 'data' is missing but needed", {
  fit <- plm(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,
             data = Produc, model = "random", index = "year")
  expect_error(clusterSE(fit, cluster.var = "state"), 
               'argument "data" is missing, with no default')
})
