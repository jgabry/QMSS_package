library(QMSS)
context("Librarian")

test_that("Library throws warnings", {
  expect_warning(Librarian(c("asg3G2GGD57ksh", "ggplot2")), 
                 "there is no package")
})

test_that("Library returns expected result", {
  x <- Librarian(c("ggplot2", "reshape2"))
  expect_identical(x, list(TRUE, TRUE))
})
