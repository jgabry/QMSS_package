library(QMSS)
context("Librarian")

my_pkgs <- c("ggplot2", "lmtest")

test_that("Library loads packages", {
  expect_false(any(my_pkgs %in% .packages()))
  
  Librarian(my_pkgs)
  expect_true(all(my_pkgs %in% .packages()))
})

test_that("Library returns expected result", {
  x <- Librarian(my_pkgs)
  expect_identical(x, list(TRUE, TRUE))
})
