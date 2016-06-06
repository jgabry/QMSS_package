library(QMSS)
context("reverseCode")

test_that("reverseCode works for numerics", {
  x <- c(1,1,1,2,3,3)
  y <- reverseCode(x)
  expect_identical(y, c(3,3,3,2,1,1))
})

test_that("reverseCode works for factors", {
  x2 <- as.factor(month.abb)
  y2 <- reverseCode(x2)
  expect_identical(levels(y2), rev(levels(x2)))
})

test_that("ReverseThis throws deprecation warning", {
  x <- c(1,1,1,2,3,3)
  expect_warning(ans <- ReverseThis(x), "Use 'reverseCode' instead")
  expect_identical(ans, reverseCode(x))
})
