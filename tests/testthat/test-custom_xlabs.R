library(QMSS)
library(ggplot2)
context("custom_xlabs")

test_that("custom_xlabs equal to reference", {
  expect_equal_to_reference(custom_xlabs(), 
                            file = "custom_xlabs-reference-1.rds")
})

test_that("custom_xlabs returns correct values", {
  x1 <- custom_xlabs()
  x2 <- custom_xlabs(angle = 10, vjust = 0.9, color = "blue")
  expect_equal(x1[[1]], element_text(angle = 90, vjust = 0.5))
  expect_equal(x2[[1]], element_text(angle = 10, vjust = 0.9, color = "blue"))
})