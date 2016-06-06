library(QMSS)
context("Tab")

x <- c(1,1,1,2,3,3,NA,NA)
y <- c(1,1,1,2,3,3)

test_that("Tab returns matrix", {
  expect_true(is.matrix(Tab(x)))
  expect_identical(ncol(Tab(x)), 3L)
})

test_that("Tab returns expected result with fake data", {
  ans <- rbind(
    Tab(x),
    Tab(x, useNA = "ifany"), 
    Tab(y, useNA = "always")
  )
  expect_equal_to_reference(ans, "Tab-reference-1.rds")
})

test_that("Tab returns expected result with GSS data", {
  ans <- with(GSS_2010,
              rbind(
                Tab(partyid, digits = 3, useNA = "ifany"),
                Tab(partyid)
                )
              )
  expect_equal_to_reference(ans, "Tab-reference-2.rds")
})


