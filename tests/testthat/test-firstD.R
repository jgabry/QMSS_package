library(QMSS)
library(plyr)
context("firstD")

df <- data.frame(id = rep(1:3, each = 3), 
                 X = c(8, 7, 9, 10, 12, 10, 7, 9, 5), 
                 Y = c(5, 2, 7, 2, 9, 5, 3, 3, 4))

test_that("firstD throws error if df specified but not group", {
  expect_error(firstD(X, df = df), 
               "if 'df' is specified then 'group' must also be specified")
})

test_that("firstD works when specifying both group and df", {
  df$Xdiff <- firstD(X, id, df)
  expect_equal_to_reference(df, file = "firstD-reference-1.rds")
})

test_that("firstD works when omitting df", {
  ans <- firstD(var = df$X, group = df$id)
  expect_equal_to_reference(ans, file = "firstD-reference-2.rds")
})

test_that("firstD works when omitting both group and df", {
  ans <- ddply(df, "id", mutate, Xdiff = firstD(X), Ydiff = firstD(Y))
  expect_equal_to_reference(ans, file = "firstD-reference-3.rds")
})
