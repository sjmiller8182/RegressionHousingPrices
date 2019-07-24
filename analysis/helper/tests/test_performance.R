source("../performance.R", chdir = TRUE)
library(testthat)

test_that("PRESS - typical", {
  expect_equal(TRUE, all.equal(0.4225, PRESS(c(1,2,1,3),c(1.1,2.5,1.05,3.4)), tol=0.0001))
})

test_that("MSE - typical", {
  expect_equal(TRUE, all.equal(0.105625, MSE(c(1,2,1,3),c(1.1,2.5,1.05,3.4)), tol=0.0001))
})
