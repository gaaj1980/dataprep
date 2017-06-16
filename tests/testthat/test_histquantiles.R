#importing packages
library(dataprep)
library(testthat)

context("Histogram by data quantiles")

test_that("Histogram c(rep(1,10),rep(2,10)) in 2 groups using function histQuantiles",{
  d <- c(rep(1,10),rep(2,10))
  current <- histQuantiles(name = "Test Groups", data = d, n = 2, plot = FALSE)
  expect_equal(as.vector(current$breaks), c(1.0,1.5,2.0))
  expect_equal(as.vector(current$counts), c(10,10))
  v <- as.vector(current$density[1]) == as.vector(current$density)
  expect_true(all(v))
})

test_that("Histogram c(seq(1,2,0.1),seq(3,4,0.1)) in 2 groups using function histQuantiles",{
  d <- c(seq(1,2,0.1),seq(3,4,0.1))
  current <- histQuantiles(name = "Test Groups", data = d, n = 2, plot = FALSE)
  expect_equal(as.vector(current$breaks), c(1.0,2.5,4.0))
  expect_equal(as.vector(current$counts), c(11,11))
  v <- as.vector(current$density[1]) == as.vector(current$density)
  expect_true(all(v))
})

test_that("Histogram c(seq(from=1, to=5, by=0.1), seq(from=6, to=10, by=0.1)) in 2 groups using function histQuantiles",{
  d <- c(seq(from=1, to=5, by=0.1), seq(from=6, to=10, by=0.1))
  current <- histQuantiles(name = "Test Groups", data = d, n = 2, plot = FALSE)
  expect_equal(as.vector(current$breaks), c(1.0,5.5,10.0))
  expect_equal(as.vector(current$counts), c(41,41))
  v <- as.vector(current$density[1]) == as.vector(current$density)
  expect_true(all(v))
})