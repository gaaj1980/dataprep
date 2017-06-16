#importing packages
library(dataprep)
library(testthat)

context("Preprocess NaN rows")

nadf <- startupssmall
nadf[2,1] <- NaN
nadf[3,2] <- NaN
nadf[4,3] <- NaN
nadf[6,3] <- NaN
nadf[7,5] <- NaN

test_that("Removes NaN rows",{
  current <- preprocessNaN(nadf, naBehavior = 0)
  expect_equal(length(current[,1]), 45)
  expect_false(any(is.na(current)))
})

test_that("Mean NaN columns",{
  current <- preprocessNaN(nadf, naBehavior = 1)
  expect_equal(length(current[,1]), 50)
  expect_false(any(is.na(current)))
  expect_equal(round(current[2,1],digits=2), 71907.82)
  expect_equal(round(current[3,2],digits=2), 121756.87)
  expect_equal(round(current[4,3],digits=2), 204274.87)
  expect_equal(round(current[6,3],digits=2), 204274.87)
  expect_equal(round(current[7,5],digits=2), 111112.44)
})

test_that("Median NaN columns",{
  current <- preprocessNaN(nadf, naBehavior = 2)
  expect_equal(length(current[,1]), 50)
  expect_false(any(is.na(current)))
  expect_equal(round(current[2,1],digits=1), 72107.6)
  expect_equal(round(current[3,2],digits=1), 122782.8)
  expect_equal(round(current[4,3],digits=1), 208157.7)
  expect_equal(round(current[6,3],digits=1), 208157.7)
  expect_equal(round(current[7,5],digits=1), 107404.3)
})

test_that("Mode NaN columns",{
  current <- preprocessNaN(nadf, naBehavior = 3)
  expect_equal(length(current[,1]), 50)
  expect_false(any(is.na(current)))
  expect_equal(current[2,1], 0)
  expect_equal(current[3,2], 136897.80)
  expect_equal(current[4,3], 0.0)
  expect_equal(current[6,3], 0.0)
  expect_equal(current[7,5], 192261.83)
})

test_that("Invalid naBehavior",{
  expect_error(preprocessNaN(nadf, naBehavior = 4))
})