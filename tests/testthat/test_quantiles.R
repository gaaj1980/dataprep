#importing packages
library(dataprep)
library(testthat)

context("Data quantiles")

#testing code
#data with 10 elements 0, 10 elements 1...10 elements 10
intdata1 <- rep(0:10,10)
floatdata1 <- rep(seq(from=0, to=10, by=0.1), 10)

                
test_that("Split intdata1 in 1, 2, 3, 4, and 5 groups using function dataQuantiles",{
  current <- dataQuantiles(data = intdata1, n = 1)
  expected <- c(0,10)
  names(expected) <- c("0%","100%")
  expect_equal(current, expected)

  current <- dataQuantiles(data = intdata1, n = 2)
  expected <- c(0,5,10)
  names(expected) <- c("0%","50%","100%")
  expect_equal(current, expected)
  
  current <- dataQuantiles(data = intdata1, n = 3)
  #since values are integers, they are rounded, thus 3.333 becomes 3 and 6.667 becomes 7
  expected <- c(0,3,7,10)
  names(expected) <- c("0%","33.33333%","66.66667%","100%")
  expect_equal(current, expected)
  
  current <- dataQuantiles(data = intdata1, n = 4)
  #since values are integers, they are rounded, thus 2.5 becomes 2 and 7.5 becomes 8
  expected <- c(0.0,2,5.0,8,10.0)
  names(expected) <- c("0%","25%","50%","75%","100%")
  expect_equal(current, expected)
  
  current <- dataQuantiles(data = intdata1, n = 5)
  expected <- c(0,2,4,6,8,10)
  names(expected) <- c("0%","20%","40%","60%","80%","100%")
  expect_equal(current, expected)
})

test_that("Split floatdata1 in 1, 2, 3, 4, and 5 groups using function dataQuantiles",{
  current <- dataQuantiles(data = floatdata1, n = 1)
  expected <- c(0.0,10.0)
  names(expected) <- c("0%","100%")
  expect_equal(current, expected)
  
  current <- dataQuantiles(data = floatdata1, n = 2)
  expected <- c(0.0,5.0,10.0)
  names(expected) <- c("0%","50%","100%")
  expect_equal(current, expected)
  
  current <- dataQuantiles(data = floatdata1, n = 3)
  #since values the floats have a single decimal, 3.3333 becomes 3.3 and 6.6667 becomes 6.7
  expected <- c(0.0,3.3,6.7,10.0)
  names(expected) <- c("0%","33.33333%","66.66667%","100%")
  expect_equal(current, expected)
  
  current <- dataQuantiles(data = floatdata1, n = 4)
  expected <- c(0.0,2.5,5.0,7.5,10.0)
  names(expected) <- c("0%","25%","50%","75%","100%")
  expect_equal(current, expected)
  
  current <- dataQuantiles(data = floatdata1, n = 5)
  expected <- c(0.0,2.0,4.0,6.0,8.0,10.0)
  names(expected) <- c("0%","20%","40%","60%","80%","100%")
  expect_equal(current, expected)
})