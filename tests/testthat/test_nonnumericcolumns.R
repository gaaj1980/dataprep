#importing packages
library(dataprep)
library(testthat)

context("Non-Numeric Columns")

test_that("The names of the non-numeric columns of startupssmall",{
  current <- getNonNumericColumns(startupssmall)
  expected <- c("State")
  expect_equal(current,expected)
})

test_that("The names of the non-numeric columns of startupshuge",{
  current <- getNonNumericColumns(startupshuge)
  expected <- c("State","State_1","State_2","State_3","State_4",
                   "State_5","State_6","State_7","State_8","State_9")
  expect_equal(current,expected)
})