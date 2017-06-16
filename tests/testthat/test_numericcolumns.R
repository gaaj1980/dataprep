#importing packages
library(dataprep)
library(testthat)

context("Numeric Columns")

test_that("The names of the numeric columns of startupssmall",{
  current <- getNumericColumns(startupssmall)
  expected <- c("R.D.Spend","Administration","Marketing.Spend","Profit")
  expect_equal(current,expected)
})

test_that("The names of the numeric columns of startupsbig",{
  current <- getNumericColumns(startupsbig)
  expected <- c("R.D.Spend","Administration","Marketing.Spend","Profit")
  expect_equal(current,expected)
})

test_that("The names of the numeric columns of startupshuge",{
  current <- getNumericColumns(startupshuge)
  numericCols <- c("R.D.Spend","Administration","Marketing.Spend","Profit")
  expected <- numericCols
  for(i in seq(1,9)) {
    expected <- c(expected,paste0(numericCols,"_",i))
  }
  expect_equal(current,expected)
})