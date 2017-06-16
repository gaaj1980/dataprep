#importing packages
library(dataprep)
library(testthat)

context("Discretize Dataframe Columns")

test_that("The default names of the numeric columns of startupssmall",{
  current <- discretizeColumnsByQuantiles(startupssmall)
  expected <- c("R.D.Spend","Administration","Marketing.Spend","State","Profit")
  expected <- c(expected,"R.D.Spend_dscr","Administration_dscr","Marketing.Spend_dscr","Profit_dscr")
  expect_equal(names(current),expected)
})

test_that("The new names of the numeric columns of startupssmall, informing the suffix '_class'",{
  current <- discretizeColumnsByQuantiles(startupssmall, suffix = "_class")
  expected <- c("R.D.Spend","Administration","Marketing.Spend","State","Profit")
  expected <- c(expected,"R.D.Spend_class","Administration_class","Marketing.Spend_class","Profit_class")
  expect_equal(names(current),expected)
})

test_that("A subset of the numeric columns of startupssmall",{
  current <- discretizeColumnsByQuantiles(startupssmall, columns = c("Marketing.Spend","R.D.Spend"))
  expected <- c("R.D.Spend","Administration","Marketing.Spend","State","Profit")
  expected <- c(expected,"Marketing.Spend_dscr","R.D.Spend_dscr")
  expect_equal(names(current),expected)
})

test_that("A subset of the numeric columns of startupssmall",{
  current <- discretizeColumnsByQuantiles(startupssmall, 
                                          columns = c("Marketing.Spend","R.D.Spend"),
                                          ns=c(2,5))
  expect_equal(sort(unique(current[,"Marketing.Spend_dscr"])),c(1,2))
  expect_equal(sort(unique(current[,"R.D.Spend_dscr"])),c(1,2,3,4,5))
})