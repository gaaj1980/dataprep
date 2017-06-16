#importing packages
library(dataprep)
library(testthat)

context("Preprocess a CSV data file")

test_that("Preprocess a data CSV created from the startupssmall dataframe",{
  fprep <- "test_preprocess_data_preprocessed.csv"
  fdata <- "test_preprocess_data.csv"
  write.csv(startupssmall, file = fdata, row.names = FALSE)
  preprocess(fdata)
  expect_true(file.exists(fprep))
  file.remove(c(fprep,fdata))
})

test_that("Preprocess a data CSV created from the startupssmall dataframe",{
  fprep <- "test_preprocess_data_xyz.csv"
  fdata <- "test_preprocess_data.csv"
  write.csv(startupssmall, file = fdata, row.names = FALSE)
  preprocess(fdata, csvSuffix = "_xyz")
  expect_true(file.exists(fprep))
  file.remove(c(fprep,fdata))
})