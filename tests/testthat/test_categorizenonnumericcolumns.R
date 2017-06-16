#importing packages
library(dataprep)
library(testthat)

context("Categorize non-numeric columns")

testdf <- startupssmall
col2 <- rev(paste0(startupssmall[,4],"_2"))
col3 <- c()
for(i in 1:length(startupssmall[,4])) {
  col3 <- c(col3,paste(startupssmall[i,4],i))
}
testdf <- cbind(startupssmall,col2,col3)

test_that("Categorize non-numeric columns",{
  current <- categorizeNonNumericColumns(testdf)
  expect_equal(length(current), 10)
  expect_equal(names(current)[8:10], c("State_ctgr","col2_ctgr","col3_ctgr"))
  
  catState <- as.vector(testdf[,4])
  catState <- replace(catState, catState=="New York", 1)
  catState <- replace(catState, catState=="California", 2)
  catState <- replace(catState, catState=="Florida", 3)
  catState <- as.numeric(catState)
  expect_equal(as.numeric(as.vector(current[,8])),catState)
  
  catCol2 <- as.vector(testdf[,6])
  catCol2 <- replace(catCol2, catCol2=="California_2", 1)
  catCol2 <- replace(catCol2, catCol2=="New York_2", 2)
  catCol2 <- replace(catCol2, catCol2=="Florida_2", 3)
  catCol2 <- as.numeric(catCol2)
  expect_equal(as.numeric(as.vector(current[,9])),catCol2)
  
  catCol3 <- 1:50
  expect_equal(as.numeric(as.vector(current[,10])),catCol3)
})

test_that("Categorize non-numeric custom column names",{
  current <- categorizeNonNumericColumns(testdf,suffix="_category")
  expect_equal(length(current), 10)
  expect_equal(names(current)[8:10], c("State_category","col2_category","col3_category"))
})


test_that("Categorize non-numeric subset columns",{
  current <- categorizeNonNumericColumns(testdf,columns=c("State","col3"))
  expect_equal(length(current), 9)
  expect_equal(names(current)[8:9], c("State_ctgr","col3_ctgr"))
})