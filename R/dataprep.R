library(graphics)
library(stats)

#' Data Quantiles
#' 
#' @description Retrieves the 0 up to 100 quantiles that split the vector \code{data} in \code{n} groups.
#'
#' @param data a data vector
#' @param n the number of groups to divide the data vector
#' @param na.rm logical; if true, any NA
#' and \code{NaN}'s are removed from \code{data} before the quantiles are computed.
#'
#' @return a named numeric vector value for 0%, 1/n%,...,n/n%
#' @export
#' @importFrom stats quantile
#' 
#' @examples
#' dataQuantiles(rep(1:10,10), 1)
#' dataQuantiles(rep(1:10,10), 3)
#' dataQuantiles(rep(seq(from=0,to=10,by=0.1),10), 4)
dataQuantiles <- function(data, n=4, na.rm=TRUE) {
  quantile(data, (0:n)/n, na.rm = na.rm)
}

#' Data Histogram by Quantiles
#'  
#' @description Creates a histogram of \code{name} with the given \code{data}, splitting it in \code{n} groups
#' based on its quantiles.
#'
#' @param name a name for the data. default="data"
#' @param data a data vector
#' @param n the number of groups to divide the data vector
#' @param na.rm logical; if true, any NA
#' and \code{NaN}'s are removed from \code{data} before the quantiles are computed.
#' @param plot logical; if true, the histogram is plotted
#' 
#' @return the histogram
#' @export
#' @importFrom graphics hist
#' 
#' @examples
#' histQuantiles("Grades", rep(1:10,10), 1)
#' histQuantiles("Grades", rep(1:10,10), 3)
#' histQuantiles("Grades", rep(seq(from=0,to=10,by=0.1),10), 4)
#'
histQuantiles <- function(name="data", data, n=4, na.rm=TRUE, plot=TRUE) {
  dq <- dataQuantiles(data, n, na.rm)
  hist(data, breaks=dq, freq=FALSE,
       main=paste0("Histogram of ", name), xlab=name,
       warn.unused=FALSE,
       plot=plot)
}

#' Discretize Data by Quantiles
#' 
#' @description Discretizes the given \code{data}, splitting it in \code{n} groups
#' based on its quantiles.
#'
#' @param data a data vector
#' @param n the number of groups to divide the data vector
#' @param na.rm logical; if true, any NA
#' and \code{NaN}'s are removed from \code{data} before the quantiles are computed.
#'
#' @return a vector where the numerical data are replaced by its equivalent groups
#' @export
#'
#' @examples
#' discretizeByQuantiles(rep(1:10,10))
#' discretizeByQuantiles(rep(1:10,10), 1)
#' discretizeByQuantiles(rep(1:10,10), 3)
#' discretizeByQuantiles(rep(seq(from=0,to=10,by=0.1),10))
#' discretizeByQuantiles(rep(seq(from=0,to=10,by=0.1),10), 5)
discretizeByQuantiles <- function(data, n=4, na.rm=TRUE) {
  dataQ <- dataQuantiles(data, n, na.rm=na.rm)
  discData <- cut(data, dataQ, labels=(1:n), include.lowest=TRUE, na.rm=na.rm)
  as.integer(discData)
}

#' Numeric Columns
#' 
#' @description  
#' Retrieves a vector with the names of the numeric columns of a dataframe \code{df}
#'
#' @param df the dataframe
#'
#' @return a vector
#' @export
#'
#' @examples 
#' getNumericColumns(startupssmall)
getNumericColumns <- function(df) {
  filtercol <- function (i) {
    is.numeric(df[,i])
  }
  filtercol <- Vectorize(filtercol)
  
  result <- names(df)
  result[filtercol(result)]
}

#' Non-numeric Columns
#' 
#' @description 
#' Retrieves a vector with the names of the non-numeric columns of a dataframe \code{df}
#'
#' @param df the dataframe
#'
#' @return a vector
#' @export
#'
#' @examples 
#' getNonNumericColumns(startupssmall)
getNonNumericColumns <- function(df) {
  cols <- colnames(df)
  numericCols <- getNumericColumns(df)
  cols[!(cols %in% numericCols)]
}

#' Discretize Numeric Columns by Quantile
#' 
#' @description 
#' Discretizes the given data frame \code{df}, splitting its columns \code{columns[0..i]} in \code{ns[0..i]} groups.
#' The new columns will have the same name of the original columns including the \code{suffix}, and will be appeded in the end of the \code{df}.
#' Processed the data \code{startupsbig} in ~1min and \code{startupshuge} in ~10min. 
#' 
#' @param df a data frame
#' @param columns a vector with the column names to be discretized. If \code{is.null(columns)},
#' every numeric column of \code{df} will be discretized
#' @param ns a vector containing the number of groups to discretize each column specified by \code{columns}. If
#' the length of \code{ns} is different from the length of \code{columns} or \code{is.null(ns)}, then it will be 
#' used a vector of 4s with the same size of \code{columns}
#' @param suffix the suffix to be appeded to the new column names
#'
#' @return a data frame with the original and the discretized
#' @export
#'
#' @examples
#' discretizeColumnsByQuantiles(startupssmall)
#' discretizeColumnsByQuantiles(startupssmall, c("R.D.Spend","Profit"), c(3,5))
discretizeColumnsByQuantiles <- function(df, columns=NULL, ns=NULL, suffix="_dscr") {
  if(is.null(columns)) {
    columns <- getNumericColumns(df)
  }
  
  if (is.null(ns) | length(ns)!=length(columns)) {
    ns <- rep(4,length(columns))
  }
  
  dscrfun <- function(name,n) {
    discretizeByQuantiles(df[,name], n)
  }
  x <- mapply(dscrfun, columns, ns)
  colnames(x) <- paste0(columns,suffix)
  cbind(df,x)
}

#' Categorize Non-numeric Columns
#' 
#' @description 
#' Categorizes the non-numeric columns of the given data frame \code{df}.
#' The non-numeric columns will be converted to factor columns with numeric labels.
#' This is useful to machine learning algorithms that use numbers instead of texts.
#'
#' @param df a data frame
#' @param columns a vector with the column names to be categorized.
#' @param suffix the suffix to be appeded to the new column names
#'
#' @return a data frame with the original and the discretized
#' @export
#'
#' @examples
#' categorizeNonNumericColumns(startupssmall)
#' categorizeNonNumericColumns(startupssmall, c("State"))
#' categorizeNonNumericColumns(startupssmall, c("State"), suffix="_categorized")
categorizeNonNumericColumns <- function(df, columns=NULL, suffix="_ctgr") {
  if(is.null(columns)) {
    columns <- getNonNumericColumns(df)
  }

  factorize <- function (colName) {
    newLevels <- unique(df[,colName])
    newLabels <- 1:length(newLevels)
    factor(df[,colName], levels = newLevels, labels = newLabels)
  }
  ctgrCols <- sapply(columns,factorize)
  colnames(ctgrCols) <- paste0(columns,suffix)
  cbind(df,ctgrCols)
}

#' Process \code{NaN}s 
#' @description 
#' Process the \code{NaN}s of the data frame, by removing rows or replacing them by mean, median or mode.
#'
#' @param df a data frame
#' @param naBehavior a vector of integers; depicts what to do whenever a \code{NaN} is found in a numerical column. 0=remove line, 1=mean, 2=median, 3=mode.
#' @param columns a vector with the columns names
#'
#' @return a data frame with the processed \code{NaN}s
#' @export
#'
#' @examples
#' preprocessNaN(startupssmall, 0)
#' preprocessNaN(startupssmall, 2, c("R.D.Spend","Profit"))
preprocessNaN = function(df, naBehavior, columns = NULL) {
  if (naBehavior==0) {
    na.omit(df)
  } else {
    if(is.null(columns)) {
      columns <- getNumericColumns(df)
    }

    avgValues <- NULL
    if (naBehavior==1) {
      avgValues <- sapply(df[,columns], mean, na.rm = TRUE)
    } else if (naBehavior==2) {
      avgValues <- sapply(df[,columns], median, na.rm = TRUE)
    } else if (naBehavior==3) {
      calcMode <- function(x) {
        uniqueX <- unique(x)
        firstMatches <- match(x, uniqueX)
        firstMatchFrequency <- tabulate(firstMatches)
        maxFrequencyIndex <- which.max(firstMatchFrequency)
        uniqueX[maxFrequencyIndex]
      }

      avgValues <- sapply(df[,columns], calcMode)
    } else {
      stop(paste0("Invalid naBehavior parameter value. Expected 0, 1, 2 or 3, but found ", naBehavior, "."))
    }

    colNames <- names(avgValues)
    for (colName in colNames) {
      avgValue <- avgValues[colName]
      colValues <- df[,colName]
      df[,colName] <- ifelse(is.na(colValues), avgValue, colValues)
    }
    df
  }
}

#' Data Preprocessing
#' 
#' @description 
#' Preprocess the data of a csv file and generates a new file with the original
#' and the preprocessed data.
#'
#' @param csv a csv file path.
#' @param csvSuffix The suffix that will be included after the name of the new csv.
#' @param numericColumns a vector with the numeric column names to be discretized
#' @param ns a vector containing the number of groups to discretize each column specified by \code{columns}
#' @param naBehavior integer; depicts what to do whenever a \code{NaN} is found in a numerical column. 0=remove line, 1=mean, 2=median, 3=mode.
#' @param numericColumnsSuffix the suffix to be appended to new numeric columns names
#' @param nonNumericColumns a vector with the non-numeric column names to be categorized
#' @param nonNumericColumnsSuffix the suffix to be appended to new non-numeric columns names
#' @param rmNonNumericColumns boolean; if \code{TRUE}
#'
#' @return the saved file
#' @export
#' @importFrom utils read.csv write.csv
#'
#' @examples
#' \dontrun{
#' preprocess("data.csv")
#' preprocess(csv="data.csv", numericColumns=c("A", "B"), ns=c(3, 5))
#' preprocess(csv="data.csv", naBehavior=0, numericColumns=c("A", "B"), ns=c(3, 5))
#' }
preprocess <- function(csv, csvSuffix="_preprocessed",
                       numericColumns = NULL, ns = 4, naBehavior = 2, numericColumnsSuffix="_dscr",
                       nonNumericColumns = NULL, nonNumericColumnsSuffix="_ctgr", rmNonNumericColumns = TRUE) {
  df <- read.csv(csv)

  #removes or replace NaN's by mean, median or mode
  df <- preprocessNaN(df = df, naBehavior = naBehavior)

  #discretize numerical data
  df <- discretizeColumnsByQuantiles(df = df, columns = numericColumns, ns = ns,
                                     suffix = numericColumnsSuffix)
  #categorize non-numerical data
  df <- categorizeNonNumericColumns(df = df, columns = nonNumericColumns,
                                     suffix = nonNumericColumnsSuffix)

  #writing data to file
  newCSV <- substr(csv, 1, nchar(csv)-4)
  newCSV <- paste0(newCSV, csvSuffix, ".csv")
  write.csv(df, file = newCSV, row.names = FALSE)
}