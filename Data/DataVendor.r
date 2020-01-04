# This script will download and save daily adjusted historical price for all stocks in ticker list
# from 01/03/2017 to today.

library(BatchGetSymbols)
library(quantmod)
library(doParallel)

DataVendor <- function(tickers = GetSP500Stocks()$Tickers, save_location = "SP500.csv", parallel = TRUE) {
  if(parallel) {
    data <- foreach(i = tickers, .combine = "cbind", .packages = "quantmod") %dopar% {
      temp <- try(getSymbols(Symbols = i, 
                             auto.assign = FALSE)[,6])
      if(class(temp) != "try-error") {
        temp
      }
      else {
        NA
      }
    }
    colnames(data) <- tickers
    
    write.zoo(x = data, file = save_location, sep = ",")
  }
}

cl <- makeCluster(16)
registerDoParallel(cl)
DataVendor()
DataVendor(read.delim(file = "AMEX.txt", stringsAsFactors = FALSE)[,1], "AMEX.csv")
DataVendor(read.delim(file = "NASDAQ.txt", stringsAsFactors = FALSE)[,1], "NASDAQ.csv")
DataVendor(read.delim(file = "NYSE.txt", stringsAsFactors = FALSE)[,1], "NYSE.csv")
stopCluster(cl)