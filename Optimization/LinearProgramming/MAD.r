library(xts)
library(zoo)
library(Rglpk)

# Load price data as time series type
prices <- xts(read.csv.zoo(file = "~/GitHub/Demos/Data/DataStorage/AMEX.csv",
                           FUN = as.Date))

# Daily log return data
raw.returns <- diff(log(prices))

# Process optimization on stocks with full 2019 performance
raw.returns <- raw.returns["2019"]
raw.returns <- raw.returns[rowSums(is.na(raw.returns)) < ncol(raw.returns) - 5, ]
raw.returns <- raw.returns[ , colSums(is.na(raw.returns)) == 0]

# Create a min MAD optimization with simple leverage and box constraints ----
mMad <- list()
mMad$risk <- "MAD"

# Summary parameter
mMad$scenarios <- nrow(returns)
mMad$tickers <- ncol(returns)
mMad$mu <- colMeans(returns)
mMad$alpha <- 0.05

# Constraints
mMad$leverage <- 1.5
mMad$uL <- 1       # Upper limitation for box constrain
mMad$lL <- - 1      # Lower limitation

