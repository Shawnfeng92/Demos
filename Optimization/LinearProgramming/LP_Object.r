library(xts)
library(zoo)
library(Rglpk)

# Load price data as time series type
prices <- xts(read.csv.zoo(file = "~/GitHub/Demos/Data/DataStorage/AMEX.csv", 
                   FUN = as.Date))

# Daily log return data
returns <- diff(log(prices))

# Process optimization on stocks with full 2019 performance
returns <- returns["2019"]
returns <- returns[rowSums(is.na(returns)) < ncol(returns) - 5, ]
returns <- returns[ , colSums(is.na(returns)) == 0]

# Create a min CVaR optimization
mVaR <- list()
mVaR$risk <- "CVaR"

# Summary parameter
mVaR$scenarios <- nrow(returns)
mVaR$tickers <- ncol(returns)
mVaR$mu <- apply(returns, 2, mean)

# Constraints
mVaR$leverage <- 1.5
mVaR$pL <- 15    # Position limitation
mVaR$uL <- 0.15  # Upper limitation for box constrain
mVaR$lL <- -0.15 # Lower limitation

# Objective function, loss + weight + VaR + position
mVaR$obj <- c(rep(1, mVaR$tickers), rep(0, mVaR$scenarios), 0, rep(0, mVaR$tickers))

# Constraints matrix
mVaR$cM <- rbind(
  c(rep(1, mVaR$tickers), rep(0, mVaR$scenarios), 0, rep(0, mVaR$tickers)),
  c(rep(0, mVaR$tickers), rep(0, mVaR$scenarios), 0, rep(1, mVaR$tickers)),

  cbind(
    matrix(returns, mVaR$scenarios), diag(1, mVaR$scenarios), rep(1, mVaR$scenarios), matrix(0, mVaR$scenarios, mVaR$tickers)
  )
)

# Optimization result
mVaR$result <- Rglpk_solve_LP(obj = mVaR$obj, mat = mVaR$cM, 
                              dir = c("<=", "<=", rep(">=", mVaR$scenarios)),
                              bounds = list(lower = list(ind = 1:length(mVaR$obj), 
                                                         val = c(rep(mVaR$lL, mVaR$tickers),
                                                                 rep(- Inf, mVaR$scenarios+1),
                                                                 rep(0, mVaR$tickers))),
                                            upper = list(ind = 1:length(mVaR$obj), 
                                                         val = c(rep(mVaR$uL, mVaR$tickers),
                                                                 rep(Inf, mVaR$scenarios+1),
                                                                 rep(0, mVaR$tickers)))),
                              rhs = c(1.5, 15, rep(0,mVaR$scenarios)),
                              types = c(rep("C", mVaR$scenarios + mVaR$tickers + 1), rep("B", mVaR$tickers)),
                              max = TRUE
)