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

returns <- raw.returns[, 1:30]

# Create a min CVaR optimization with simple leverage and box constraints ----
mCVaR <- list()
mCVaR$risk <- "CVaR"

# Summary parameter
mCVaR$scenarios <- nrow(returns)
mCVaR$tickers <- ncol(returns)
mCVaR$mu <- colMeans(returns)
mCVaR$alpha <- 0.05

# Constraints
mCVaR$leverage <- 1.5
mCVaR$uL <- 1       # Upper limitation for box constrain
mCVaR$lL <- - 1      # Lower limitation

# Objective function, loss + weight + VaR
mCVaR$obj <- c(rep(0, mCVaR$tickers), 
              rep(-1/mCVaR$alpha/mCVaR$scenarios, mCVaR$scenarios), 
              -1)

# Constraints matrix
mCVaR$cM <- rbind(
  c(rep(1, mCVaR$tickers), rep(0, mCVaR$scenarios + 1)),
  cbind(
    matrix(returns, mCVaR$scenarios), 
    diag(1, mCVaR$scenarios), 
    rep(1, mCVaR$scenarios)
  )
)

# Optimization result
mCVaR$time <- system.time(
  mCVaR$result <- Rglpk_solve_LP(
    obj = mCVaR$obj, mat = mCVaR$cM,
    dir = c("==", rep(">=", mCVaR$scenarios)),
    bounds = list(lower = list(ind = 1:mCVaR$tickers,
                               val = rep(mCVaR$lL, mCVaR$tickers)),
                  upper = list(ind = 1:mCVaR$tickers,
                               val = rep(mCVaR$uL, mCVaR$tickers))),
    rhs = c(1.5, rep(0,mCVaR$scenarios)),
    max = TRUE
  ))

# Outputs
mCVaR$weight <- mCVaR$result$solution[1:mCVaR$tickers]
mCVaR$CVAR <- mCVaR$result$optimum
names(mCVaR$weight) <- colnames(returns)
mCVaR$VaR <- -mCVaR$result$solution[length(mCVaR$obj)]

# Create a min CVaR optimization with postion limitation ----
mCVaR.p <- mCVaR

# Add position limitation
mCVaR.p$pL <- 15
mCVaR.p$obj <- c(rep(0, mCVaR.p$tickers), 
               rep(-1/mCVaR.p$alpha/mCVaR.p$scenarios, mCVaR.p$scenarios), 
               -1, rep(0, mCVaR.p$tickers))

# Constraints matrix
mCVaR.p$cM <- rbind(
  c(rep(1, mCVaR.p$tickers), rep(0, mCVaR.p$scenarios + 1 + mCVaR.p$tickers)),
  c(rep(0, mCVaR.p$tickers + 1 + mCVaR.p$scenarios), rep(1, mCVaR.p$tickers)),
  cbind(
    matrix(returns, mCVaR.p$scenarios), 
    diag(1, mCVaR.p$scenarios), 
    rep(1, mCVaR.p$scenarios),
    matrix(0, mCVaR.p$scenarios, mCVaR.p$tickers)
  ),
  cbind(diag(1, mCVaR.p$tickers), 
        matrix(0, mCVaR.p$tickers, mCVaR.p$scenarios+1), 
        diag(-mCVaR.p$lL, mCVaR.p$tickers)),
  cbind(diag(-1, mCVaR.p$tickers), 
        matrix(0, mCVaR.p$tickers, mCVaR.p$scenarios+1), 
        diag(mCVaR.p$uL, mCVaR.p$tickers))
)

# Optimization result
mCVaR.p$time <- system.time(
  mCVaR.p$result <- Rglpk_solve_LP(
    obj = mCVaR.p$obj, mat = mCVaR.p$cM, 
    dir = c("==", "<=", rep(">=", mCVaR.p$scenarios + 2 * mCVaR.p$tickers)),
    bounds = list(lower = list(ind = 1:mCVaR.p$tickers, 
                               val = rep(mCVaR.p$lL, mCVaR.p$tickers)),
                  upper = list(ind = 1:mCVaR.p$tickers, 
                               val = rep(mCVaR.p$uL, mCVaR.p$tickers))),
    rhs = c(1.5, mCVaR.p$pL, rep(0,mCVaR.p$scenarios + 2 * mCVaR.p$tickers)),
    types = c(rep("C", mCVaR.p$tickers + mCVaR.p$scenarios + 1), 
              rep("B", mCVaR.p$tickers)),
    max = TRUE
))

# Outputs
mCVaR.p$weight <- mCVaR.p$result$solution[1:mCVaR.p$tickers]
mCVaR.p$CVAR <- mCVaR.p$result$optimum
names(mCVaR.p$weight) <- colnames(returns)
mCVaR.p$VaR <- -mCVaR.p$result$solution[length(mCVaR.p$obj)]