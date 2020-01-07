library(xts)
library(zoo)
library(osqp)

# Load price data as time series type
prices <- xts(read.csv.zoo(
  file = "~/GitHub/Demos/Data/DataStorage/AMEX.csv",
  FUN = as.Date
))

# Daily log return data
raw.returns <- diff(log(prices))

# Process optimization on stocks with full 2019 performance
raw.returns <- raw.returns["2019"]
raw.returns <- raw.returns[rowSums(is.na(raw.returns)) < ncol(raw.returns) - 5, ]
raw.returns <- raw.returns[, colSums(is.na(raw.returns)) == 0]

returns <- raw.returns

# Create a min volatility optimization ----
mVol <- list()

# Create stat information
mVol$risk <- "Sigma"
mVol$tickers <- ncol(returns)
mVol$scenerio <- nrow(returns)
mVol$mu <- colMeans(returns)
mVol$cov <- cov(returns)

# Create leverage and box constraints
mVol$l <- 1.5
mVol$uL <- 0.15
mVol$lL <- -0.15

# Create P matrix
mVol$P <- mVol$cov

# Create q vector
mVol$q <- rep(0, mVol$tickers)

# Optimization result
mVol$time <- system.time(
  mVol$result <- solve_osqp(
    P = mVol$P,
    q = mVol$q,
    A = rbind(rep(1, mVol$tickers), diag(1, mVol$tickers)),
    l = c(mVol$l, rep(mVol$lL, mVol$tickers)),
    u = c(mVol$l, rep(mVol$uL, mVol$tickers)),
    pars = osqpSettings(verbose = FALSE)
  )
)

# Output
mVol$weights <- mVol$result$x
names(mVol$weights) <- colnames(returns)