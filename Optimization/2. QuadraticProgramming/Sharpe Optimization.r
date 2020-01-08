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
mVol$Volatility <- t(mVol$weights) %*% mVol$cov %*% mVol$weights
mVol$r <- t(mVol$weights) %*% mVol$mu

# Create a min Sharpe Ratio optimization ----
mVol.r <- mVol

# Create P matrix
mVol.r$P <- rbind(cbind(mVol.r$cov, rep(0, mVol.r$tickers)), rep(0, mVol.r$tickers + 1))

# Create q vector
mVol.r$q <- rep(0, mVol.r$tickers + 1)

# Optimization result
mVol.r$time <- system.time(
  mVol.r$result <- solve_osqp(
    P = mVol.r$P,
    q = mVol.r$q,
    A = rbind(
      c(rep(1, mVol.r$tickers), -mVol.r$l),
      c(mVol.r$mu, 0),
      cbind(diag(-1, mVol.r$tickers), rep(mVol.r$uL, mVol.r$tickers)),
      cbind(diag(1, mVol.r$tickers), rep(-mVol.r$lL, mVol.r$tickers))
    ),
    l = c(0, 1, rep(0, 2 * mVol.r$tickers)),
    u = c(0, 1, rep(Inf, 2 * mVol.r$tickers)),
    pars = osqpSettings(verbose = FALSE)
  )
)

# Output
mVol.r$shrinkage <- mVol.r$result$x[mVol.r$tickers + 1]
mVol.r$weights <- mVol.r$result$x[1:mVol.r$tickers] / mVol.r$shrinkage
names(mVol.r$weights) <- colnames(returns)
mVol.r$Volatility <- sqrt(t(mVol.r$weights) %*% mVol.r$cov %*% mVol.r$weights)
mVol.r$r <- t(mVol.r$weights) %*% mVol.r$mu
mVol.r$Sharpe <- mVol.r$r / sqrt(mVol.r$Volatility)
