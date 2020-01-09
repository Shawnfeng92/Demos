#
# Author: Xiaokang Feng
#

library(xts)
library(zoo)
library(osqp)
library(Rglpk)
library(foreach)
library(doParallel)

# Load price data as time series type
# prices <- xts(read.csv.zoo(
#   file = "~/GitHub/Demos/1. Data/DataStorage/AMEX.csv",
#   FUN = as.Date
# ))

# Daily log return data
# raw.returns <- diff(log(prices))

# Process optimization on stocks with full 2019 performance
# raw.returns <- raw.returns["2019"]
# raw.returns <- raw.returns[rowSums(is.na(raw.returns)) < ncol(raw.returns) - 5, ]
# raw.returns <- raw.returns[, colSums(is.na(raw.returns)) == 0]

returns <- raw.returns[, sample(1:1995, 15)]

# Function to draw efficient frontier
qpEF <- function(R) {
  muList <- colMeans(R)
  sdList <- apply(R, 2, sd)
  assets <- cbind(sdList, muList)
  colnames(assets) <- c("Sigma", "Mean Return")

  returnList <- seq(
    from = min(muList),
    to = max(muList),
    length.out = 100
  )

  EF <- c()
  A <- cov(returns)
  cl <- parallel::makeCluster(16)
  doParallel::registerDoParallel(cl)
  EF <- foreach(i = returnList, .combine = "rbind", .packages = "osqp") %dopar% {
    x <- solve_osqp(
      P = A, q = rep(0, ncol(R)), A = rbind(muList, rep(1, ncol(R))),
      l = c(i, 1), u = c(i, 1), pars = osqpSettings(verbose = FALSE)
    )$x
    c(sqrt(t(x) %*% cov(R) %*% x), t(x) %*% muList)
  }
  stopCluster(cl)

  plot(EF,
    type = "l", xlab = "sigma", ylab = "return",
    ylim = c(min(muList), max(muList)),
    xlim = c(0, max(sdList))
  )
  points(assets, col = "red")
  points(rbind(c(0, 0), c(max(muList) / max(EF[, 2] / EF[, 1]), max(muList))), type = "l", col = "green")
}

qpEF(returns)
