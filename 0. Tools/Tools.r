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

returns <- raw.returns[1:50, sample(1:1995, 5)]

# Function to draw efficient frontier
EF <- function(R, risk = "sigma", parallel = FALSE) {
  # Information of return data
  muVector <- colMeans(R)
  sdVector <- apply(R, 2, sd)
  assets <- cbind(sdVector, muVector)
  colnames(assets) <- c("Risk", "Mean Return")
  
  # Efficient frontier points container
  EF <- c()
  
  # Efficient frontier points return value
  returnList <- seq(from = min(muVector), to = max(muVector), length.out = 100)

  P <- cov(returns)
  if(parallel){
    EF <- foreach(i = returnList, .combine = "rbind", .packages = "osqp") %dopar% {
      # QP solution for each return
      x <- solve_osqp(
        P = P, q = rep(0, ncol(R)), A = rbind(muVector, rep(1, ncol(R))),
        l = c(i, 1), u = c(i, 1), pars = osqpSettings(verbose = FALSE))$x
      
      # Store efficient frontier points
      c(sqrt(t(x) %*% cov(R) %*% x), t(x) %*% muVector)
    }
  } else {
    for (i in returnList) {
      # QP solution for each return
      x <- solve_osqp(
        P = P, q = rep(0, ncol(R)), A = rbind(muVector, rep(1, ncol(R))),
        l = c(i, 1), u = c(i, 1), pars = osqpSettings(verbose = FALSE))$x
      
      # Store efficient frontier points
      EF <- rbind(EF, c(sqrt(t(x) %*% cov(R) %*% x), t(x) %*% muVector))
    }
  }

  # Plot efficient frontier
  plot(EF,
    type = "l", xlab = "sigma", ylab = "return",
    ylim = c(min(0, min(muVector)), max(muVector)),
    xlim = c(0, max(sdVector)),
    main = "Efficient Frontier",
    lwd = 1, col = "darksalmon"
  )

  # Add Underlyings points
  lines(assets, col = "honeydew4", type = "p", pch = 19)
  text(assets, rownames(assets), cex=0.6, pos=4, col="honeydew4")

  # Tangent line
  lines(rbind(c(0, 0), c(max(muVector) / max(EF[, 2] / EF[, 1]), max(muVector))),
        type = "l", col = "darkseagreen", lwd = 2)
  grid()
  
  legend("topleft", legend = c("Tangent Line", "Efficient Frontier", "Underlyings"),
         col = c("darkseagreen", "darksalmon", "honeydew4"), lty = 1, bg='ivory')
}

