library(xts)
library(zoo)
library(Rglpk)

prices <- read.csv.zoo(file = "~/GitHub/Optimization/Data/SP500.csv")
returns <- diff(log(xts(prices)))["2019"]
returns <- returns[,which(apply(returns, 2, function(x){sum(is.na(x)) == 0}))]

n <- ncol(returns)
s <- nrow(returns)

mean <- colMeans(returns)
Amat <- cbind(
  rbind(
    rep(1, n), mean, returns
  ),
  rbind(
    rep(0, n), rep(0, n), cbind(diag(1, s, s), rep(1, s))
  )
)