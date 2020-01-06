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

returns <- raw.returns[, 1:31]

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

# Objective function, weight + loss + VaR
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

# Objective function, weight + loss + VaR + position 
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

# Create a min CVaR optimization with target return ----
mCVaR.t <- mCVaR

# Add target return constraint
mCVaR.t$target <- 0.15/252

# Constraints matrix
mCVaR.t$cM <- rbind(
  c(rep(1, mCVaR.t$tickers), rep(0, mCVaR.t$scenarios + 1)),
  c(mCVaR.t$mu, rep(0, mCVaR.t$scenarios + 1)),
  cbind(
    matrix(returns, mCVaR.t$scenarios), 
    diag(1, mCVaR.t$scenarios), 
    rep(1, mCVaR.t$scenarios)
  )
)

# Optimization result
mCVaR.t$time <- system.time(
  mCVaR.t$result <- Rglpk_solve_LP(
    obj = mCVaR.t$obj, mat = mCVaR.t$cM,
    dir = c("==", rep(">=", mCVaR.t$scenarios + 1)),
    bounds = list(lower = list(ind = 1:mCVaR.t$tickers,
                               val = rep(mCVaR.t$lL, mCVaR.t$tickers)),
                  upper = list(ind = 1:mCVaR.t$tickers,
                               val = rep(mCVaR.t$uL, mCVaR.t$tickers))),
    rhs = c(1.5, mCVaR.t$target, rep(0,mCVaR.t$scenarios)),
    max = TRUE
  ))

# Outputs
mCVaR.t$weight <- mCVaR.t$result$solution[1:mCVaR.t$tickers]
mCVaR.t$CVAR <- mCVaR.t$result$optimum
names(mCVaR.t$weight) <- colnames(returns)
mCVaR.t$VaR <- -mCVaR.t$result$solution[length(mCVaR.t$obj)]

# Create a max ratio of return over CVaR optimization ----
mCVaR.r <- mCVaR

# Objective function, weight + loss + VaR + shrinkage 
mCVaR.r$obj <- c(rep(0, mCVaR.r$tickers), 
                 rep(-1/mCVaR.r$alpha/mCVaR.r$scenarios, mCVaR.r$scenarios), 
                 -1, 0)

# Constraints matrix
mCVaR.r$cM <- rbind(
  c(rep(1, mCVaR.r$tickers), rep(0, mCVaR.r$scenarios + 1), -mCVaR.r$leverage),
  c(mCVaR.r$mu, rep(0, mCVaR.r$scenarios + 1), 0),
  cbind(
    matrix(returns, mCVaR.r$scenarios), 
    diag(1, mCVaR.r$scenarios), 
    rep(1, mCVaR.r$scenarios),
    rep(0, mCVaR.r$scenarios)
  ),
  cbind(
    diag(-1, mCVaR.r$tickers), 
    matrix(0, mCVaR.r$tickers, mCVaR.r$scenarios + 1),
    rep(mCVaR.r$uL, mCVaR.r$tickers)
  ),
  cbind(
    diag(1, mCVaR.r$tickers), 
    matrix(0, mCVaR.r$tickers, mCVaR.r$scenarios + 1),
    rep(-mCVaR.r$lL, mCVaR.r$tickers)
  )
)

# Optimization result
mCVaR.r$time <- system.time(
  mCVaR.r$result <- Rglpk_solve_LP(
    obj = mCVaR.r$obj, mat = mCVaR.r$cM,
    dir = c("==", "==", rep(">=", mCVaR.r$scenarios + 2 * mCVaR.r$tickers)),
    bounds = list(lower = list(ind = 1:mCVaR.r$tickers,
                               val = rep(-Inf, mCVaR.r$tickers)),
                  upper = list(ind = 1:mCVaR.r$tickers,
                               val = rep(Inf, mCVaR.r$tickers))),
    rhs = c(0, 1, rep(0,mCVaR.r$scenarios), rep(0, 2 * mCVaR.r$tickers)),
    max = TRUE
  ))

# Outputs
mCVaR.r$weight <- mCVaR.r$result$solution[1:mCVaR.r$tickers]
mCVaR.r$shrinkage <- mCVaR.r$result$solution[length(mCVaR.r$obj)]
mCVaR.r$weight <- mCVaR.r$weight / mCVaR.r$shrinkage
mCVaR.r$CVAR <- mCVaR.r$result$optimum / mCVaR.r$shrinkage
names(mCVaR.r$weight) <- colnames(returns)
mCVaR.r$VaR <- -mCVaR.r$result$solution[length(mCVaR.r$obj)] / mCVaR.r$shrinkage
mCVaR.r$ratio <- mean(returns %*% mCVaR.r$weight) / mCVaR.r$CVAR