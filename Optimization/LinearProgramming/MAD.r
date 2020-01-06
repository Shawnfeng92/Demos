library(xts)
library(zoo)
library(Rglpk)

# Load price data as time series type
# prices <- xts(read.csv.zoo(file = "~/GitHub/Demos/Data/DataStorage/AMEX.csv",
#                            FUN = as.Date))

# Daily log return data
# raw.returns <- diff(log(prices))

# Process optimization on stocks with full 2019 performance
# raw.returns <- raw.returns["2019"]
# raw.returns <- raw.returns[rowSums(is.na(raw.returns)) < ncol(raw.returns) - 5, ]
# raw.returns <- raw.returns[ , colSums(is.na(raw.returns)) == 0]

returns <- raw.returns[, 1:31]

# Create a min MAD optimization with simple leverage and box constraints ----
mMad <- list()
mMad$risk <- "MAD"

# Summary parameter
mMad$scenarios <- nrow(returns)
mMad$tickers <- ncol(returns)
mMad$mu <- colMeans(returns)

# Constraints
mMad$leverage <- 1.5
mMad$uL <- 1       # Upper limitation for box constrain
mMad$lL <- - 1      # Lower limitation

# Objective function, weight + absolute distance
mMad$obj <- c(rep(0, mMad$tickers), 
               rep(1, mMad$scenarios))

# Constraint Matrix
mMad$cM <- rbind(
  c(rep(1, mMad$tickers), rep(0, mMad$scenarios)),
  cbind(
    matrix(
      returns - matrix(rep(mMad$mu, mMad$scenarios), mMad$scenarios), mMad$scenarios
      ), diag(1, mMad$scenarios)
  ),
  cbind(
    matrix(
      - returns + matrix(rep(mMad$mu, mMad$scenarios), mMad$scenarios), mMad$scenarios
      ), diag(1, mMad$scenarios)
  )
)


# Optimization result
mMad$time <- system.time(
  mMad$result <- Rglpk_solve_LP(
    obj = mMad$obj, mat = mMad$cM,
    dir = c("==", rep(">=", 2 * mMad$scenarios)),
    bounds = list(lower = list(ind = 1:mMad$tickers,
                               val = rep(mMad$lL, mMad$tickers)),
                  upper = list(ind = 1:mMad$tickers,
                               val = rep(mMad$uL, mMad$tickers))),
    rhs = c(mMad$leverage, rep(0, 2 * mMad$scenarios)),
    max = FALSE
  ))

# Outputs
mMad$weight <- mMad$result$solution[1:mMad$tickers]
mMad$MAD <- mMad$result$optimum
names(mMad$weight) <- colnames(returns)

