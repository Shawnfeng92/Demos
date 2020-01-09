#
# Author: Xiaokang Feng
# GitHub: https://github.com/Shawnfeng92
#
# Final version. Object-orient.
#

library(xts)
library(zoo)
setOldClass("zoo")

portfolio.data <- setClass(
  "portfolio.data",
  slots = list(
    tickers = "character",
    nticker = "integer",
    scenario = "Date",
    nscenrio = "integer",
    returns = "zoo"
  ),
  prototype = list(
    tickers = NA_character_,
    nticker = NA_integer_,
    scenario = NA_real_,
    nscenrio = NA_integer_,
    returns = NA_real_
  )
)

portfolio.utility <- setClass(
  "portfolio.utility",
  slots = list(
    risk = "character",
    return = "character"
  )
)


portfolio.const <- setClass(
  "portfolio.const",
  slots = list(
    type = "character",
    leverage = "numeric",
    box_const = "list",
    group_const = "list",
    turnover = "numeric",
    position_limitation = "integer",
    target_return = "numeric",
    target_risk = "numeric",
    diversity = "numeric"
  )
)

portfolio <- setClass(
  "portfolio",
  slots = list(
    name = "character",
    data =  "portfolio.data",
    utility = "portfolio.utility",
    const = "portfolio.const"
  )
)

