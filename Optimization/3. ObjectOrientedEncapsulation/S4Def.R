#
# Author: Xiaokang Feng
# GitHub: https://github.com/Shawnfeng92
#
# Final version. Object-orient.
#

library(xts)
library(zoo)
setOldClass("zoo")

PortfolioData <- setClass(
  "PortfolioData",
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

PortfolioUtility <- setClass(
  "PortfolioUtility",
  slots = list(
    risk = "character",
    return = "character"
  ),
  prototype = list(
    risk = NA_character_,
    return = NA_character_
  )
)

PortfolioConst <- setClass(
  "PortfolioConst",
  slots = list(
    type = "character",
    leverage = "numeric",
    box_const = "numeric",
    group_const = "numeric",
    turnover = "numeric",
    position_limitation = "integer",
    target_return = "numeric",
    target_risk = "numeric",
    diversity = "numeric"
  ),
  prototype = list(
    type = NA_character_,
    leverage = 1,
    box_const = NA_real_,
    group_const = NA_real_,
    turnover = NA_real_,
    position_limitation = NA_integer_,
    target_return = NA_real_,
    target_risk = NA_real_,
    diversity = NA_real_
  )
)

PortfolioResult <- setClass(
  "PortfolioResult",
  slots = list(
    time = "numeric",
    weights = "numeric",
    risk = "character",
    return = "numeric",
    target = "numeric"
  ),
  prototype = list(
    time = NA_real_,
    weights = NA_real_,
    risk = NA_character_,
    return = NA_real_,
    target = NA_real_
  )
)

Portfolio <- setClass(
  "Portfolio",
  slots = list(
    name = "character",
    data =  "PortfolioData",
    utility = "PortfolioUtility",
    const = "PortfolioConst",
    result = "PortfolioResult"
  ),
  prototype = list(
    name = NA_character_,
    data = NA,
    utility = NA,
    const = NA,
    result = NA
  )
)

