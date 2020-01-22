data <- as.matrix(read.csv(
  file = "C:/Users/Shawn/Desktop/DeathRatePrediction.csv", 
  sep = ",", 
  stringsAsFactors = FALSE
  ))

train <- cbind(1, data[1:50, 2:17])
test <- cbind(1, data[51:60, 2:17])


rm(data)

beta <- solve(t(train[1:50, 1:16]) %*% train[1:50, 1:16]) %*% t(train[1:50, 1:16]) %*% train[, 17]

test[, 1:16] %*% beta - test[,17]
