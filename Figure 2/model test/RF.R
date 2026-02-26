# 
library(randomForest)
library(caret)

rm(list = ls())
otu <- read.delim("AW.txt", na.strings = "NA", header = TRUE, fileEncoding = "UTF-16")
otu <- na.omit(otu)  

# 
set.seed(123)

# 
train_index <- createDataPartition(otu$amewig, p = 0.8, list = FALSE)
train_data <- otu[train_index, ]
test_data <- otu[-train_index, ]
train_mean <- mean(train_data$amewig)
print(train_mean)
#
otu_forest <- randomForest(amewig ~ ., data = train_data, importance = TRUE, ntree = 500)

#
pred <- predict(otu_forest, newdata = test_data)

#
mse <- mean((pred - test_data$amewig)^2)

#）
ss_total <- sum((test_data$amewig - mean(test_data$amewig))^2)
ss_res <- sum((test_data$amewig - pred)^2)
r_squared <- 1 - ss_res / ss_total
residuals <- test_data$amewig - pred
residual_sd <- sd(residuals)
#
cat("MSE:", round(mse, 4), "\n")
cat("R²:", round(r_squared, 4), "\n")
cat("Residual SD:", round(residual_sd, 4), "\n")
