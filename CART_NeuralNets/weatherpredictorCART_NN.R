weather.df <- read.csv("weatherAUSv2.csv")

#convert date to a date format
weather.df$Date <- as.Date(weather.df$Date , "%m/%d/%Y")
library(lubridate)
weather.df$Month <- as.factor(month(weather.df$Date))

#drop Date and Location variables
weather.df <- weather.df[, -c(1:2)]

#partition the data
train.rows <- sample(rownames(weather.df), nrow(weather.df)*0.6)
train.data <- weather.df[train.rows, ]
valid.rows <- setdiff(rownames(weather.df), train.rows)
valid.data <- weather.df[valid.rows, ]

library(rpart)
library(rpart.plot)
#create full classification tree
class.tree1 <- rpart(RainTomorrow ~ ., data = train.data, method = "class", cp = 0, minsplit = 1)
# plot tree
prp(class.tree1, type = 1, extra = 1, varlen = -10,
    box.col = ifelse(class.tree1$frame$var == "<leaf>", 'gray', 'white'))

library(caret)
# classify records in the validation data
class.tree1.pred.valid <- predict(class.tree1, valid.data, type = "class")
confusionMatrix(class.tree1.pred.valid, 
                as.factor(valid.data$RainTomorrow), 
                positive = "Yes")

#relationship between cp and size of tree
plotcp(class.tree1)

# cross-validation procedure
# argument cp sets the smallest value for the complexity parameter
cv.ct <- rpart(RainTomorrow ~ ., data = train.data, method = "class",
               cp = 0.00001, minsplit = 5, xval = 10)
printcp(cv.ct)
# prune by lower cp
pruned.ct <- prune(cv.ct, cp = 0.0102041)
prp(pruned.ct, type = 1, extra = 1, varlen = -10,
    box.col = ifelse(pruned.ct$frame$var == "<leaf>", 'gray', 'white'))

# confusion matrix for pruned tree on validation data
pruned.ct.pred.valid <- predict(pruned.ct, valid.data, type = "class")
confusionMatrix(pruned.ct.pred.valid, 
                as.factor(valid.data$RainTomorrow), 
                positive = "Yes")

t(t(names(weather.df)))
# New record for predictor values
weather.new <- weather.df[1, -21] # just drop the outcome variable
cols.new <- colnames(weather.new)
# set all variables to 0
for (i in cols.new) {
  weather.new[[i]] <- 0
}
# new record classification values
weather.new$Month <- 1
weather.new$MinTemp <- 21.4
weather.new$MaxTemp <- 37.5
weather.new$Rainfall <- 0
weather.new$Evaporation <- 14.8
weather.new$Sunshine <- 6.9
weather.new$WindGustDir <- "NNE"
weather.new$WindGustSpeed <- 43
weather.new$WindDir9am <- "ENE"
weather.new$WindDir3pm <- "NNE"
weather.new$WindSpeed9am <- 26
weather.new$WindSpeed3pm <- 9
weather.new$Humidity9am <- 34
weather.new$Humidity3pm <- 29
weather.new$Pressure9am <- 1013.1
weather.new$Pressure3pm <- 1009.6
weather.new$Cloud9am <- 7
weather.new$Cloud3pm <- 6
weather.new$Temp9am <- 26.2
weather.new$Temp3pm <- 34.1
weather.new$RainToday <- "No"


# random forest
# install.packages("randomForest")
library(randomForest)
rf <- randomForest(as.factor(RainTomorrow) ~ ., data = train.data, ntree = 500,
                   mtry = 4, nodesize = 5, importance = TRUE)

# variable importance plot
varImpPlot(rf, type = 1)
#confusion matrix
rf.pred <- predict(rf, valid.data)
confusionMatrix(rf.pred, as.factor(valid.data$RainTomorrow), positive = "Yes")

library(neuralnet)
library(nnet)

summary(weather.df)

#log transformation of highly skewed predictors
library(e1071)
num.cols <- colnames(weather.df[, c(1:19, 22)])
for (i in num.cols) {
  boxplot(weather.df[[i]],
          ylab = i)
  print(skewness(weather.df[[i]]))
}

weather.df$Rainfall <- log(weather.df$Rainfall + 1)

#converting categorical variables to binary dummy variables
dummies <- as.data.frame(model.matrix(~ 0 + RainToday + Month + WindGustDir + WindDir9am + WindDir3pm, data = weather.df))
t(t(names(dummies)))
t(t(names(weather.df)))

weather.df <- cbind(weather.df[, -c( 20, 6, 8, 9, 22)], dummies[ , -c(1, 21, 35, 51)])
t(t(names(weather.df)))
summary(weather.df)

#standardizing data 0-1
norm.values <- preProcess(weather.df, method = "range")
weather.norm <- predict(norm.values, weather.df)
summary(weather.norm)

# partition the data
set.seed(2)
train.index <- sample(nrow(weather.df), nrow(weather.df)*0.6)
valid.index <- setdiff(row.names(weather.df), train.index)
weather.train <- weather.df[train.index, ]
weather.valid <- weather.df[valid.index, ]

summary(weather.train)

# 1 hidden layer containing 3 nodes
nn <- neuralnet(RainTomorrow ~ ., data = weather.train, 
                linear.output = FALSE, hidden = 3)
plot(nn, rep = "best")

#confusion matrix
predict <- compute(nn, data.frame(weather.valid))
predicted.class <- apply(predict$net.result, 1, which.max) - 1
confusionMatrix(as.factor(ifelse(predicted.class == 1, "Yes", "No")), 
                weather.valid$RainTomorrow, positive = "Yes")

# 1 hidden layer containing 8 nodes
nn2 <- neuralnet(RainTomorrow ~ ., data = weather.train, 
                linear.output = FALSE, hidden = 8)
plot(nn2, rep = "best")

#confusion matrix
predict2 <- compute(nn2, data.frame(weather.valid))
predicted.class2 <- apply(predict2$net.result, 1, which.max) - 1
confusionMatrix(as.factor(ifelse(predicted.class2 == 1, "Yes", "No")), 
                weather.valid$RainTomorrow, positive = "Yes")

# 2 hidden layers with 3 nodes in each
nn3 <- neuralnet(RainTomorrow ~ ., data = weather.train, hidden = c(3, 3), stepmax = 1e6)
plot(nn3, rep = "best")

#confusion matrix
predict3 <- compute(nn3, data.frame(weather.valid))
predicted.class3 <- apply(predict3$net.result, 1, which.max) - 1
confusionMatrix(as.factor(ifelse(predicted.class3 == 1, "Yes", "No")), 
                weather.valid$RainTomorrow, positive = "Yes")

