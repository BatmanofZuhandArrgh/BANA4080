###I. EDA
##1. Loading the data into weather dataframe
weather<-read.csv("/Users/MACOS/Desktop/Data Mining/5. Logistic Regression/weatherAUS.csv")

##2. Summary
summary(weather)
dim(weather)
str(weather)

#3. Install packages
library(lubridate)
library(forecast)
library(gplots)

#4. Creating New Variables
#Changing Date to Month only and removing Date variable
weather$Date <- as.Date(weather$Date , "%m/%d/%Y")
Month <- month(weather$Date)
weather <- cbind(weather, Month)
weather <- weather[-c(1)]

#Turning RainToday into a dummy variable
DummyRainToday <- model.matrix(~0 + RainToday, data = weather)
DummyRainToday <- as.data.frame(DummyRainToday)
DummyRainToday <- DummyRainToday[c("RainTodayYes")]
weather <-cbind(weather,DummyRainToday)
weather <- weather[, -c(21)]

#Turning RainTomorrow into a dummy variable 
DummyRainTomorrow <- model.matrix(~0 + RainTomorrow, data = weather)
DummyRainTomorrow <- as.data.frame(DummyRainTomorrow)
DummyRainTomorrow <- DummyRainTomorrow[c(2)]
weather <- cbind(weather, DummyRainTomorrow)
weather <- weather[, -c(21)]
#5. Cleaning data
#Deleting the Wind Directions Variable
t(t(names(weather)))
weather <- weather[, -c(7,9,10)]

#Since we expect our models to work in all locations, we delete the Location variable.
weather <- weather[-c(1)]

#Replacing missing numerical data with the median of the rest of the variables
weather$MinTemp[is.na(weather$MinTemp)] <- median(weather$MinTemp, na.rm = TRUE)
weather$MaxTemp[is.na(weather$MaxTemp)] <- median(weather$MaxTemp, na.rm = TRUE)
weather$Rainfall[is.na(weather$Rainfall)] <- median(weather$Rainfall, na.rm = TRUE)
weather$Evaporation[is.na(weather$Evaporation)] <- median(weather$Evaporation, na.rm = TRUE)
weather$Sunshine[is.na(weather$Sunshine)] <- median(weather$Sunshine, na.rm = TRUE)
weather$WindGustSpeed[is.na(weather$WindGustSpeed)] <- median(weather$WindGustSpeed, na.rm = TRUE)
weather$WindSpeed9am[is.na(weather$WindSpeed9am)] <- median(weather$WindSpeed9am, na.rm = TRUE)
weather$WindSpeed3pm[is.na(weather$WindSpeed3pm)] <- median(weather$WindSpeed3pm, na.rm = TRUE)
weather$Humidity9am[is.na(weather$Humidity9am)] <- median(weather$Humidity9am, na.rm = TRUE)
weather$Humidity3pm[is.na(weather$Humidity3pm)] <- median(weather$Humidity3pm, na.rm = TRUE)
weather$Pressure9am[is.na(weather$Pressure9am)] <- median(weather$Pressure9am, na.rm = TRUE)
weather$Pressure3pm[is.na(weather$Pressure3pm)] <- median(weather$Pressure3pm, na.rm = TRUE)
weather$Cloud9am[is.na(weather$Cloud9am)] <- median(weather$Cloud9am, na.rm = TRUE)
weather$Cloud3pm[is.na(weather$Cloud3pm)] <- median(weather$Cloud3pm, na.rm = TRUE)
weather$Temp9am[is.na(weather$Temp9am)] <- median(weather$Temp9am, na.rm = TRUE)
weather$Temp3pm[is.na(weather$Temp3pm)] <- median(weather$Temp3pm, na.rm = TRUE)

#Two Way Contingency Table between RainTodayYes and RainTomorrowYes
table.weather <- table(weather$RainTodayYes, weather$RainTomorrowYes)

table.weather

#Perform Chi-square test
chisq.test(table.weather)

#6. Correlation Table
#Finding the variables with the higher correlation to Rain Tomorrow
colfunc <- colorRampPalette(c("red", "white", "green"))
heatmap.2(cor(weather), Rowv = FALSE, Colv = FALSE, dendrogram = "none", col = colfunc(15), cellnote = round(cor(weather), 2), notecol = "black", key = FALSE, trace = 'none', margins = c(10,10))
round(cor(weather), 2)

#7. Boxplot
boxplot(weather$Cloud3pm ~ weather$RainTomorrowYes, xlab = "Cloud at 3PM", ylab = "Rain Tmr?")
boxplot(weather$Sunshine ~ weather$RainTomorrowYes, xlab = "Sunshine", ylab = "Rain Tmr?")
boxplot(weather$Humidity3pm ~ weather$RainTomorrowYes, xlab = "Humidity at 3PM", ylab = "Rain Tmr?")
boxplot(weather$RainTodayYes ~ weather$RainTomorrowYes, xlab = "Rain Today?", ylab = "Rain Tmr?")

##Partitioning the data
set.seed(1)
## partition into 60% training and 40% validation
train.rows <- sample(rownames(weather), nrow(weather)*0.6)
#Collect all the columns with training row id into training set:
train.data <- weather[train.rows,]
#Assign row ID's that are not already in a training set into a validation
valid.rows <- setdiff(rownames(weather), train.rows)
valid.data <- weather[valid.rows, ]

##############Logistic Regression
weather.glm0 <- glm(RainTomorrowYes ~ ., family = "binomial", data = train.data)
options(scipen = 999)
data.frame(odds = exp(coef(weather.glm0)))
summary(weather.glm0)

# evaluating model performance
library(gains)
pred <- predict(weather.glm0, valid.data)
gain <- gains(valid.data$RainTomorrowYes, pred, groups = 100)

#############Using Logistic Regression to predict the validation data
logit.reg.pred <- predict(weather.glm0, valid.data, type = "response")
data.frame(actual = valid.data$RainTomorrowYes[1:30], predicted = logit.reg.pred[1:30])

##############Confusion Matrix
library(caret)
confusionMatrix(factor(ifelse(pred >= 0.5, 1, 0)), factor(valid.data$RainTomorrowYes), positive = "1")
par(mfrow = c(1, 1))

# roc curve
library(pROC)
r <- roc(valid.data$RainTomorrowYes, logit.reg.pred)
plot.roc(r)
auc(r)

#Creating lift chart
pred.glm0.valid <- predict(weather.glm0, newdata = valid.data, type = "response")
library(gains)
gain <- gains(valid.data$RainTomorrowYes, pred.glm0.valid, groups = length(pred.glm0.valid))


# plot lift chart
plot(c(0, gain$cume.pct.of.total * sum(valid.data$RainTomorrowYes)) ~ c(0, gain$cume.obs),
     xlab = "# of cases", ylab = "Cumulative", main = "Lift Chart", type = "l")
lines(c(0, sum(valid.data$RainTomorrowYes)) ~ c(0, nrow(valid.data)), lty = 2)


#Creating the Decile chart
gain.dec <- gains(valid.data$RainTomorrowYes, pred.glm0.valid)
heights <- gain.dec$mean.resp / mean(valid.data$RainTomorrowYes)
dec.lift <- barplot(heights, names.arg = gain.dec$depth, ylim = c(0, 4),
                    xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise Lift Chart")


#Stepwise Regression
## Model with no predictor for bottom of search range
weather.glm0.null <- glm(RainTomorrowYes ~ 1, family = "binomial", data = train.data)
## Run stepwise regression model
weather.glm0.step <- step(weather.glm0.null, scope = list(weather.glm0.null, upper = weather.glm0), direction = "both")
#Predictor summary
summary(weather.glm0.step)
AIC(weather.glm0.step)
BIC(weather.glm0.step)
pred.step <- predict(weather.glm0.step, newdata = valid.data)
accuracy(pred.step, valid.data$RainTomorrowYes)

#Forward Selection
weather.glm0 <- glm(RainTomorrowYes ~ ., family = "binomial", data = train.data)
#Create model with no predictors for bottom of search range
weather.glm0.null <- glm(RainTomorrowYes ~ 1, family = "binomial", data = train.data)
#Use step() to run forward selection
weather.glm0.fwd <- step(weather.glm0.null, scope = list(weather.glm0.null, upper = weather.glm0), direction = "forward")
#predictor sumamry
summary(weather.glm0.fwd)
AIC(weather.glm0.fwd)
BIC(weather.glm0.fwd)
pred.fwd <- predict(weather.glm0.fwd, newdata = valid.data)
accuracy(pred.fwd, valid.data$RainTomorrowYes)

#Backward Elimination
weather.glm0.back <- step(weather.glm0, direction = "backward")
#Predictor summary
summary(weather.glm0.back)
AIC(weather.glm0.back)
BIC(weather.glm0.back)
pred.back <- predict(weather.glm0.back, newdata = valid.data)
accuracy(pred.back, valid.data$RainTomorrowYes)

