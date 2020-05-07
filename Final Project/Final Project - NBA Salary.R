NBASalary <- read.csv("/Users/MACOS/Desktop/Data Mining/Final Project/2017-18_NBA_salary.csv", header = TRUE)
summary(NBASalary)

#Removing Name and Team column because there are too many attributes to turn into a dummy variable
NBASalary<-NBASalary[, -c(1,6)]

#Making NBA_Country a binary dummy variable since 77% of the entries are USA
CountryUSA <- model.matrix(~ 0 + NBA_Country, 
                           data = NBASalary)
as.data.frame(CountryUSA)
CountryUSA <- CountryUSA[, -c(1:43)]
NBASalary <- cbind(NBASalary[, -c(2)], CountryUSA)
t(t(names(NBASalary)))

#creating a correlation table to determine which predictors could be important
library(gplots)
colfunc <- colorRampPalette(c("red", "white", "green"))
heatmap.2(cor(Filter(is.numeric, NBASalary), use = "complete.obs"), Rowv = FALSE, 
          Colv = FALSE, dendrogram = "none", lwid=c(0.1,4), lhei=c(0.1,4), 
          col = colfunc(15), 
          cellnote = round(cor(Filter(is.numeric, NBASalary), use = "complete.obs"),2),
          notecol = "black", key = FALSE, trace = 'none')

# Histogram of Salary
options(scipen = 888) #Turn off Scientific Notation
hist(NBASalary$Salary, xlab = "Salary", main = "Histogram of Salary", col = "blue", las = 2)

#=> VORP,MP, WS and DWS have the strongest correlations and we may use one of them for our models.
# We create Scatter plots for those 4 variables
options(scipen = 0) #Turn on Scientific Notation
#boxplot(NBASalary$Salary ~ NBASalary$WS, ylab = "NBASalary", xlab = "Wins Shares") #Not really helpful
plot(NBASalary$Salary ~ NBASalary$WS, xlab = "Wins Shares", ylab = "Salary", las = 1)
plot(NBASalary$Salary ~ NBASalary$VORP, xlab = "Value Over Replacement Player", ylab = "Salary")
plot(NBASalary$Salary ~ NBASalary$OWS, xlab = "Offensive Wins Shares", ylab = "Salary")
plot(NBASalary$Salary ~ NBASalary$MP, xlab = "Minutes Played", ylab = "Salary")

set.seed(1)
## partitioning into training (60%) and validation (40%) 
# randomly sample 60% of the row IDs for training; the remaining 40% serve as
# validation
train.rows <- sample(rownames(NBASalary), nrow(NBASalary)*0.6)
# collect all the columns with training row ID into training set:
train.data <- NBASalary[train.rows, ]
# assign row IDs that are not already in the training set into validation
valid.rows <- setdiff(rownames(NBASalary), train.rows)
valid.data <- NBASalary[valid.rows, ]

library(forecast)
#Fitting a full regression model
options(scipen = 999, digits = 5)
NBASalary.lm <- lm(Salary ~ ., data = train.data)
#Predictor summary
sum.full <- summary(NBASalary.lm)
sum.full
dim(sum.full$coefficients)
AIC(NBASalary.lm)

# perform exhaustive search
#library(leaps)
#imdb.search <- regsubsets(imdb_score ~ ., data = train.data, nbest = 1, nvmax = ncol(train.data),
#                          really.big = TRUE)
# sum <- summary(imdb.search)
# t(t(sum$cp))

# perform forward selelction
NBASalary.lm.null <- lm(Salary ~ 1, data = train.data)
NBASalary.lm.fwd <- step(NBASalary.lm.null, scope = list(NBASalary.lm.null, upper = NBASalary.lm), 
                    direction = "forward")
sum.fwd <- summary(NBASalary.lm.fwd)
sum.fwd
dim(sum.fwd$coefficients)
AIC(NBASalary.lm.fwd)

# perform backward elimination
NBASalary.lm.back <- step(NBASalary.lm, direction = "backward")
sum.back <- summary(NBASalary.lm.back)
sum.back
dim(sum.back$coefficients)
AIC(NBASalary.lm.back)

# perform stepwise regression
NBASalary.lm.step <- step(NBASalary.lm.null, scope = list(NBASalary.lm.null, upper = NBASalary.lm),
                     direction = "both")
sum.step <- summary(NBASalary.lm.step)
sum.step
dim(sum.step$coefficients)
AIC(NBASalary.lm.step)

sum.full$adj.r.squared
sum.fwd$adj.r.squared
sum.back$adj.r.squared
sum.step$adj.r.squared

dim(sum.full$coefficients)
dim(sum.fwd$coefficients)
dim(sum.back$coefficients)
dim(sum.step$coefficients)

# use the models to predict IMDB scores on the validation data
library(forecast)

pred.full <- predict(NBASalary.lm, newdata = valid.data)
accuracy(pred.full, valid.data$Salary)

pred.fwd <- predict(NBASalary.lm.fwd, newdata = valid.data)
accuracy(pred.fwd, valid.data$Salary)

pred.back <- predict(NBASalary.lm.back, newdata = valid.data)
accuracy(pred.back, valid.data$Salary)

pred.step <- predict(NBASalary.lm.step, newdata = valid.data)
accuracy(pred.step, valid.data$Salary)

#creating regression tree to predict salary
NBASalary.rt <- rpart(Salary ~ ., data = train.data)
prp(NBASalary.rt, digits = 4, type = 1, extra = 1, varlen = -10,
    box.col = ifelse(NBASalary.rt$frame$var == "<leaf>", 'gray', 'white'))

#finding out-of-sample prediction
NBASalary.valid.rt.pred <- predict(NBASalary.rt, newdata = valid.data)
RMSE(NBASalary.valid.rt.pred, valid.data$Salary)

#finding in-sample prediction
NBASalary.train.rt.pred <- predict(NBASalary.rt)
library(caret)
RMSE(NBASalary.train.rt.pred, train.data$Salary)

