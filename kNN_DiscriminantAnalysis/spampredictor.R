#Load all packages needed
library(dplyr)
library(DiscriMiner)
library(caret)
library(lubridate)
library(forecast)
library(gplots)
library(gains)
library(caret)
library(FNN)
#Load Dataset and rename variables
read.csv("spambase.csv")
spam.df <- read.csv("spambase.csv")

spam.df <- rename(spam.df, "re:" = re., "C;" = C., "C(" = C..1, "C[" = C..2, "C!" = C..3, "C$" = C..4, "C#" = C..5)
t(t(names(spam.df)))

#Split data up by spam or nonspam
aggregate(. ~ Spam, spam.df, mean)

## partition into 60% training and 40% validation for DA
set.seed(1)
train.index.da <- sample(nrow(spam.df), nrow(spam.df) * 0.6)
valid.index.da <- as.numeric(setdiff(rownames(spam.df), train.index.da))
spam.train.da <- spam.df[train.index.da, ]
spam.valid.da <- spam.df[valid.index.da, ]

#Running Linear Discriminant Analysis
spam.da <- linDA(spam.df[, c(1:57)], spam.df$Spam, validation = "learntest", 
                 learn = train.index.da, test = valid.index.da)
spam.da$functions

#Confusion Matrix
confusionMatrix(spam.da$classification, 
                as.factor(spam.valid.da$Spam), 
                positive = "1")

# lift chart
gain <- gains(as.numeric(spam.valid.da$Spam), 
              exp(spam.da$scores[, 2]) / (exp(spam.da$scores[, 1]) + exp(spam.da$scores[, 2])), 
              groups = length(spam.valid.da))
# plot lift chart
plot(c(0, gain$cume.pct.of.total * sum(as.numeric(spam.valid.da$Spam))) ~ c(0, gain$cume.obs),
     xlab = "# of cases", ylab = "Cumulative", main = "Lift Chart", type = "l")
lines(c(0, sum(as.numeric(spam.valid.da$Spam))) ~ c(0, nrow(spam.valid.da)), lty = 2)

# compute deciles and plot decile-wise lift chart
gain <- gains(as.numeric(spam.valid.da$Spam), 
              exp(spam.da$scores[, 2]) / (exp(spam.da$scores[, 1]) + exp(spam.da$scores[, 2])))
heights <- gain$mean.resp / mean(as.numeric(spam.valid.da$Spam))
dec.lift <- barplot(heights, names.arg = gain$depth, ylim = c(0, 6),
                    xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise Lift Chart")

# create new record
spam.new <- spam.df[1, -58] # just copy the first row of spam.df and drop the outcome variable
cols.new <- colnames(spam.new)
# set all variables to 0
for (i in cols.new) {
  spam.new[[i]] <- 0
}

# only update those variables whose values are not 0
spam.new$mail <- 3.77
spam.new$you <- 1.88
spam.new$money <- 1.88
spam.new$`C;` <- 0.264
spam.new$CAP_avg <- 4.333
spam.new$CAP_long <- 13
spam.new$CAP_tot <- 78

# standardize numerical predictors to 0-1 scale for both the new record and full dataset
cols <- colnames(spam.df[,-58])
for (i in cols) {
  spam.new[[i]] <- (spam.new[[i]] - min(spam.df[[i]])) / (max(spam.df[[i]]) - min(spam.df[[i]]))
  spam.df[[i]] <- (spam.df[[i]] - min(spam.df[[i]])) / (max(spam.df[[i]]) - min(spam.df[[i]]))
}
summary(spam.df)

## partition into 60% training and 40% validation for knn
set.seed(1)
train.index.knn <- sample(nrow(spam.df), nrow(spam.df) * 0.6)
valid.index.knn <- as.numeric(setdiff(rownames(spam.df), train.index.knn))
spam.train.knn <- spam.df[train.index.knn, ]
spam.valid.knn <- spam.df[valid.index.knn, ]

##Standardization
spam.df.norm <- spam.df

##################KNN Analysis

## normalize data
# initialize normalized training, validation, and complete data frames to originals
spam.train.norm <- spam.train.knn
spam.valid.norm <- spam.valid.knn
spamKNN.df.norm <- spam.df

# use knn() to compute knn
# knn() is available in library FNN (provides a list of the nearest neighbors)
# and library class (allows a numerical output variable)
nn1 <- knn(train = spam.train.norm[, 1:57], test = spam.valid.norm[, 1:57], cl = spam.train.norm$Spam, k = 1)
row.names(spam.train.knn)[attr(nn1, "nn1.index")]
#creating a confusion matrix for k = 1
confusionMatrix(nn1, as.factor(spam.valid.norm$Spam), positive = "1")

# classifying a new email using k=1
knn.pred.new <- knn(spam.df.norm[, 1:57], spam.new, cl = spam.df.norm[, 58], k=1)
knn.pred.new
## measuring accuracy of different k-values
# initialize a data frame with two columns: k and accuracy
accuracy.df <- data.frame(k = seq(1, 20, 1), accuracy = rep(0, 20))

# compute knn for different k on validation set
for (i in 1:20) {
  knn.pred <- knn(train = spam.train.norm[, 1:57], test = spam.valid.norm[, 1:57], 
                  cl = spam.train.norm$Spam, k = i)
  accuracy.df[i, 2] <- confusionMatrix(knn.pred, as.factor(spam.valid.norm$Spam), positive = "1")$overall[1]
}
accuracy.df

#computing confusion matrix for the most accurate k value (k = 3)
nn3 <- knn(train = spam.train.norm[, 1:57], test = spam.valid.norm[, 1:57], cl = spam.train.norm$Spam, k = 3)
confusionMatrix(nn3, as.factor(spam.valid.norm$Spam), positive = "1")

# classifying a new email using k=3
knn.pred.new <- knn(spam.df.norm[, 1:57], spam.new, cl = spam.df.norm[, 58], k=3)
knn.pred.new






