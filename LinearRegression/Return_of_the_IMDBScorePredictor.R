#Depends on your dataset, the Color variable could be named Color or !..color. Please be wary of this

## S1: Import the Dataset into R
IMDBmovies.df <- read.csv("IMDB Movie Dataset.csv", header = TRUE)
#install.packages
library(gplots)
library(forecast)
## S2: Summarize the data
summary(IMDBmovies.df)

## S3: Cleaning the data
#1. Make sure there is no abnormal values
IMDBmovies.df <- subset.data.frame(IMDBmovies.df, (imdb_score <= 10 & imdb_score >= 0)|is.na(imdb_score))
# No score is more than 10 and less than 0
IMDBmovies.df <- subset.data.frame(IMDBmovies.df, (title_year >= 1915 & title_year <= 2016) | is.na(title_year))
# The movies included are released from 1915 to 2016

## S4: Handling Missing data
#1. Replacing missing numerical data with the median of the rest of the variables
IMDBmovies.df$budget[is.na(IMDBmovies.df$budget)] <- median(IMDBmovies.df$budget, na.rm = TRUE)
IMDBmovies.df$num_critic_for_reviews[is.na(IMDBmovies.df$num_critic_for_reviews)] <- median(IMDBmovies.df$num_critic_for_reviews, na.rm = TRUE)
IMDBmovies.df$duration[is.na(IMDBmovies.df$duration)] <- median(IMDBmovies.df$duration, na.rm = TRUE)
IMDBmovies.df$director_facebook_likes[is.na(IMDBmovies.df$director_facebook_likes)] <- median(IMDBmovies.df$director_facebook_likes, na.rm = TRUE)
IMDBmovies.df$actor_3_facebook_likes[is.na(IMDBmovies.df$actor_3_facebook_likes)] <- median(IMDBmovies.df$actor_3_facebook_likes, na.rm = TRUE)
IMDBmovies.df$actor_1_facebook_likes[is.na(IMDBmovies.df$actor_1_facebook_likes)] <- median(IMDBmovies.df$actor_1_facebook_likes, na.rm = TRUE)
IMDBmovies.df$gross[is.na(IMDBmovies.df$gross)] <- median(IMDBmovies.df$gross, na.rm = TRUE)
IMDBmovies.df$facenumber_in_poster[is.na(IMDBmovies.df$facenumber_in_poster)] <- median(IMDBmovies.df$facenumber_in_poster, na.rm = TRUE)
IMDBmovies.df$num_user_for_reviews[is.na(IMDBmovies.df$num_user_for_reviews)] <- median(IMDBmovies.df$num_user_for_reviews, na.rm = TRUE)
IMDBmovies.df$title_year[is.na(IMDBmovies.df$title_year)] <- median(IMDBmovies.df$title_year, na.rm = TRUE)
IMDBmovies.df$actor_2_facebook_likes[is.na(IMDBmovies.df$actor_2_facebook_likes)] <- median(IMDBmovies.df$actor_2_facebook_likes, na.rm = TRUE)
IMDBmovies.df$aspect_ratio[is.na(IMDBmovies.df$aspect_ratio)] <- median(IMDBmovies.df$aspect_ratio, na.rm = TRUE)

#2. Delete all observations with fewer than 30 missing data cell (categorical)
IMDBmovies.df <- IMDBmovies.df[IMDBmovies.df$ï..color != "", ] 
IMDBmovies.df <- IMDBmovies.df[!(is.na(IMDBmovies.df$director_name)|IMDBmovies.df$director_name==""), ]
IMDBmovies.df <- IMDBmovies.df[!(is.na(IMDBmovies.df$actor_2_name)|IMDBmovies.df$actor_2_name==""), ]
IMDBmovies.df <- IMDBmovies.df[!(is.na(IMDBmovies.df$actor_1_name)|IMDBmovies.df$actor_1_name==""), ]
IMDBmovies.df <- IMDBmovies.df[!(is.na(IMDBmovies.df$movie_title)|IMDBmovies.df$movie_title==""), ]
IMDBmovies.df <- IMDBmovies.df[!(is.na(IMDBmovies.df$actor_3_name)|IMDBmovies.df$actor_3_name==""), ]
#We decided not to clean movie_imdb_link variables and plot_keywords, since they have nothing to do with the IMDB score
summary(IMDBmovies.df)

## S5: Creating dummy variables
# Change categorical data to numeric data
model.matrix(~0 + language, data = IMDBmovies.df)
English <- model.matrix(~0 + language, data = IMDBmovies.df)
as.data.frame(English)
English <- as.data.frame(English)

# Single out English to show if the movie is in English (1) or not (0)
English[, c(13)]
English <- English[, c(13)]

# Bind the variable to the dataset
IMDBmovies.df <-cbind(IMDBmovies.df, English)

# Building a boxplot of English binary variables
boxplot(IMDBmovies.df$imdb_score ~ IMDBmovies.df$English, xlab = "English", ylab = "IMDB Score")

IMDBmovies.df = IMDBmovies.df[,!grepl("^language",names(IMDBmovies.df))]

## S6: Creating a heatmap/correlation table
#sapply(IMDBmovies.df, class)
#sapply(IMDBmovies.df, is.factor)

# Building a correlation table with complete observations
cor(IMDBmovies.df[sapply(IMDBmovies.df, function(x) !is.factor(x))])

# Building the correlation table using only complete observations
#cor(IMDBmovies.df[sapply(IMDBmovies.df, function(x) !is.factor(x))], use = "complete.obs")

# Round those correlations up to 2 digits after the decimal
colfunc <- colorRampPalette(c("green", "white", "red"))
round(cor(IMDBmovies.df[sapply(IMDBmovies.df, function(x) !is.factor(x))], use = "complete.obs"),2)
heatmap.2(cor(IMDBmovies.df[sapply(IMDBmovies.df, function(x) !is.factor(x))], use = "complete.obs"), Rowv = FALSE, Colv = FALSE, dendrogram = "none", col = colfunc(16), cellnote = round(cor(IMDBmovies.df[sapply(IMDBmovies.df, function(x) !is.factor(x))], use = "complete.obs"),2), notecol = "black", key = FALSE, trace = 'none', margins = c(10,10))

#Building a historygram for the frequency of number of users for reviews
hist(IMDBmovies.df$duration, xlab = "Duration")
hist(IMDBmovies.df$num_critic_for_reviews, xlab = "Number of Critic Reviews")
hist(IMDBmovies.df$num_user_for_reviews, xlab = "Number of User Reviews")
hist(IMDBmovies.df$num_voted_users, xlab = "Number of User Votes")
hist(IMDBmovies.df$imdb_score, xlab = "IMDB scores")


##ScatterPLOTS
plot(IMDBmovies.df$imdb_score ~ IMDBmovies.df$num_voted_users, xlab = "Number of User Votes",ylab = "IMDB Score")
plot(IMDBmovies.df$imdb_score ~ IMDBmovies.df$num_user_for_reviews, xlab = "Number of User Reviews",ylab = "IMDB Score")
plot(IMDBmovies.df$imdb_score ~ IMDBmovies.df$duration, xlab = "Duration",ylab = "IMDB Score")
plot(IMDBmovies.df$imdb_score ~ IMDBmovies.df$num_critic_for_reviews, xlab = "Number of Critic Reviews",ylab = "Number of Critic Reviews")

##BarCharts
data.for.plot <- aggregate.data.frame(IMDBmovies.df$duration, by = list(IMDBmovies.df$imdb_score), FUN = length)
names(data.for.plot) <- c("duration","IMDbscore")
barplot(height = data.for.plot$IMDbscore,names.arg = data.for.plot$duration, xlab = "IMDb Score", ylab = "Duration")

data.for.plot2 <- aggregate.data.frame(IMDBmovies.df$num_user_for_reviews, by = list(IMDBmovies.df$imdb_score), FUN = length)
names(data.for.plot2) <- c("userreviews","IMDbscore")
barplot(height = data.for.plot2$IMDbscore,names.arg =  data.for.plot2$userreviews, xlab = "IMDb Score", ylab = "Number of User Reviews")

data.for.plot3 <- aggregate.data.frame(IMDBmovies.df$num_critic_for_reviews, by = list(IMDBmovies.df$imdb_score), FUN = length)
names(data.for.plot3) <- c("criticreviews","IMDbscore")
barplot(height = data.for.plot3$IMDbscore,names.arg =  data.for.plot3$criticreviews, xlab = "IMDb Score", ylab = "Number of Critic Reviews")

data.for.plot4 <- aggregate.data.frame(IMDBmovies.df$num_voted_users, by = list(IMDBmovies.df$imdb_score), FUN = length)
names(data.for.plot4) <- c("criticreviews","IMDbscore")
barplot(height = data.for.plot4$IMDbscore,names.arg =  data.for.plot4$num_voted_users, xlab = "IMDb Score", ylab = "Number of User Votes")

View(IMDBmovies.df)

# Changing color into a binary variable and binding it to the dataset
model.matrix(~0 + ï..color, data = IMDBmovies.df)
Color <- model.matrix(~0 + ï..color, data = IMDBmovies.df)
as.data.frame(Color)
Color <- as.data.frame(Color)
Color <- Color[, c(3)]
IMDBmovies.df <-cbind(IMDBmovies.df, Color)

#Changing country to a USA binary and binding it to the dataset
USA <- model.matrix(~0 + country, data = IMDBmovies.df)
USA <- as.data.frame(USA)
USA <- USA[, c("countryUSA")]
IMDBmovies.df <-cbind(IMDBmovies.df, USA)

# Changing Content Rating to a PG-13 binary and binding it to the dataset
PG13 <- model.matrix(~0 + content_rating, data = IMDBmovies.df)
PG13 <- as.data.frame(PG13)
PG13 <- PG13[, c("content_ratingPG-13")]
IMDBmovies.df <-cbind(IMDBmovies.df, PG13)

#Evaluating other predictors
str(IMDBmovies.df)
View(IMDBmovies.df)
#Getting rid of ones that will not be useful
## (Includes Actor names, director names, movie title, genre, keywords and original categorical columns that we now have binary variables for)
IMDBmovies.df <- IMDBmovies.df[-c(1:2,7,10:12,15,17:18,20:21)]

#Partitioning the data, 60% of the data for the training set and 40% for the validation
set.seed(1)
train.rows <- sample(rownames(IMDBmovies.df), nrow(IMDBmovies.df)*.6)
train.data <- IMDBmovies.df[train.rows, ]
valid.rows <- setdiff(rownames(IMDBmovies.df), train.rows)
valid.data <- IMDBmovies.df[valid.rows, ]


#Fitting a full regression model
IMDB.lm <- lm(imdb_score ~ ., data = train.data)
options(scipen = 999)
#Predictor summary
summary(IMDB.lm)
AIC(IMDB.lm)
pred.lm <- predict(IMDB.lm, newdata = valid.data)
accuracy(pred.lm, valid.data$imdb_score)

#Stepwise Regression
## Model with no predictor for bottom of search range
IMDB.lm.null <- lm(imdb_score ~ 1, data = train.data)
## Run stepwise regression model
IMDB.lm.step <- step(IMDB.lm.null, scope = list(IMDB.lm.null, upper = IMDB.lm), direction = "both")
#Predictor summary
summary(IMDB.lm.step)
AIC(IMDB.lm.step)
pred.step <- predict(IMDB.lm.step, newdata = valid.data)
accuracy(pred.step, valid.data$imdb_score)

#Forward Selection
IMDB.lm <- lm(imdb_score ~ ., data = train.data)
#Create model with no predictors for bottom of search range
IMDB.lm.null <- lm(imdb_score ~ 1, data = train.data)
#Use step() to run forward selection
IMDB.lm.fwd <- step(IMDB.lm.null, scope = list(IMDB.lm.null, upper = IMDB.lm), direction = "forward")
#predictor sumamry
summary(IMDB.lm.fwd)
AIC(IMDB.lm.fwd)
pred.fwd <- predict(IMDB.lm.fwd, newdata = valid.data)
accuracy(pred.fwd, valid.data$imdb_score)

#Backward Elimination
imdb.lm.back <- step(IMDB.lm, direction = "backward")
#Predictor summary
summary(imdb.lm.back)
AIC(imdb.lm.back)
pred.back <- predict(imdb.lm.back, newdata = valid.data)
accuracy(pred.back, valid.data$imdb_score)
