## S1: Import the Dataset into R
IMDBmovies.df <- read.csv("/Users/MACOS/Downloads/IMDB_Movie_Dataset.csv")

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
#IMDBmovies.df$content_rating[is.na(IMDBmovies.df$content_rating)] <- '.'

#2. Delete all observations with fewer than 30 missing data cell (categorical)
IMDBmovies.df <- IMDBmovies.df[!is.na(IMDBmovies.df$color), ]
IMDBmovies.df <- IMDBmovies.df[!is.na(IMDBmovies.df$director_name), ]
IMDBmovies.df <- IMDBmovies.df[!is.na(IMDBmovies.df$actor_2_name), ]
IMDBmovies.df <- IMDBmovies.df[!is.na(IMDBmovies.df$actor_1_name), ]
IMDBmovies.df <- IMDBmovies.df[!is.na(IMDBmovies.df$movie_title), ]
IMDBmovies.df <- IMDBmovies.df[!is.na(IMDBmovies.df$actor_3_name), ]
#IMDBmovies.df$content_rating == "" <- IMDBmovies.df$content_rating == "."
#We decided not to clean movie_imdb_link variables, since they have nothing to do with the IMDB score
