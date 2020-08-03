##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")


# if using R 4.0 or later
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)


library(dslabs)
library(tidyverse)
library(lubridate)

# Changing the format of the timestamp
edx <- mutate(edx, date = as_datetime(timestamp))
validation <- mutate(validation, date = as_datetime(timestamp))

#Extracting the year to be its own column
edx <- edx %>% mutate(year=as.numeric(str_sub(title,-5,-2)))
validation <- validation %>% mutate(year=as.numeric(str_sub(title,-5,-2)))

# Summary of the 'edx' dataset
summary(edx)

#Distribution of the movie ratings
edx%>%group_by(rating)%>%
  summarise(number_of_reviews=n())%>%
  ggplot(aes(rating, number_of_reviews))+
  geom_point(color= "blue")+
  ggtitle("Distribution of the ratings")

# Number of unique movies in the 'edx' dataset
edx %>% 
  select(movieId)%>%
  unique()%>%
  nrow()
#Number of unique users in the 'edx' dataset
edx %>% 
  select(userId)%>%
  unique()%>%
  nrow()

# Showing the effect of the number of ratings on the average rating of a movie
edx%>%group_by(movieId)%>%
  summarise(n=n(), avg_rating=mean(rating))%>%
  ggplot(aes(n,avg_rating))+
  geom_point()+
  ggtitle("Distribution of the mean rating by number of ratings")

# Showing the number of ratings per user
edx%>%group_by(userId)%>%
  summarise(number_of_reviews=n())%>%
  ggplot(aes(number_of_reviews))+
  geom_histogram(color= "grey")+
  scale_x_log10()+
  ggtitle("Distribution of the number of ratings by users")

# Showing the effect of the year on the average rating of a movie
edx%>%group_by(year)%>%
  summarise(avg_rating=mean(rating))%>%
  ggplot(aes(year,avg_rating))+
  geom_smooth(color="red")+
  geom_point()+
  ggtitle("Distribution of the ratings by year of release")


#Methods/Analysis

#Overall Mean Model
overall <- mean(edx$rating)
overall_mean_rmse <- RMSE(validation$rating, overall)

#Creation of a table to store our results
rmse_table <- data_frame(model= "Giving the overall mean rating to all ratings", RMSE= overall_mean_rmse)

rmse_table %>% knitr::kable()

# Movie effect Model

movie_norm_avgs <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating-overall))

pred_ratings_movie <- validation %>%
  left_join(movie_norm_avgs, by="movieId")%>%
  mutate(prediction = overall+b_i)

movie_model_rmse <- RMSE(validation$rating, pred_ratings_movie$prediction)

rmse_table <- bind_rows(rmse_table, data_frame(model= "Movie Effect Model", RMSE=movie_model_rmse))

rmse_table %>% knitr::kable()

# Movie and User effect Models

user_norm_avgs <- edx %>% 
  left_join(movie_norm_avgs, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating-overall-b_i))

pred_ratings_user <- validation %>%
  left_join(movie_norm_avgs, by="movieId")%>%
  left_join(user_norm_avgs, by="userId")%>%
  mutate(prediction=overall + b_i + b_u)

user_model_rmse <- RMSE(validation$rating, pred_ratings_user$prediction)

rmse_table <- bind_rows(rmse_table, data_frame(model= "Movie and User Effect Model", RMSE=user_model_rmse))

rmse_table %>% knitr::kable()

#Regularization of Movie and User Effects

lambdas <- seq(1, 15, 0.25)

rmses <- sapply(lambdas, function(l){
  
  b_i <- edx %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - overall)/(n()+l))
  
  b_u <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - overall)/(n()+l))
  
  predicted_ratings <- validation %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = overall + b_i + b_u) %>%
    .$pred
  return(RMSE(validation$rating, predicted_ratings))
})
plot(lambdas, rmses)
lambda <- lambdas[which.min(rmses)]
print(lambda)

#Compute predictions with regularization for movie and user bias

movie_reg_avgs <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - overall)/(n()+lambda))

user_reg_avgs <- edx %>% 
  left_join(movie_reg_avgs, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - overall)/(n()+lambda))

predicted_ratings_reg <- validation %>% 
  left_join(movie_reg_avgs, by = "movieId") %>%
  left_join(user_reg_avgs, by = "userId") %>%
  mutate(pred = overall + b_i + b_u) %>%
  .$pred

reg_model_rmse <-RMSE(validation$rating, predicted_ratings_reg)

rmse_table <- bind_rows(rmse_table, data_frame(model= "Regularized Movie and User Effect Model", RMSE=reg_model_rmse))

rmse_table %>% knitr::kable()

#Regularization with Movie, User and Year effects
lambdas2 <- seq(1, 8, 0.25)

rmses2 <- sapply(lambdas2, function(l){
  
  b_i <- edx %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - overall)/(n()+l))
  
  b_u <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - overall)/(n()+l))
  
  b_y <- edx%>%
    left_join(b_i, by="movieId")%>%
    left_join(b_u, by="userId")%>%
    group_by(year)%>%
    summarize(b_y = sum(rating - b_i - b_u - overall)/(n()+l))
  
  predicted_ratings <- validation %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_y, by = "year")%>%
    mutate(pred = overall + b_i + b_u + b_y) %>%
    .$pred
  return(RMSE(validation$rating, predicted_ratings))
})
plot(lambdas2, rmses2)
lambda_final <- lambdas2[which.min(rmses2)]
print(lambda_final)

#Calculating Final predictions and RMSE with regularization

movie_reg2_avgs <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - overall)/(n()+lambda_final))

user_reg2_avgs <- edx %>% 
  left_join(movie_reg2_avgs, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - overall)/(n()+lambda_final))

year_reg2_avgs <- edx%>%
  left_join(movie_reg2_avgs, by = "movieId") %>%
  left_join(user_reg2_avgs, by = "userId") %>%
  group_by(year)%>%
  summarize(b_y = sum(rating - b_i - b_u - overall)/(n()+lambda_final))

predicted_ratings_reg2 <- validation %>% 
  left_join(movie_reg2_avgs, by = "movieId") %>%
  left_join(user_reg2_avgs, by = "userId") %>%
  left_join(year_reg2_avgs, by="year")%>%
  mutate(pred = overall + b_i + b_u + b_y) %>%
  .$pred

reg2_model_rmse <-RMSE(validation$rating, predicted_ratings_reg2)

print(reg2_model_rmse)

rmse_table <- bind_rows(rmse_table, data_frame(model= "Regularized Movie, User and Year Effect Model", RMSE=reg2_model_rmse))

# Final results table
rmse_table %>% knitr::kable()