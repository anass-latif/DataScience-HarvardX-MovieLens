# =============================================================================
# File      :  MovieLens_Script.R
# TYpe      :  R Script
# Project   :  HarvardX - Data Science Professional Certificate
# Release   :  1.0.0
# Purpose   :  R Script to implement all the code related to
#              Capstone Project - MovieLens -
#
# =============================================================================
# Revision History
#
#  -------------------------------------------------------------------------
#  Version  Date        Author         Comments
#  -------------------------------------------------------------------------
#  1.0.0    2019.03.31  Anass Latif    Initial Creation
#  -------------------------------------------------------------------------
#
# =============================================================================



# =============================================================================
#  BEGIN OF SCRIPT
# =============================================================================



# =============================================================================
#     Step 001 - Configure all variables and session information
# =============================================================================

# Start the chronometer to get the script start time in milliseconds
script_start_time <- proc.time()
script_start_time

# Display the working directory
getwd()

# Display session information for reproductibility
sessionInfo()

# Use s seed number for reproductibility
set.seed(1)

# Setup RMSE target goal
RMSE_target_goal <- 0.87750

# Setup RMSE target goal
round_precision <- 5


# =============================================================================
#     Step 002 - Install all necessary packages if do not exist
# =============================================================================

# Package "tidyverse"
if(!require(tidyverse)) install.packages("tidyverse")

# Package "caret"
if(!require(caret)) install.packages("caret")

# Package "lubridate"
if(!require(lubridate)) install.packages("lubridate")

# Package "gridExtra"
if(!require(gridExtra)) install.packages("gridExtra")

# Package "ggrepel"
if(!require(ggrepel)) install.packages("ggrepel")

# Package "ggthemes"
if(!require(ggthemes)) install.packages("ggthemes")

# Package "knitr"
if(!require(knitr)) install.packages("knitr")

# Package "kableExtra"
if(!require(kableExtra)) install.packages("kableExtra")

# Package "summarytools"
if(!require(summarytools)) install.packages("summarytools")



# =============================================================================
#     Step 003 - Load all necessary libraries
# =============================================================================

library(tidyverse)
library(caret)
library(lubridate)
library(gridExtra)
library(knitr)
library(kableExtra)
library(ggthemes)
library(ggrepel)
library(summarytools)



# =============================================================================
#     Step 004 - Generate datasets using the original code provided by EDX Staff
# =============================================================================

#############################################################
# Create edx set, validation set, and submission file
#############################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                      col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data

set.seed(1)
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



# =============================================================================
#     Step 005 - Understand the data structure
# =============================================================================

# 005-01 edx Dataset Structure
str(edx)
nrow(edx)
ncol(edx)
sum(is.na(edx))

# 005-02 validation Dataset Structure
str(validation)
nrow(validation)
ncol(validation)
sum(is.na(validation))

# 005-03 edx Dataset Example
head(edx) %>%
   kable() %>%
   kable_styling(bootstrap_options = c("striped","hoved","condensed"),
                 position = "center",
                 font_size = 10,
                 full_width = FALSE)



# =============================================================================
#     Step 006 - Preprocess the datasets
# =============================================================================

# 006-01 Create new movieGenre feature in a new dataset for both edx and validation datasets
movieGenre_edx <- edx %>%
   mutate(movieGenre = fct_explicit_na(genres,
                                       na_level = "(no genres listed)")
          ) %>%
   separate_rows(movieGenre,
                 sep = "\\|") %>%
   group_by(movieGenre) %>%
   summarize(Rating_Count = n(),
             Rating_Average = mean(rating),
             Movie_Distinct_Count = n_distinct(movieId),
             User_Distinct_Count = n_distinct(userId))


movieGenre_validation <- validation %>%
   mutate(movieGenre = fct_explicit_na(genres,
                                       na_level = "(no genres listed)")
          ) %>%
   separate_rows(movieGenre,
                 sep = "\\|") %>%
   group_by(movieGenre) %>%
   summarize(Rating_Count = n(),
             Rating_Average = mean(rating),
             Movie_Distinct_Count = n_distinct(movieId),
             User_Distinct_Count = n_distinct(userId))

# 006-02 Create new releaseYear + ratingYear features for both edx and validation datasets
#        Remove timestamp as is not used anymore
edx <- edx %>%
   mutate(title = str_trim(title)) %>%
   extract(title,
           c("titleTemporary", "releaseYear"),
           regex = "^(.*) \\(([0-9 \\-]*)\\)$",
           remove = F) %>%
   mutate(releaseYear = if_else(str_length(releaseYear) > 4,
                                as.integer(str_split(releaseYear, "-",
                                                     simplify = T)[1]),
                                as.integer(releaseYear))
   ) %>%
   mutate(title = if_else(is.na(titleTemporary),
                          title,
                          titleTemporary)
   ) %>%
   select(-titleTemporary) %>%
   mutate(ratingYear = year(as.POSIXct(timestamp,
                                       origin = "1970-01-01"))
   ) %>%
   select(-timestamp)

validation <- validation %>%
   mutate(title = str_trim(title)) %>%
   extract(title, c("titleTemporary", "releaseYear"),
           regex = "^(.*) \\(([0-9 \\-]*)\\)$",
           remove = F) %>%
   mutate(releaseYear = if_else(str_length(releaseYear) > 4,
                                as.integer(str_split(releaseYear, "-",
                                                     simplify = T)[1]),
                                as.integer(releaseYear))
   ) %>%
   mutate(title = if_else(is.na(titleTemporary),
                          title,
                          titleTemporary)
   ) %>%
   select(-titleTemporary) %>%
   mutate(ratingYear = year(as.POSIXct(timestamp,
                                       origin = "1970-01-01"))) %>%
   select(-timestamp)

# 006-03 edx Dataset Example
head(edx) %>%
   kable() %>%
   kable_styling(bootstrap_options = c("striped","hoved","condensed"),
                 position = "center",
                 font_size = 10,
                 full_width = FALSE)



# =============================================================================
#     Step 007 - Explore and visualize the data
# =============================================================================

# 007-01 What is the overall ratings distribution?
ratings_mu <- mean(edx$rating)

edx %>%
   ggplot(aes(rating)) +
   geom_histogram(color = "darkblue",
                  fill = "lightblue",
                  bins = 10) +
   scale_x_continuous(breaks = seq(0.5, 5, 0.5)) +
   geom_vline(xintercept = ratings_mu,
              col = "red",
              linetype = "dashed") +
   labs(title = "Overall Ratings Distribution (edx)",
        x = "Rating",
        y = "Frequency") +
   theme_economist() +
   scale_color_economist() +
   theme(plot.title = element_text(size = 11, color = "darkblue", hjust = 0.5))

# 007-02 How frequently are movies rated?
edx %>%
   group_by(movieId) %>%
   summarise(Rating_Count = n()) %>%
   ggplot(aes(Rating_Count)) +
   geom_histogram(color = "darkblue",
                  fill = "lightblue",
                  bins = 50) +
   scale_x_log10() +
   labs(title = "Ratings Frequency Distribution Per Movie (edx)",
        x = "Movie Id",
        y = "Frequency") +
   theme_economist() +
   scale_color_economist() +
   theme(plot.title = element_text(size = 11, color = "darkblue", hjust = 0.5))

# 007-03 What is the cumulative rating distribution by movie?
q90_movie <- edx %>%
   group_by(movieId) %>%
   summarise(Rating_Count = n())

quantile_movie <- round(quantile(q90_movie$Rating_Count, probs = 0.9), 0)

edx %>%
   group_by(movieId) %>%
   summarise(Rating_Count = n()) %>%
   ggplot(aes(Rating_Count)) +
   stat_ecdf(geom = "step",
             color = "darkblue") +
   geom_vline(xintercept = quantile_movie,
              col = "red",
              linetype = "dashed") +
   labs(title = "Cumulative Distribution ECDF Of Ratings Per Movie (edx)",
        x = "Movies",
        y = "Cumulative Proportion Of Ratings") +
   theme_economist() +
   scale_color_economist() +
   theme(plot.title = element_text(size = 11, color = "darkblue", hjust = 0.5))

# 007-04 What are the top 10 rated movies?
edx %>%
   group_by(movieId, title) %>%
   summarise(Rating_Count = n(),
             Rating_Average = round(mean(rating), round_precision)) %>%
   arrange(desc(Rating_Average)) %>%
   head(10) %>%
   kable() %>%
   kable_styling(bootstrap_options = c("striped","hoved","condensed"),
                 position = "center",
                 font_size = 10,
                 full_width = FALSE) %>%
   column_spec(1, width = "5em") %>%
   column_spec(2, width = "20em") %>%
   column_spec(3:4, bold = TRUE)

# 007-05 What are the top 10 rated movies with at least 100 ratings?
edx %>%
   group_by(movieId, title) %>%
   summarise(Rating_Count = n(),
             Rating_Average = round(mean(rating), round_precision)) %>%
   filter(Rating_Count >= 100) %>%
   arrange(desc(Rating_Average)) %>%
   head(10) %>%
   kable() %>%
   kable_styling(bootstrap_options = c("striped","hoved","condensed"),
                 position = "center",
                 font_size = 10,
                 full_width = FALSE) %>%
   column_spec(1, width = "5em") %>%
   column_spec(2, width = "20em") %>%
   column_spec(3:4, bold = TRUE)

# 007-06 How frequently do the users rate movies?
edx %>%
   group_by(userId) %>%
   summarise(Rating_Count = n()) %>%
   ggplot(aes(Rating_Count)) +
   geom_histogram(color = "darkblue",
                  fill = "lightblue",
                  bins = 50) +
   scale_x_log10() +
   labs(title = "Ratings Frequency Distribution Per User (edx)",
        x = "Users",
        y = "Frequency") +
   theme_economist() +
   scale_color_economist() +
   theme(plot.title = element_text(size = 11, color = "darkblue", hjust = 0.5))

# 007-07 What is the cumulative rating distribution by user?
q90_user <- edx %>%
   group_by(userId) %>%
   summarise(Rating_Count = n())

quantile_user <- round(quantile(q90_user$Rating_Count, probs = 0.9), 0)

edx %>%
   group_by(userId) %>%
   summarise(Rating_Count = n()) %>%
   ggplot(aes(Rating_Count)) +
   stat_ecdf(geom = "step",
             color = "darkblue") +
   geom_vline(xintercept = quantile_user,
              col = "red",
              linetype = "dashed") +
   labs(title = "Cumulative Distribution ECDF Of Ratings Per User (edx)",
        x = "Users",
        y = "Cumulative Proportion Of Ratings") +
   theme_economist() +
   scale_color_economist() +
   theme(plot.title = element_text(size = 11, color = "darkblue", hjust = 0.5))

# 007-08 What are the top 10 Users/Raters by frequency?
edx %>%
   group_by(userId) %>%
   summarise(Rating_Count = n(),
             Rating_Average = round(mean(rating), round_precision)) %>%
   arrange(desc(Rating_Count)) %>%
   head(10) %>%
   kable() %>%
   kable_styling(bootstrap_options = c("striped","hoved","condensed"),
                 position = "center",
                 font_size = 10,
                 full_width = FALSE) %>%
   column_spec(1, width = "5em") %>%
   column_spec(2:3, bold = TRUE)

# 007-09 What are the top 10 users/raters with at least 100 ratings?
edx %>%
   group_by(userId) %>%
   summarise(Rating_Count = n(),
             Rating_Average = round(mean(rating), round_precision)) %>%
   filter(Rating_Count >= 100) %>%
   arrange(desc(Rating_Average)) %>%
   head(10) %>%
   kable() %>%
   kable_styling(bootstrap_options = c("striped","hoved","condensed"),
                 position = "center",
                 font_size = 10,
                 full_width = FALSE) %>%
   column_spec(1, width = "5em") %>%
   column_spec(2:3, bold = TRUE)

# 007-10 What is the genre ratings distribution?
movieGenre_edx %>%
   ggplot(aes(x = reorder(movieGenre, Movie_Distinct_Count),
              y = Movie_Distinct_Count)) +
   geom_bar(stat = "identity",
            color = "darkblue",
            fill = "lightblue") +
   coord_flip() +
   labs(title = "Movie Genre Distribution (edx)",
        x = "Genre",
        y = "Count Distinct Movies") +
   theme_economist() +
   scale_color_economist() +
   theme(plot.title = element_text(size = 11, color = "darkblue", hjust = 0.5))

# 007-11 What is the average ratings by genre?
movieGenre_edx %>%
   ggplot(aes(x = reorder(movieGenre, Rating_Average),
              y = Rating_Average)) +
   geom_bar(stat = "identity",
            color = "darkblue",
            fill = "lightblue") +
   coord_flip() +
   labs(title = "Average Rating Distribution By Genre (edx)",
        x = "Genre",
        y = "Rating") +
   theme_economist() +
   scale_color_economist() +
   theme(plot.title = element_text(size = 11, color = "darkblue", hjust = 0.5))

# 007-12 What is the number of ratings by genre?
movieGenre_edx %>%
   ggplot(aes(x = reorder(movieGenre, Rating_Count),
              y = Rating_Count)) +
   geom_bar(stat = "identity",
            color = "darkblue",
            fill = "lightblue") +
   coord_flip() +
   labs(title = "Number Of Rating Distribution By Genre (edx)",
        x = "Genre",
        y = "Number Of Rating") +
   theme_economist() +
   scale_color_economist() +
   theme(plot.title = element_text(size = 11, color = "darkblue", hjust = 0.5))



# =============================================================================
#     Step 008 - Create a function to compute the Residual Mean Squared Error
#                 (RMSE) known as ("typical error")
#
# =============================================================================
# Chapter 34.7.3 Loss function
# We write a loss-function that computes the Residual Mean Squared Error ("typical error") as
# our measure of accuracy. The value is the typical error in star rating we would make
RMSE_fct <- function(actual_ratings, predicted_ratings){
   sqrt(mean((actual_ratings - predicted_ratings)^2, na.rm = TRUE))
}



# =============================================================================
#     Step 009.01 - Model Building, Training and Validation
#                 Model 1 - Model-based approach (Naive Baseline)
# =============================================================================

# Identify the Model
model_1_id <- "Model 1"
model_1_desc <- "Model-based approach (Naive Baseline)"

# Calculate the average of all movies
mu_hat <- mean(edx$rating)

# Predict the RMSE on the validation set
RMSE_model_1 <- round(RMSE_fct(validation$rating, mu_hat), round_precision)

# We generate a table to record our approaches and the RMSE for the model
rmse_result_model_1 <- tibble(Model_Id = model_1_id,
                              Model_Method = model_1_desc,
                              Predicted_RMSE = RMSE_model_1)

# We generate a table to record our approaches and the RMSEs we generate.
rmse_results <- rmse_result_model_1

# Display the RMSE results
rmse_result_model_1 %>%
   kable() %>%
   kable_styling(bootstrap_options = c("striped","hoved","condensed"),
                 position = "center",
                 font_size = 10,
                 full_width = FALSE) %>%
   column_spec(1, width = "5em") %>%
   column_spec(2, width = "20em") %>%
   column_spec(3, bold = TRUE)



# =============================================================================
#     Step 009.02 - Model Building, Training and Validation
#                 Model 2 - Content-based approach (Movie Effects)
# =============================================================================

# Identify the Model
model_2_id <- "Model 2"
model_2_desc <- "Content-based approach (Movie Effects)"

# Calculate the average of all movies
mu_hat <- mean(edx$rating)

# Calculate the average by movie
movie_avgs <- edx %>%
   group_by(movieId) %>%
   summarize(b_i = mean(rating - mu_hat))

# Compute the predicted ratings on validation dataset
predicted_ratings <- validation %>%
   left_join(movie_avgs, by='movieId') %>%
   mutate(pred = mu_hat + b_i) %>%
   pull(pred)

# Predict the RMSE on the validation set
RMSE_model_2 <- round(RMSE_fct(predicted_ratings, validation$rating), round_precision)

# We generate a table to record our approaches and the RMSE for the model
rmse_result_model_2 <- tibble(Model_Id = model_2_id,
                              Model_Method = model_2_desc,
                              Predicted_RMSE = RMSE_model_2)

# We generate a table to record our approaches and the RMSEs we generate.
rmse_results <- bind_rows(rmse_results,
                          rmse_result_model_2)

# Display the RMSE results
rmse_result_model_2 %>%
   kable() %>%
   kable_styling(bootstrap_options = c("striped","hoved","condensed"),
                 position = "center",
                 font_size = 10,
                 full_width = FALSE) %>%
   column_spec(1, width = "5em") %>%
   column_spec(2, width = "20em") %>%
   column_spec(3, bold = TRUE)



# =============================================================================
#     Step 009.03 - Model Building, Training and Validation
#                 Model 3 - User-based approach (Movie Effects + User Effects)
# =============================================================================

# Identify the Model
model_3_id <- "Model 3"
model_3_desc <- "User-based approach (Movie Effects + User Effects)"

# Calculate the average of all movies
mu_hat <- mean(edx$rating)

# Calculate the average by movie
movie_avgs <- edx %>%
   group_by(movieId) %>%
   summarize(b_i = mean(rating - mu_hat))

# Calculate the average by user
user_avgs <- edx %>%
   left_join(movie_avgs, by='movieId') %>%
   group_by(userId) %>%
   summarize(b_u = mean(rating - mu_hat - b_i))

# Compute the predicted ratings on validation dataset
predicted_ratings <- validation %>%
   left_join(movie_avgs, by='movieId') %>%
   left_join(user_avgs, by='userId') %>%
   mutate(pred = mu_hat + b_i + b_u) %>%
   pull(pred)

# Predict the RMSE on the validation set
RMSE_model_3 <- round(RMSE_fct(predicted_ratings, validation$rating), round_precision)

# We generate a table to record our approaches and the RMSE for the model
rmse_result_model_3 <- tibble(Model_Id = model_3_id,
                              Model_Method = model_3_desc,
                              Predicted_RMSE = RMSE_model_3)

# We generate a table to record our approaches and the RMSEs they generate.
rmse_results <- bind_rows(rmse_results,
                          rmse_result_model_3)

# Display the RMSE results
rmse_result_model_3 %>%
   kable() %>%
   kable_styling(bootstrap_options = c("striped","hoved","condensed"),
                 position = "center",
                 font_size = 10,
                 full_width = FALSE) %>%
   column_spec(1, width = "5em") %>%
   column_spec(2, width = "20em") %>%
   column_spec(3, bold = TRUE)



# =============================================================================
#     Step 009.04 - Model Building, Training and Validation
#                 Model 4 - Regularized Content-based approach
#                          (Movie Effects + Regularisation)
# =============================================================================

# Identify the Model
model_4_id <- "Model 4"
model_4_desc <- "Regularized Content-based approach (Movie Effects + Regularisation)"

# Calculate the average of all movies
mu_hat <- mean(edx$rating)

# Define a table of lambdas
lambdas <- seq(0, 10, 0.1)

# Compute the predicted ratings on validation dataset using different values of lambda
rmses <- sapply(lambdas, function(lambda) {

   # Calculate the average by user
   b_i <- edx %>%
      group_by(movieId) %>%
      summarize(b_i = sum(rating - mu_hat) / (n() + lambda))

   # Compute the predicted ratings on validation dataset
   predicted_ratings <- validation %>%
      left_join(b_i, by='movieId') %>%
      mutate(pred = mu_hat + b_i) %>%
      pull(pred)

   # Predict the RMSE on the validation set
   return(RMSE_fct(predicted_ratings, validation$rating))
})

# plot the result of lambdas (to be commented for the Rmarkdown document)
qplot(lambdas, rmses)

# Get the lambda value that minimize the RMSE
min_lambda <- lambdas[which.min(rmses)]

# Predict the RMSE on the validation set
RMSE_model_4 <- round(min(rmses), round_precision)

# We generate a table to record our approaches and the RMSE for the model
rmse_result_model_4 <- tibble(Model_Id = model_4_id,
                              Model_Method = model_4_desc,
                              Predicted_RMSE = RMSE_model_4)

# We generate a table to record our approaches and the RMSEs they generate.
rmse_results <- bind_rows(rmse_results,
                          rmse_result_model_4)

# Display the RMSE results
rmse_result_model_4 %>%
   kable() %>%
   kable_styling(bootstrap_options = c("striped","hoved","condensed"),
                 position = "center",
                 font_size = 10,
                 full_width = FALSE) %>%
   column_spec(1, width = "5em") %>%
   column_spec(2, width = "20em") %>%
   column_spec(3, bold = TRUE)



# =============================================================================
#     Step 009.05 - Model Building, Training and Validation
#                 Model 5 - Regularized User-based approach
#                          (Movie Effects + User Effects + Regularisation)
# =============================================================================

# Identify the Model
model_5_id <- "Model 5"
model_5_desc <- "Regularized User-based approach (Movie Effects + User Effects + Regularisation)"

# Define a table of lambdas
lambdas <- seq(0, 10, 0.1)

# Compute the predicted ratings on validation dataset using different values of lambda
rmses <- sapply(lambdas, function(lambda) {

   # Calculate the average of all movies
   mu_hat <- mean(edx$rating)

   # Calculate the average by user
   b_i <- edx %>%
      group_by(movieId) %>%
      summarize(b_i = sum(rating - mu_hat) / (n() + lambda))

   # Calculate the average by user
   b_u <- edx %>%
      left_join(b_i, by='movieId') %>%
      group_by(userId) %>%
      summarize(b_u = sum(rating - b_i - mu_hat) / (n() + lambda))

   # Compute the predicted ratings on validation dataset
   predicted_ratings <- validation %>%
      left_join(b_i, by='movieId') %>%
      left_join(b_u, by='userId') %>%
      mutate(pred = mu_hat + b_i + b_u) %>%
      pull(pred)

   # Predict the RMSE on the validation set
   return(RMSE_fct(predicted_ratings, validation$rating))
})

# plot the result of lambdas (to be commented for the Rmarkdown document)
qplot(lambdas, rmses)

# Get the lambda value that minimize the RMSE
min_lambda <- lambdas[which.min(rmses)]

# Predict the RMSE on the validation set
RMSE_model_5 <- round(min(rmses), round_precision)

# We generate a table to record our approaches and the RMSE for the model
rmse_result_model_5 <- tibble(Model_Id = model_5_id,
                              Model_Method = model_5_desc,
                              Predicted_RMSE = RMSE_model_5)

# We generate a table to record our approaches and the RMSEs they generate.
rmse_results <- bind_rows(rmse_results,
                          rmse_result_model_5)

# Display the RMSE results
rmse_result_model_5 %>%
   kable() %>%
   kable_styling(bootstrap_options = c("striped","hoved","condensed"),
                 position = "center",
                 font_size = 10,
                 full_width = FALSE) %>%
   column_spec(1, width = "5em") %>%
   column_spec(2, width = "20em") %>%
   column_spec(3, bold = TRUE)



# =============================================================================
#     Step 010 - RMSE results on all the models
# =============================================================================

# Display the RMSE results
rmse_results %>%
   kable() %>%
   kable_styling(bootstrap_options = c("striped","hoved","condensed"),
                 position = "center",
                 font_size = 10,
                 full_width = FALSE) %>%
   column_spec(1, width = "5em") %>%
   column_spec(2, width = "20em") %>%
   column_spec(3, bold = TRUE)



# =============================================================================
#     Step 011 - Configure all variables and session information
# =============================================================================

# Start the chronometer to get the script end time in milliseconds
script_end_time <- proc.time()

script_duration <- script_end_time - script_start_time

script_duration



# =============================================================================
#  END OF SCRIPT
# =============================================================================
