###---Intro to cross-validation with modelR

# Useful libraries
library(modelr); library(tidyverse)

# Useful function for fitting a model
fit_model <- function(resample_obj, formula){
  data <- resample_obj %>% as_tibble() # get a tibble from the resampled object
  fit <- lm(formula, data)
    
  return(fit)
}

# example: fit_model(cy_train, "Votepts ~ ERA") # What do you think this is doing?

# Load data
#
setwd("./data")
cy <- read_csv("cy-young-pitcher-data-2005-2015.csv")

cy_sub <- cy %>%
  select(Name, Team, Year, Votepts, ERA, WHIP, K9, W, IP, SV) %>%
  mutate(closer = as.integer(SV > 5)) %>%
  filter(IP > 45, Votepts > 0)


# Separate into train/test sets
set.seed(1234) # Set the random number generator for R

# Get a random sample of the data for our training set
cy_train <- cy_sub %>%
  sample_frac(0.75)

# Get the test set
cy_test <- cy_sub %>%
  setdiff(cy_train)


# Split the training data into 5 folds
train_kfold <- cy_train %>%
  crossv_kfold(5, id = "fold")

# Fit the model holding out each fold, and then test it on the held out fold
train_kfold_err <- train_kfold %>%
  mutate(model_fit = map2(train, "Votepts ~ ERA + WHIP", fit_model), # fit your model to each fold
         fold_err = map2(model_fit, test, mse)) # get the fold errors

# Average the fold errors and take the square rood
train_kfold_err %>%
  summarize(mean_err = sqrt(mean(unlist(fold_err))))


###---Your turn: Amend lines 44-50 so that you fit a model that includes Wins, K9, WHIP, ERA, and ERA^2 as predictors.

train_kfold_err <- train_kfold %>%
  mutate(model_fit = map2(train, "Votepts ~ ERA + WHIP + W + K9 + ERA^2", fit_model), # fit your model to each fold
         fold_err = map2(model_fit, test, mse)) # get the fold errors

# Average the fold errors and take the square rood
train_kfold_err %>%
  summarize(mean_err = sqrt(mean(unlist(fold_err))))