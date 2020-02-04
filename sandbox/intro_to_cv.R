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
  select(Name, Team, Year, Votepts, ERA, WHIP, K9, W, IP, SV, WPA, FIP) %>%
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


train_kfold_err <- train_kfold %>%
  mutate(model_fit_1 = map2(train, "Votepts ~ ERA + WHIP + W + K9 + ERA^2", fit_model), 
         model_fit_2 = map2(train, "Votepts ~ ERA + WHIP + W + K9 + ERA^2 + closer:ERA", fit_model), 
         model_fit_3 = map2(train, "Votepts ~ ERA + WHIP + W + K9 + ERA^2 + WHIP^2 + WHIP^3 + SV:closer", fit_model), 
         model_fit_4 = map2(train, "Votepts ~ poly(ERA, 3) + poly(WHIP, 2) + poly(W, 2) + poly(K9, 2) + closer", fit_model),
         model_fit_5 = map2(train, "Votepts ~ poly(ERA, 3) + poly(WHIP, 1) + poly(WPA, 1) + poly(FIP, 1) + K9 + W + closer", fit_model),
         fold_err_1 = map2(model_fit_1, test, mse), 
         fold_err_2 = map2(model_fit_2, test, mse), 
         fold_err_3 = map2(model_fit_3, test, mse), 
         fold_err_4 = map2(model_fit_4, test, mse), 
         fold_err_5 = map2(model_fit_5, test, mse)) %>%
  summarize(err_1 = sqrt(mean(unlist(fold_err_1))), 
            err_2 = sqrt(mean(unlist(fold_err_2))), 
            err_3 = sqrt(mean(unlist(fold_err_3))), 
            err_4 = sqrt(mean(unlist(fold_err_4))), 
            err_5 = sqrt(mean(unlist(fold_err_5))))
train_kfold_err



