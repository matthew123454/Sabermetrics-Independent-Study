###---This code is to introduce you to regression trees

# Useful libraries
library(rpart); library(tidyverse)


# Load data
setwd("~/Desktop/Sabermetrics independint study/data")
setwd("./data")
cy <- read_csv("cy-young-pitcher-data-2005-2015.csv")

cy_sub <- cy %>%
  select(Name, Team, Year, Votepts, ERA, WHIP, K9, W, IP, SV) %>%
  mutate(closer = as.integer(SV > 5)) %>%
  filter(IP > 45, Votepts > 0)


### Separate into train/test sets
set.seed(1234) # Set the random number generator for R

# Get a random sample of the data for our training set
cy_train <- cy_sub %>%
  sample_frac(0.75)

# Get the test set
cy_test <- cy_sub %>%
  setdiff(cy_train)

# Function for getting RMSE
get_test_rmse_cy <- function(model, test_data){
  test_pred <- predict(model, test_data)
  return(sqrt(mean((test_pred - test_data$Votepts)^2)))
}


# Fit a tree with only ERA and WHIP as predictors
tree1 <- rpart(Votepts ~ ERA + WHIP, cy_train)

# Plot the tree
plot(tree1)

# Get test error
get_test_rmse_cy(tree1, cy_test)


###---Your turn: Fit a tree with ERA, WHIP, Wins, and K9 as predictors.
###---           Plot that tree, and get the test RMSE