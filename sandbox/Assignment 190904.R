library(tidyverse)
library(skimr)

cy_tmp <- readRDS("./sandbox/cy_sub_train-test.RDS")
cy_train <- cy_tmp$train
cy_test <- cy_tmp$test

###--------------------------------------------###
### Exercise 1:
### Write a function to get the test error for
### a linear regression model
### Hint: see get_lm_rmse_cy function


###--------------------------------------------###
### Exercise 2:
### Fit a model with all predictors except
### team, year, and name
### Interpret the parameter estimates (a.k.a. the regression coefficients)
### Interpret the training error
### Get the test error


###--------------------------------------------###
### Exercise 3:
### Fit a model with all predictors except
### team, year, and name, but include the following interaction terms:
###   W * closer
###   SV * closer
###   WHIP * closer
###   ERA * closer
###   IP * closer
### Interpret the parameter estimates (a.k.a. the regression coefficients)
### Interpret the training error
### Get the test error




###--------------------------------------------###
### Exercise 4:
### Compare the models in EX 3-4. Which is better?
### How might you improve the model in EX 4?

