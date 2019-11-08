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

get_lm_test_rmse <- function(model, new_data, outcome="Votepts"){
  # outcome = "Votepts"
  
  # make predictions with your model on the new_data
  pred_values <- predict(model, new_data)
  
  # compare those predictions to the actual values
  return(sqrt(mean((new_data[[outcome]] - pred_values)^2)))
}

get_lm_test_rmse(model=mod3, new_data=cy_test, outcome="Votepts")
get_lm_test_rmse(model=mod3, new_data=cy_test)

###--------------------------------------------###
### Exercise 2:
### Fit a model with all predictors except
### team, year, and name
### Interpret the parameter estimates (a.k.a. the regression coefficients)
### Interpret the training error
### Get the test error
mod_all = lm(Votepts ~ ERA + WHIP + K9 + W + IP + SV + closer, data = cy_train)

pred_vals = predict(mod_all, cy_test)
sqrt(mean((cy_test$Votepts - pred_vals)^2))

get_lm_test_rmse(mod_all, cy_test)

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

