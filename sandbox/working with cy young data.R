library(tidyverse)
library(skimr)

setwd("~/Desktop/Sabermetrics independint study/data")
setwd("./data")
cy <- read_csv("cy-young-pitcher-data-2005-2015.csv")
# cy <- read_csv("~/Desktop/Sabermetrics independnt study/data/cy-young-pitcher-data-2005-2015.csv")

head(cy)
sort(names(cy))

skim(cy)
as.numeric(gsub(" %", "", cy$BBrate))/100

cy <- cy %>%
  mutate(BBrate = as.numeric(gsub(" %", "", BBrate))/100)
skim(cy)

cy_sub <- cy %>%
  select(Name, Team, Year, Votepts, ERA, WHIP, K9, W, IP, SV) %>%
  mutate(closer = as.integer(SV > 5)) %>%
  filter(IP > 45, Votepts > 0)

skim(cy_sub)

### Next week: Basic Prediction Models
set.seed(1234) # Set the random number generator for R

# Get a random sample of the data for our training set
cy_train <- cy_sub %>%
  sample_frac(0.75)

# Get the test set
cy_test <- cy_sub %>%
  setdiff(cy_train)


# Fit our first model
mod1 <- lm(Votepts ~ ERA, data = cy_train)
mod1
summary(mod1)

# Function for getting RMSE
get_lm_rmse_cy <- function(model, test_data){
  test_pred <- predict(model, test_data)
  return(sqrt(mean((test_pred - test_data$Votepts)^2)))
}

# Get the predicted values on the test set
get_lm_rmse_cy(mod1, cy_test)


# Fit our second model
mod2 <- lm(Votepts ~ ERA + WHIP + K9 + W + IP + SV + closer, data = cy_train)
mod2
summary(mod2)

# Get the predicted values on the test set
get_lm_rmse_cy(mod2, cy_test)


# Fit our third model
mod3 <- lm(Votepts ~ ERA + WHIP + K9 + W + IP + SV, data = cy_train)
mod3
summary(mod3)

get_lm_rmse_cy(mod3, cy_test)


# Quadratic expansion
mod4 <- lm(Votepts ~ ERA + I(ERA^2), data = cy_train)


# Simple model
mod5 <- lm(Votepts ~ ERA, data = cy_train)
summary(mod5)
summary(mod4)

get_lm_rmse_cy(mod4, cy_test)
get_lm_rmse_cy(mod5, cy_test)


mod6 <- lm(Votepts ~ WHIP + closer, data = cy_train)
mod7 <- lm(Votepts ~ WHIP + closer + WHIP*closer, data = cy_train)
summary(mod6)
summary(mod7)

get_lm_rmse_cy(mod6, cy_test)
get_lm_rmse_cy(mod7, cy_test)

saveRDS(list(train = cy_train, test = cy_test), 
        "./sandbox/cy_sub_train-test.RDS")
