library(tidyverse)
library(skimr)

# setwd("~/Desktop/Sabermetrics independint study/data")
# setwd("./data")
cy <- read_csv("~/Desktop/Sabermetrics independint study/data/cy-young-pitcher-data-2005-2015.csv")

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