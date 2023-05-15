# Model Results
library(tidyverse)
library(tidymodels)
library(kableExtra)
library(vip)
library(doMC)
library(parallel)

tidymodels_prefer()

registerDoMC(cores = 4)

##### LOAD PACKAGES/DATA ##############################################

load("results/rf_tuned3.rda")
test <- read.csv("data/test.csv")
train <- read.csv("data/train.csv")

rf_workflow <- rf_workflow %>% 
  finalize_workflow(select_best(rf_tuned, metric = "rmse"))
 
##### FINAL FIT ######################################################
final_fit <- fit(rf_workflow, train)

predictions <- predict(final_fit, test) %>% 
  bind_cols(test %>% select(id)) %>% 
  rename(y = .pred)

write_csv(predictions, file = "submissions/18_submission.csv")

