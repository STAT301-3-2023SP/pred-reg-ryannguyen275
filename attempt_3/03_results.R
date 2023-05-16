# Model Results
library(tidyverse)
library(tidymodels)

tidymodels_prefer()

registerDoMC(cores = 4)

##### LOAD PACKAGES/DATA ##############################################
load("attempt_3/setups/setup_1.rda")
test <- read_csv("data/test.csv")

load("attempt_3/results/rf_tuned.rda")
load("attempt_3/results/svm_radial_tuned.rda")
load("attempt_3/results/svm_poly_tuned.rda")

data_stacks <- stacks() %>%
  add_candidates(rf_tuned) %>% # 15 models
  add_candidates(svm_radial_tuned) %>% # 1 model
  add_candidates(svm_poly_tuned)

as_tibble(data_stacks)
 
##### FINAL FIT ######################################################
svm_poly_workflow <- svm_poly_workflow %>% 
  finalize_workflow(select_best(svm_poly_tuned, metric = "rmse"))

final_fit <- fit(svm_poly_workflow, train_data)

predictions <- predict(final_fit, test) %>% 
  bind_cols(test %>% select(id)) %>% 
  rename(y = .pred)

write_csv(predictions, file = "submissions/21_submission.csv")

