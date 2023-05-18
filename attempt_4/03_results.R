# Model Results
library(tidyverse)
library(tidymodels)
library(stacks)

tidymodels_prefer()

##### LOAD PACKAGES/DATA ##############################################
load("attempt_4/setups/setup_1.rda")
load("data/train_data.rda")
test <- read_csv("data/test.csv")

load("attempt_4/results/en_tuned.rda")
load("attempt_4/results/svm_radial_tuned.rda")
load("attempt_4/results/svm_poly_tuned.rda")
load("attempt_4/results/bt_tuned.rda")
load("attempt_4/results/rf_tuned.rda")
load("attempt_4/results/nn_tuned.rda")


data_stacks <- stacks() %>%
  add_candidates(bt_tuned) %>%
  add_candidates(en_tuned) %>% 
  add_candidates(nn_tuned) %>% 
  add_candidates(rf_tuned) %>%
  add_candidates(svm_radial_tuned) %>% 
  add_candidates(svm_poly_tuned)

save(data_stacks, file = "attempt_4/results/data_stacks.rda")

as_tibble(data_stacks)

data_model <- data_stacks %>% 
  blend_predictions(penalty = 10^(-6:-1))

data_model <- data_model %>% 
  fit_members()

load("attempt_4/results/data_stacks.rda")
 
##### FINAL FIT ######################################################

predictions <- test %>%
  bind_cols(predict(data_model, .)) %>% 
  select(id, .pred) %>% 
  rename(y = .pred)

write_csv(predictions, file = "submissions/23_submission.csv")

