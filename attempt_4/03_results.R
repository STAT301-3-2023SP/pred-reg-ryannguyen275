# Model Results
library(tidyverse)
library(tidymodels)
library(stacks)

tidymodels_prefer()

##### LOAD PACKAGES/DATA ##############################################
load("attempt_4/setups/setup_1.rda")
test <- read_csv("data/test.csv")

load("attempt_4/results/en_tuned.rda")
load("attempt_4/results/svm_radial_tuned.rda")
load("attempt_4/results/svm_poly_tuned.rda")


data_stacks <- stacks() %>%
  add_candidates(rf_tuned) %>%
  add_candidates(svm_radial_tuned) %>% 
  add_candidates(svm_poly_tuned)

as_tibble(data_stacks)

data_model <- data_stacks %>% 
  blend_predictions(penalty = 10^(-6:-1))

data_model <- data_model %>% 
  fit_members()

save(data_model, file = "attempt_3/results/data_model.rda")
 
##### FINAL FIT ######################################################

predictions <- test %>%
  bind_cols(predict(data_model, .)) %>% 
  select(id, .pred) %>% 
  rename(y = .pred)

write_csv(predictions, file = "submissions/22_submission.csv")

