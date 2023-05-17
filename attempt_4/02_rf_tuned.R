
##### LOAD PACKAGES/DATA ##############################################

library(tidymodels)
library(tidyverse)
library(stacks)

library(doMC)
registerDoMC(cores = 4)

tidymodels_prefer ()

load("attempt_4/setups/setup_1.rda")

##### DEFINE ENGINES/WORKFLOWS #########################################
rf_model <- rand_forest(min_n = tune(),
                        mtry = tune()) %>% 
  set_engine("ranger", importance = "impurity") %>% 
  set_mode("regression")

rf_param <- extract_parameter_set_dials(rf_model) %>% 
  update(mtry = mtry(range = c(8,18)))

rf_grid <- grid_regular(rf_param, levels = 5)

rf_workflow <- workflow() %>% 
  add_model(rf_model) %>% 
  add_recipe(recipe1)

##### TUNE GRID ########################################################
rf_tuned <- tune_grid(rf_workflow,
                      resamples = folds,
                      grid = rf_grid,
                      verbose = TRUE,
                      control = control_stack_resamples(),
                      metrics = metric_set(rmse))


save(rf_tuned, rf_workflow, file = "attempt_4/results/rf_tuned.rda")
