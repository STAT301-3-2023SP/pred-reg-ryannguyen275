# Single Layer Neural Network Tuning

##### LOAD PACKAGES/DATA ##############################################

library(tidymodels)
library(tidyverse)
library(stacks)

library(doMC)
registerDoMC(cores = 4)

tidymodels_prefer ()

load("attempt_2/setups/setup_1.rda")

##### DEFINE ENGINES/WORKFLOWS #########################################
nn_model <- mlp(mode = "regression",
                hidden_units = tune(),
                penalty = tune()) %>%
  set_engine("nnet")

nn_param <- extract_parameter_set_dials(nn_model)

nn_grid <- grid_regular(nn_param, levels = 5)

nn_workflow <- workflow() %>% 
  add_model(nn_model) %>% 
  add_recipe(recipe1)

##### TUNE GRID ########################################################
nn_tuned <- tune_grid(nn_workflow,
                      resamples = folds,
                      grid = nn_grid,
                      control = control = control_stack_resamples(),
                      metrics = metric_set(rmse))

save(nn_tuned, nn_workflow, file = "attempt_4/results/nn_tuned.rda")
