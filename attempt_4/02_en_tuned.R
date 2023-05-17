# Elastic Net Tuning

##### LOAD PACKAGES/DATA ##############################################

library(tidymodels)
library(tidyverse)
library(stacks)

library(doMC)
registerDoMC(cores = 4)

tidymodels_prefer()

load("attempt_4/setups/setup_1.rda")

##### DEFINE ENGINES/WORKFLOWS #########################################
en_model <- linear_reg(mode = "regression",
                         penalty = tune(), 
                         mixture = tune()) %>% 
  set_engine("glmnet")

en_param <- extract_parameter_set_dials(en_model)

en_grid <- grid_regular(en_param, levels = 7)

en_workflow <- workflow() %>% 
  add_model(en_model) %>% 
  add_recipe(recipe1)

##### TUNE GRID ########################################################

en_tuned <- tune_grid(en_workflow,
                      resamples = folds,
                      grid = en_grid,
                      verbose = TRUE,
                      control = control_stack_resamples(),
                      metrics = metric_set(rmse))

save(en_tuned, en_workflow, file = "attempt_4/results/en_tuned.rda")

