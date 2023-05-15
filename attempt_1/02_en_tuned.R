# Elastic Net Tuning

##### LOAD PACKAGES/DATA ##############################################

library(tidymodels)
library(tidyverse)
library(tictoc)

library(doMC)
registerDoMC(cores = 4)

tidymodels_prefer ()

load("data/setup5.rda")

##### DEFINE ENGINES/WORKFLOWS #########################################
en_model <- linear_reg(mode = "regression",
                         penalty = tune(), 
                         mixture = tune()) %>% 
  set_engine("glmnet")

en_param <- extract_parameter_set_dials(en_model)

en_grid <- grid_regular(en_param, levels = 5)

en_workflow <- workflow() %>% 
  add_model(en_model) %>% 
  add_recipe(recipe6)

##### TUNE GRID ########################################################

en_tuned <- tune_grid(en_workflow,
                      resamples = folds1,
                      grid = en_grid,
                      verbose = TRUE,
                      control = control_grid(save_pred = TRUE, 
                                             save_workflow = TRUE,
                                             verbose = TRUE,
                                             parallel_over = "everything"))

save(en_tuned, en_workflow, file = "results/en_tuned4.rda")

