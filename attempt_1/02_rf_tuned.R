
##### LOAD PACKAGES/DATA ##############################################

library(tidymodels)
library(tidyverse)
library(tictoc)

library(doMC)
registerDoMC(cores = 4)

tidymodels_prefer ()

load("data/setup8.rda")

##### DEFINE ENGINES/WORKFLOWS #########################################
rf_model <- rand_forest(min_n = tune(),
                        mtry = tune()) %>% 
  set_engine("ranger", importance = "impurity") %>% 
  set_mode("regression")

rf_param <- extract_parameter_set_dials(rf_model) %>% 
  update(mtry = mtry(range = c(1,10)))

rf_grid <- grid_regular(rf_param, levels = 5)

rf_workflow <- workflow() %>% 
  add_model(rf_model) %>% 
  add_recipe(recipe9)

##### TUNE GRID ########################################################
rf_tuned <- tune_grid(rf_workflow,
                      resamples = folds,
                      grid = rf_grid,
                      verbose = TRUE,
                      control = control_grid(save_pred = TRUE, 
                                             save_workflow = TRUE,
                                             verbose = TRUE,
                                             parallel_over = "everything"))


save(rf_tuned, rf_workflow, file = "results/rf_tuned3.rda")
