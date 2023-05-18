# K-Nearest Neighbors Tuning

##### LOAD PACKAGES/DATA ##############################################

library(tidymodels)
library(tidyverse)
library(stacks)

library(doMC)
registerDoMC(cores = 4)

load("attempt_4/setups/setup_1.rda")

##### DEFINE ENGINES/WORKFLOWS #########################################
knn_model <- nearest_neighbor(mode = "regression",
                              neighbors = tune()) %>% 
  set_engine("kknn")

knn_param <- extract_parameter_set_dials(knn_model)

knn_grid <- grid_regular(knn_param, levels = 5)

knn_workflow <- workflow() %>% 
  add_model(knn_model) %>% 
  add_recipe(recipe1)

##### TUNE GRID ########################################################
knn_tuned <- tune_grid(knn_workflow,
                       resamples = folds,
                       verbose = TRUE,
                       grid = knn_grid,
                       control = control_stack_resamples(),
                       metrics = metric_set(rmse))

save(knn_tuned, knn_workflow, file = "attempt_4/results/knn_tuned.rda")

