# SVM Radial Tuning

##### LOAD PACKAGES/DATA ##############################################

library(tidymodels)
library(tidyverse)
library(tictoc)

library(doMC)
registerDoMC(cores = 4)

tidymodels_prefer ()

load("attempt_2/setups/setup_1.rda")

##### DEFINE ENGINES/WORKFLOWS #########################################
svm_radial_model <- svm_rbf(mode = "regression",
                            cost = tune(),
                            rbf_sigma = tune()) %>%
  set_engine("kernlab")

svm_radial_param <- extract_parameter_set_dials(svm_radial_model)

svm_radial_grid <- grid_regular(svm_radial_param, levels = 5)

svm_radial_workflow <- workflow() %>% 
  add_model(svm_radial_model) %>% 
  add_recipe(recipe1)

##### TUNE GRID ########################################################
svm_radial_tuned <- tune_grid(svm_radial_workflow,
                              resamples = folds,
                              grid = svm_radial_grid,
                              verbose = TRUE,
                              control = control_grid(save_pred = TRUE, 
                                                     save_workflow = TRUE,
                                                     verbose = TRUE,
                                                     parallel_over = "everything"))


save(svm_radial_tuned, svm_radial_workflow, file = "attempt_2/results/svm_radial_tuned.rda")