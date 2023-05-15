# SVM Polynomial Tuning

##### LOAD PACKAGES/DATA ##############################################

library(tidymodels)
library(tidyverse)
library(tictoc)

library(doMC)
registerDoMC(cores = 4)

tidymodels_prefer ()

load("data/setup.rda")

##### DEFINE ENGINES/WORKFLOWS #########################################
svm_poly_model <- svm_poly(mode = "regression",
                           cost = tune(),
                           degree = tune(),
                           scale_factor = tune()) %>%
  set_engine("kernlab")

svm_poly_param <- extract_parameter_set_dials(svm_poly_model)

svm_poly_grid <- grid_regular(svm_poly_param, levels = 5)

svm_poly_workflow <- workflow() %>% 
  add_model(svm_poly_model) %>% 
  add_recipe(l_recipe1)

##### TUNE GRID ########################################################
svm_poly_tuned <- tune_grid(svm_poly_workflow,
                            resamples = folds,
                            grid = svm_poly_grid,
                            verbose = TRUE,
                            control = control_grid(save_pred = TRUE, 
                                                   save_workflow = TRUE,
                                                   verbose = TRUE,
                                                   parallel_over = "everything"))


save(svm_poly_tuned, svm_poly_workflow, file = "results/svm_poly_tuned.rda")

