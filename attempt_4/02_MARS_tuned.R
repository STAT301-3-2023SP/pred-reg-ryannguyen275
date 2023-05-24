# MARS Tuning

##### LOAD PACKAGES/DATA ##############################################

library(tidymodels)
library(tidyverse)
library(stacks)

library(doMC)
registerDoMC(cores = 4)

tidymodels_prefer()

load("attempt_4/setups/setup_1.rda")

##### DEFINE ENGINES/WORKFLOWS #########################################
mars_model <- mars(mode = "regression",
                   num_terms = tune(),
                   prod_degree = tune()) %>%
  set_engine("earth")

mars_param <- extract_parameter_set_dials(mars_model) %>% 
  update(num_terms = num_terms(range = c(8, 18)))

mars_grid <- grid_regular(mars_param, levels = 5)

mars_workflow <- workflow() %>% 
  add_model(mars_model) %>% 
  add_recipe(recipe1)

##### TUNE GRID #######################################################
mars_tuned <- tune_grid(mars_workflow,
                        resamples = folds,
                        grid = mars_grid,
                        verbose = TRUE,
                        control = control_stack_resamples(),
                        metrics = metric_set(rmse)
)

save(mars_tuned, mars_workflow, file = "attempt_4/results/mars_tuned.rda")

