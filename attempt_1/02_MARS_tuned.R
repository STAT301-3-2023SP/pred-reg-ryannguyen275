# MARS Tuning

##### LOAD PACKAGES/DATA ##############################################

library(tidymodels)
library(tidyverse)
library(tictoc)

library(doMC)
registerDoMC(cores = 4)

tidymodels_prefer ()

load("data/setup.rda")

##### DEFINE ENGINES/WORKFLOWS #########################################
mars_model <- mars(mode = "regression",
                   num_terms = tune(),
                   prod_degree = tune()) %>%
  set_engine("earth")

mars_param <- extract_parameter_set_dials(mars_model) %>% 
  update(num_terms = num_terms(range = c(1, 10)))

mars_grid <- grid_regular(mars_param, levels = 5)

mars_workflow <- workflow() %>% 
  add_model(mars_model) %>% 
  add_recipe(l_recipe1)

##### TUNE GRID #######################################################
mars_tuned <- tune_grid(mars_workflow,
                        resamples = folds,
                        grid = mars_grid,
                        verbose = TRUE,
                        control = control_grid(save_pred = TRUE, # create extra column for each prediction
                                               save_workflow = TRUE, # lets you use extract_workflow
                                               verbose = TRUE,
                                               parallel_over = "everything"),
                        # metrics = metric_set())
)

save(mars_tuned, mars_workflow, file = "results/mars_tuned.rda")

