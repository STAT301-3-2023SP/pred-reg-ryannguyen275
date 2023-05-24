# Model Results
library(tidyverse)
library(tidymodels)

tidymodels_prefer()

registerDoMC(cores = 4)

##### LOAD PACKAGES/DATA ##############################################
load("attempt_2/setups/setup_1.rda")
load("data/train.rda")
test <- read_csv("data/test.csv")

result_files <- list.files("attempt_1/results", "*.rda", full.names = TRUE)

for(i in result_files) {
  load(i)
}

load("attempt_2/results/_tuned.rda")
load("attempt_2/results/svm_radial_tuned.rda")
load("attempt_2/results/nn_tuned.rda")
load("attempt_2/results/model_results.rda")

nn_tuned %>% 
  show_best(metric = "rmse")

####### PUT ALL GRIDS TG ############################################################
model_set <- as_workflow_set(
  "elastic_net" = en_tuned,
  "rand_forest" = rf_tuned, 
  "knn" = knn_tuned,
  "boosted_tree" = bt_tuned,
  "nn" = nn_tuned,
  "svm_poly" = svm_poly_tuned,
  "svm_radial" = svm_radial_tuned,
  "mars" = mars_tuned
)

## Table of results
model_results <- model_set %>% 
  group_by(wflow_id) %>% 
  mutate(best = map(result, show_best, metric = "roc_auc", n = 1)) %>% 
  select(best) %>% 
  unnest(cols = c(best))

save(model_results, file = "attempt_1/results/model_results.rda")
load("attempt_2/results/svm_poly_tuned.rda")
load("attempt_2/results/model_results.rda")
 
##### FINAL FIT ######################################################
nn_workflow <- nn_workflow %>% 
  finalize_workflow(select_best(nn_tuned, metric = "roc_auc"))

final_fit <- fit(nn_workflow, train)

predictions <- predict(final_fit, test) %>% 
  bind_cols(test %>% select(id)) %>% 
  rename(y = .pred_class)

write_csv(predictions, file = "submissions/submission_1.csv")

