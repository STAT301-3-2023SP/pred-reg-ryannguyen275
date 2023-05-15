# Load package(s) ----
library(tidymodels)
library(tidyverse)
library(naniar)
library(doMC)
library(vip)

registerDoMC(cores = 4)

# handle common conflicts
tidymodels_prefer()

# Seed
set.seed(3013)

########## load in data ######################################################
train <- read.csv("data/train.csv")
test <- read.csv("data/test.csv")

load(file = "data/train_data.rda")

folds <- vfold_cv(train_data, v = 5, repeats = 3)
save(folds, file = "data/folds.rda")

# picking variables
# correlations, remove too correlated variables from recipe
# lasso, use default parameters 

# model refinement: refold data and redo folds 

ggplot(train_data, aes(x = y)) +
  geom_histogram()

ggplot(train_data, aes(x = log(y))) +
  geom_histogram()

ggplot(train_data, aes(x = log(y))) +
  geom_histogram()

miss_table <- miss_var_summary(train) # none over 20 percent

############## initial recipe for lasso/rf selection ############################
init_recipe <- recipe(y ~., data = train_data) %>%
  step_rm(id) %>%
  step_nzv(all_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_impute_mean(all_predictors())

init_recipe %>% 
  prep() %>% 
  bake(new_data = NULL)

############## variable selection using lasso ###################################
lasso_mod <- linear_reg(mode = "regression",
                        penalty = tune(),
                        mixture = 1) %>% 
  set_engine("glmnet")

lasso_params <- extract_parameter_set_dials(lasso_mod)
lasso_grid <- grid_regular(lasso_params, levels = 5)

lasso_workflow <- workflow() %>% 
  add_model(lasso_mod) %>% 
  add_recipe(init_recipe)

lasso_tune <- lasso_workflow %>% 
  tune_grid(resamples = folds,
            grid = lasso_grid)

lasso_wkflw_final <- lasso_workflow %>% 
  finalize_workflow(select_best(lasso_tune, metric = "rmse"))

lasso_fit <- fit(lasso_wkflw_final, data = train_data)

lasso_tidy <- lasso_fit %>% 
  tidy() %>%
  filter(estimate != 0)

View(lasso_tidy)

save(lasso_tidy, file = "data/lasso_variables3.rda")

load("data/lasso_variables3.rda")


############## variable selection using random forest #############################]

rf_mod <- rand_forest(mode = "regression",
                      mtry = tune()) %>% 
  set_engine("ranger", importance = "impurity")

rf_params <- extract_parameter_set_dials(rf_mod) %>% 
  recipes::update(mtry = mtry(range = c(1,5)))

rf_grid <- grid_regular(rf_params, levels = 5)

rf_workflow <- workflow() %>% 
  add_model(rf_mod) %>% 
  add_recipe(init_recipe)

rf_tune <- rf_workflow %>% 
  tune_grid(resamples = folds,
            grid = rf_grid)

rf_wkflw_final <- rf_workflow %>% 
  finalize_workflow(select_best(rf_tune, metric = "rmse"))

rf_fit <- fit(rf_wkflw_final, data = train_data)

save(rf_fit, file = "data/rf_variables.rda")

load("data/rf_variables.rda")

rf_vip <- rf_fit %>% 
  extract_fit_parsnip %>% 
  vip()

save(rf_vip, file = "data/rf_vip2.rda")

# Rf Variables 1
# x105, x702, x102, x561, x014, x096, x696, x420, x569, x366

# Rf Variables 2
# x146, x355, x014, x755, x687, x102, x096, x420, x591, x619

lasso_tidy %>%
  pull(term)

# lasso
# x014, x017, x105, x146, x186, x477, x561, x567, x568, x581, x619, x622, x740, x750

# All variables for selection
# x105, x702, x561, x696, x569, x366, x146, x355, x014, x755, x687, x102, x096, 
# x420, x591, x619, x017, x186, x477, x567, x568, x581, x622, x740, x750
