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
load("data/folds.rda")
load("data/train_data.rda")

########## set up recipes #####################################################

# RECIPE 1: Kitchen Sink
recipe1 <- recipe(y ~ x105+ x702+ x561+ x696+ x569+ x366+ x146+ x355+ x014+ 
                      x755+ x687+ x102+ x096+ x420+ x591+ x619+ x017+ x186+ 
                      x477+ x567+ x568+ x581+ x622+ x740+ x750, data = train_data) %>%
  step_YeoJohnson(all_predictors()) %>% 
  step_nzv(all_numeric_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_impute_bag(all_numeric_predictors()) %>% 
  step_corr(all_predictors())

recipe1 %>% 
  prep() %>% 
  bake(new_data = NULL)

save(recipe1, folds, train_data, test_data, file = "attempt_4/setups/setup_1.rda")

length(select_if(train_data,is.numeric))
