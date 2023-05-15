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
# from class
read_rds("./data/train_data.rds")

train_log <- train %>% 
  mutate(log_y = log(y))

train_lasso <- read.csv("data/train.csv") %>% 
  select(c(y, x014, x017, x102, x105, x108, x146, x186, x477, 
           x561, x567, x568, x581, x619, x622, x740, x750))

train_rf <- read.csv("data/train.csv") %>% 
  select(c(y, x014, x146, x619, x102, x355, x755, x105, x670, x687, x721))

train_all <- train %>% 
  select(c(y, x014, x017 , x102 , x105 , x108 , x146 , x186 , x477 , 
             x561 , x567 , x568 , x581 , x619 , x622 , x740 , x750 , 
             x355 , x755 ,x670 ,x687 , x721))

corrplot::corrplot(cor(train_all), method = 'color')

train_some <- train %>% 
  select(c(y, x014, x102 , x105, x146, 
           x561 , x567, x619, x750 , 
           x355 , x755 ,x670 ,x687 , x721))

corrplot::corrplot(cor(train_some), method = 'color')

  
test <- read.csv("data/test.csv")

folds <- vfold_cv(train, v = 5, repeats = 3, strata = y)
folds1 <- vfold_cv(train1, v = 5, repeats = 3, strata = y)


########## set up recipes #####################################################

# RECIPE 1: Lasso Kitchen Sink
l_recipe1 <- recipe(y ~ x014 + x017 + x102 + x105 + x108 + x146 + x186 + x477 + 
                    x561 + x567 + x568 + x581 + x619 + x622 + x740 + x750, data = train) %>%
  step_nzv(all_numeric_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_impute_mean(all_numeric_predictors())

l_recipe1 %>% 
  prep() %>% 
  bake(new_data = NULL)

save(l_recipe1, folds, file = "data/setup.rda")

# RECIPE 2: RF Kitchen Sink
rf_recipe1 <- recipe(y ~x014 + x146 + x619 + x102 + x355 + x755 + x105 + x670 + x687 + x721,
                     data = train) %>%
  step_nzv(all_numeric_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_impute_mean(all_numeric_predictors())

rf_recipe1 %>% 
  prep() %>% 
  bake(new_data = NULL)

save(rf_recipe1, folds, file = "data/setup1.rda")

# RECIPE 3: Lasso x Rf Kitchen Sink
recipe3 <- recipe(y ~ x014 + x102 + x619 + x146 + x105, data = train) %>%
  step_nzv(all_numeric_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_impute_mean(all_numeric_predictors())

recipe3 %>% 
  prep() %>% 
  bake(new_data = NULL)

save(recipe3, folds, file = "data/setup2.rda")

# RECIPE 4: Lasso + Rf Kitchen Sink
recipe4 <- recipe(y ~ x014 + x017 + x102 + x105 + x108 + x146 + x186 + x477 + 
                    x561 + x567 + x568 + x581 + x619 + x622 + x740 + x750 + 
                    x355 + x755 +x670 +x687 + x721,  data = train) %>%
  step_nzv(all_numeric_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_impute_mean(all_numeric_predictors()) +
  step_corr(all_predictors)

recipe4 %>% 
  prep() %>% 
  bake(new_data = NULL)

save(recipe4, folds, file = "data/setup3.rda")


# RECIPE 5: Lasso + Rf Kitchen Sink, bag
recipe5 <- recipe(y ~ x014 + x017 + x102 + x105 + x108 + x146 + x186 + x477 + 
                    x561 + x567 + x568 + x581 + x619 + x622 + x740 + x750 + 
                    x355 + x755 +x670 +x687 + x721,  data = train) %>%
  step_nzv(all_numeric_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_impute_bag(all_numeric_predictors())

recipe5 %>% 
  prep() %>% 
  bake(new_data = NULL)

save(recipe5, folds, file = "data/setup4.rda")

# RECIPE 6: Lasso + Rf Kitchen Sink, bag
recipe6 <- recipe(log_y ~ x014 + x017 + x102 + x105 + x108 + x146 + x186 + x477 + 
                    x561 + x567 + x568 + x581 + x619 + x622 + x740 + x750 + 
                    x355 + x755 +x670 +x687 + x721,  data = train_log) %>%
  step_nzv(all_numeric_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_impute_mean(all_numeric_predictors())

recipe6 %>% 
  prep() %>% 
  bake(new_data = NULL)

save(recipe6, folds1, file = "data/setup5.rda")

# RECIPE 7: Correlation PLot Kitchen Sink, bag
recipe7 <- recipe(y ~ x014+ x102 + x105+ x146+ 
                  x561+ x567+ x619+ x750+ 
                  x355+ x755+ x670+ x687+ x721,  data = train) %>%
  step_nzv(all_numeric_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_impute_knn(all_numeric_predictors())

recipe7 %>% 
  prep() %>% 
  bake(new_data = NULL)

save(recipe7, folds, file = "data/setup6.rda")

# RECIPE 8: Correlation Plot Kitchen Sink, bag
recipe8 <- recipe(log_y ~ x014+ x102 + x105+ x146+ 
                    x561+ x567+ x619+ x750+ 
                    x355+ x755+ x670+ x687+ x721,  data = train_log) %>%
  step_nzv(all_numeric_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_impute_knn(all_numeric_predictors())

recipe9 %>% 
  prep() %>% 
  bake(new_data = NULL)

save(recipe8, folds, file = "data/setup7.rda")
