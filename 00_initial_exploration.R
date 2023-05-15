# Data Exploration
library(tidyverse)
library(tidymodels)

train <- read_csv("data/train.csv")
test <- read_csv("data/test.csv")

# best practice to split train data to prevent overfitting

set.seed(1234)
my_split <- initial_split(train, prop = 0.75, strata = y)

train_data <- training(my_split)
test_data <- testing(my_split)

################################################################
# Functions for exploration
boxplot_fun <- function(var = NULL) {
  ggplot(train_data, aes(x = (!!sym(var)), y = y)) +
    geom_boxplot()
}

boxplot_log_fun <- function(var = NULL) {
  ggplot(train_data, aes(x = (!!sym(var)), y = log(y))) +
    geom_boxplot()
}



################################################################
# Distribution of Y

ggplot(train_data, aes(y)) +
  geom_histogram()

MASS::boxcox(lm(y ~1, train_data))
# recommends a potential log transformation

ggplot(train_data, aes(x = log(y))) +
  geom_histogram()

################################################################
# missingness

missing_lst <- list()

for(var in colnames(train_data)) {
  missing_lst[var] <- train_data %>% 
    select(any_of(var)) %>% 
    filter(is.na(!!sym(var))) %>% 
    summarize(num_missing = n())
}

missing_tbl <- enframe(unlist(missing_lst))

missing_tbl %>%
  mutate(pct = value/4034) %>% 
  arrange(desc(pct))

################################################################
# remove zero_var
# step_zv() <- automatically does this for us
var_lst <- list()

for(var in colnames(train_data)) {
  var_lst[var] <- train_data %>% 
    select(any_of(var)) %>%
    summarize(sd = sd(!!sym(var), na.rm = TRUE))
}

var_tbl <- enframe(unlist(var_lst))

# remove zero variance
# high variance might benefit from a transformation 

zero_var <- var_tbl %>% 
  filter(value == 0) %>% 
  pull(name) # tidyverse version of data$variable

# update training data to remove unwanted variables
train_data <- train_data %>% 
  select(!all_of(zero_var))

################################################################
# high correlation
# step_corr could potentially do the same thing 



################################################################
# miscoded categorical variables

cat_lst <- list()

for(var in colnames(train_data)) {
  cat_lst[var] <- train_data %>% 
    select(any_of(var)) %>%
    # count unique values in variable
    summarize(unique = length(unique(!!sym(var))))
}

cat_tbl <- enframe(unlist(cat_lst))

cat_var <- cat_tbl %>% 
  filter(value <= 10) %>% 
  select(name)

# x516, x556
# make data factors
train_data <- train_data %>% 
  mutate(x516 = as.factor(x516),
         x556 = as.factor(x556))

boxplot_log_fun(var = "x025")

map(cat_var, boxplot_fun)

# choose if any look like they have a relationship
# turn to factor with mutate
# do so in both training/testing

# could write function for histograms to explore variable
# could write function for scatterplots to explore relationship with y

# save out "clean data sets"
# consider variable reduction such as lasso and random forest

save(train_data, test_data, file = "data/train_data.rda")
