library(ggplot2)
library(tidyverse)
library(tidymodels)
library(corrplot)
library(ggthemes)
tidymodels_prefer()
library(ISLR)
library(yardstick)

abalone <- read.csv("C:\\abalone.csv")
view(abalone)
abalone$rings

# Q1: Create abalone dataset with age
abalone <- abalone %>%
  mutate(age = abalone$rings + 1.50)
view(abalone)
## view distribution of age
abalone %>% ggplot(aes(x=age))+ geom_histogram(bins=60)

# Q2: split into training and testing set 
set.seed(4857)
abalone$rings = NULL
abalone_split <- initial_split(abalone, prop = 0.8,
                               strata = age)
abalone_train <- training(abalone_split)
abalone_test <- testing(abalone_split)

# Q3: recipe predicting age (without rings)
abalone_recipe <- recipe(age ~ ., 
                         data = abalone_train) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_interact(terms=~starts_with("type"):shucked_weight) %>%
  step_interact(terms=~longest_shell:diameter) %>%
  step_interact(terms=~shucked_weight:shell_weight) %>%
  step_normalize(all_predictors()) %>%
  prep(abalone_train) 
abalone_recipe
abalone_train
prep(abalone_recipe)
summary(abalone_recipe)

# Q4: Create and Store a linear object using "lm" engine
lm_model <- linear_reg() %>%
  set_engine("lm")

# Q5: Set up wkflow with model and recipe
lm_wflow <- workflow() %>%
  add_model(lm_model) %>%
  add_recipe(abalone_recipe)

# Q6: Use fit() to predict age with given data
lm_fit <- fit(lm_wflow, abalone_train)
lm_fit
df <- data.frame(longest_shell = 0.5, diameter = 0.10,
                 height = 0.30, whole_weight = 4,
                 shucked_weight = 1, viscera_weight = 2,
                 shell_weight = 1, type = "F")
predict(lm_fit, new_data = df)

# Q7: 
abalone_train_res <- predict(lm_fit, new_data = abalone_train %>%
                               select(-age))
abalone_train_res <- bind_cols(abalone_train_res,
                               abalone_train %>% select(age))
abalone_train_res %>% head()

abalone_metrics <- metric_set(rmse, rsq, mae)
abalone_metrics(abalone_train_res, truth = age, estimate = .pred)

