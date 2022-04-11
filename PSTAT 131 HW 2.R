library(ggplot2)
library(tidyverse)
library(tidymodels)
library(corrplot)
library(ggthemes)
tidymodels_prefer()
library(ISLR)
data("abalone")
abalone$rings


# QUESTION 1
##create abalone dataset with age 
abalone1 <- abalone %>%
  mutate(age = abalone$rings + 1.5)
view(abalone1)
## view distribution of age 
abalone1 %>% ggplot(aes(x=age))+ geom_histogram(bins=60)

# QUESTION 2
## split abalone1 into training and testing set
## with stratified samplying 
set.seed(4857)
abalone1$rings = NULL
abalone1_split <- initial_split(abalone1, prop = 0.8,
                               strata = age)
abalone1_train <- training(abalone1_split)
abalone1_test <- testing(abalone1_split)

# QUESTION 3
## create a recipe predicting age with all other predictors 
## do not use rings to predict age 
## 1. Dummy code any categorical predictors
## 2 create interactions btwn variables listed
albalone1_recipe <- recipe(age ~ ., 
    data = abalone1_train) %>%
    step_dummy(all_nominal_predictors()) %>%
    step_interact(terms=~starts_with("type"):shucked_weight) %>%
    step_interact(terms=~longest_shell:diameter) %>%
    step_interact(terms=~shucked_weight:shell_weight) %>%
    step_normalize(all_predictors()) %>%
    prep()
prep(albalone1_recipe)
summary(albalone1_recipe)
# QUESTION 4
## Create and store a linear regression object
lm_model <- linear_reg() %>%
  set_engine("lm")

# QUESTION 5
## 1. set up an empty workflow
## 2. add the model you created from 4
## 3. add the recipe you created from 3
lm_wflow <- workflow() %>%
  add_model(lm_model) %>%
  add_recipe(albalone1_recipe)

# QUESTION 6
## use fit object to predict the age of a female 
## abalone with:
### longest_shell = 0.5
### diameter = 0.10
### height = 0.30
### whole_weight = 4
### shucked_weight = 1
### viscera_weight = 2
### shell_weight = 1
lm_fit <- fit(lm_wflow, abalone1_train) %>% extract_fit_parsnip() %>% tidy()
df <- data.frame(longest_shell = 0.5, diameter = 0.10,
                 height = 0.30,whole_weight = 4,
                 shucked_weight = 1, viscera_weight = 2,
                 shell_weight = 1, type = "F")
predict(lm_fit, new_data = df)

# QUESTION 7
## 1. create a metric set that includes R^2, RMSE, MAE
## 2. use predict() and bind_cols() to create a tibble
## of your model's predicted valyes from the training data
## along with the actual observed ages 
abalone_train_res <- predict(lm_fit, new_data = abalone1_train %>%
                               select(-age))
abalone_train_res <- bind_cols(abalone_train_res,
                               abalone1_train %>% select(age))
abalone_train_res %>% head()

abalone_metrics <- metric_set(rmse, rsq, mae)
abalone_metrics(abalone_train_res, truth = age, estimate = .pred)






