library(tidymodels)
library(palmerpenguins)
library(dplyr)
set.seed(123)
penguins_split <- initial_split(data = penguins, prop = 0.7)
train_data <- training(penguins_split)
test_data <- testing(penguins_split)
training_view <- glimpse(train_data) %>% as_tibble()
testing_view <- glimpse(test_data) %>% as_tibble()
nrow(train_data) * 1/10
penguin_fold <- vfold_cv(train_data, v = 10)
glimpse(penguin_fold) %>% as_tibble()

# exercise 16
# A logistic regression model is a type of linear model used for binary classes of logistic data. This model is useful for predicting a probability using a logistic function and simulating logistic regression data.
# A rand_forest model is a model that creates many decision trees. The output of this model is the majority classification and average prediction. 
logistic_model <- multinom_reg() %>%
  set_engine("nnet") %>%
  set_mode("classification")
rf_model <- rand_forest() %>%
  set_engine("ranger") %>%
  set_mode("classification")

install.packages("workflowsets")
library(workflowsets)

log_wf_rs <- workflow() %>%
  add_formula(species ~ .) %>%
  add_model(logistic_model) %>% 
  fit_resamples(resamples = penguin_fold,
                control = control_resamples(save_pred = TRUE))
  
rf_wf_rs <- workflow() %>%
  add_formula(species ~ .) %>%
  add_model(rf_model) %>% 
  fit_resamples(resamples = penguin_fold,
                control = control_resamples(save_pred = TRUE))

collect_metrics(log_wf_rs)
collect_metrics(rf_wf_rs)

wf_set <- 
  workflow_set(list(species ~ .), list(logistic_model, rf_model)) %>%
  workflow_map("fit_resamples", 
               resamples = penguin_fold, 
               metrics = metric_set(accuracy),
               control = control_resamples(save_pred = TRUE))
wf_rank <- wf_set %>% collect_metrics() %>%
  filter(.metric == "accuracy") %>%
  arrange(desc(mean))
glimpse(wf_rank)  
