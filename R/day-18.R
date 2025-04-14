
library(tidyverse)
library(tidymodels)
library(dplyr)

# Ingest
covid_url <- 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv'
pop_url   <- 'https://www2.census.gov/programs-surveys/popest/datasets/2020-2023/counties/totals/co-est2023-alldata.csv'

data = readr::read_csv(covid_url)
census = readr::read_csv(pop_url) 

# Clean
census = census |>
  filter(COUNTY == "000") |> 
  mutate(fips = STATE) |>
  select(fips, contains("2021"))

state_data <- data |> 
  group_by(fips) |>
  mutate(new_cases  = pmax(0, cases - lag(cases)),
         new_deaths = pmax(0, deaths - lag(deaths))) |>
  ungroup() |>
  left_join(census, by = "fips") |>
  mutate(m = month(date), y = year(date),
         season = case_when(
           m %in% 3:5 ~ "Spring",
           m %in% 6:8 ~ "Summer",
           m %in% 9:11 ~ "Fall",
           m %in% c(12, 1, 2) ~ "Winter"
         )) |> 
  group_by(state, y, season) |>
  mutate(season_cases  = sum(new_cases, na.rm = TRUE), 
         season_deaths = sum(new_deaths, na.rm = TRUE))  |> 
  distinct(state, y, season, .keep_all = TRUE) |> 
  ungroup() |> 
  select(state, contains('season'), y, POPESTIMATE2021, BIRTHS2021, DEATHS2021) |> 
  drop_na() |> 
  mutate(logD = log(season_deaths + 1)) 

skimr::skim(state_data)

# Resample
split <- initial_split(state_data, prop = 0.8, strata = season)
train <- training(split)
test <- testing(split)
folds <- vfold_cv(train, v = 10)

# Engineer 
rec = recipe(logD ~ . , data = train) |> 
  step_rm(state, season_deaths) |>  # Removing unnecessary columns
  step_dummy(all_nominal()) |>
  step_scale(all_numeric_predictors()) |> 
  step_center(all_numeric_predictors())

# Models specifications for Regression!
lm_model <- linear_reg() |> 
  set_engine("lm") |> 
  set_mode("regression")

rf_model <- rand_forest() |> 
  set_engine("ranger") |> 
  set_mode("regression")

b_mod <- boost_tree() |> 
  set_engine("xgboost") |> 
  set_mode("regression")

nn_mod <- mlp(hidden_units = 10) |> 
  set_engine("nnet") |> 
  set_mode("regression")

# Workflow
install.packages("randomForest")
library(randomForest)
wf = workflow_set(list(rec), list(lm_model, 
                                  rf_model, 
                                  b_mod, 
                                  nn_mod
)) |> 
  workflow_map(resamples = folds) 

# Select
autoplot(wf)

# Fit
fit <- workflow() |> 
  add_recipe(rec) |> 
  add_model(rf_model) |> 
  fit(data = train)

vip::vip(fit)

# Evaluate
a <- augment(fit, new_data = test) |> 
  mutate(diff = abs(logD - .pred))

metrics(a, truth = logD, estimate = .pred)

library(ggplot2)
ggplot(a, aes(x = 10^logD, y = 10^.pred)) + 
  geom_point() + 
  geom_abline() +
  geom_label(aes(label = paste(state, season)), nudge_x = 0.1, nudge_y = 0.1) +
  labs(title = "Random Forest Model", 
       x = "Actual", 
       y = "Predicted") + 
  theme_minimal()




