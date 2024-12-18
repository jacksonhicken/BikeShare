
library(tidyverse)
library(tidymodels)
library(vroom)
library(patchwork)
library(poissonreg)
library(parsnip)
library(glmnet)
library(rpart)
library(ranger)
library(stacks)
library(dbarts)

# load data ---------------------------------------------------------------

bike_train <- vroom("train.csv")
bike_test <-vroom("test.csv")

# EDA ---------------------------------------------------------------------

plot_1 <- ggplot(data = bike_train, aes(x = temp, y = count)) + 
  geom_point() +
  geom_smooth(se = F)

plot_2 <- ggplot(data = bike_train, aes(x = humidity, y = count)) + 
  geom_point() +
  geom_smooth(se = F)

plot_3 <- ggplot(data = bike_train, aes(x = windspeed, y = count)) + 
  geom_point() +
  geom_smooth(se = F)

data <- data.frame(
  weather = bike_train$weather,
  count = bike_train$count)

average_counts <- aggregate(count ~ weather, data = data, FUN = mean)

plot_4 <- ggplot(average_counts, aes(x = weather, y = count)) +
  geom_bar(stat = "identity")

save <- (plot_1 + plot_2) / (plot_3 + plot_4)


# recipe ----------------------------------------------------------

bike_train1 <- bike_train %>% 
  select(-c(casual, registered)) %>% 
  mutate(count = log(count))

my_recipe <- recipe(count ~ . , data=bike_train1) %>%
  step_mutate(weather = ifelse(weather == 4, 3, weather)) %>% 
  step_mutate(weather = factor(weather, levels = c(1, 2, 3))) %>%
  step_mutate(holiday = factor(holiday, levels = c(0, 1))) %>% 
  step_mutate(season = factor(season, levels = c(1, 2, 3, 4))) %>%
  step_mutate(workingday = factor(workingday, levels = c(0, 1))) %>% 
  step_time(datetime, features= "hour") %>% 
  step_date(datetime, features = "year") %>%
  step_rm(datetime) %>% 
  step_mutate(datetime_hour = as.factor(datetime_hour)) %>% 
  step_rm(atemp) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_normalize(all_numeric_predictors())

prepped_recipe <- prep(my_recipe)
bake(prepped_recipe, new_data=bike_test) #do i need to prep this? or only the train data?
bake(prepped_recipe, new_data=bike_train1)


# linear regression -------------------------------------------------------

linear_model <- linear_reg() %>% 
  set_engine("lm") %>% 
  set_mode("regression")

bike_workflow <- workflow() %>% 
  add_recipe(my_recipe) %>% 
  add_model(linear_model) %>% 
  fit(data = bike_train1)

preds <- predict(bike_workflow, new_data = bike_test) %>% 
  mutate(.preds = exp(.preds)) %>% 
  bind_cols(.,bike_test) %>% 
  select(datetime,.pred) %>% 
  rename(count= .pred) %>% 
  mutate(count = pmax(0,count)) %>% 
  mutate(datetime= as.character(format(datetime)))

vroom_write(x = preds, file="./BikePreds.csv", delim=",")


# poisson regression ------------------------------------------------------

poisson_model <- poisson_reg() %>% 
  set_engine("glm") %>% 
  set_mode("regression")

bike_workflow <- workflow() %>% 
  add_recipe(my_recipe) %>% 
  add_model(poisson_model) %>% 
  fit(data = bike_train)

preds <- predict(bike_workflow, new_data = bike_test) %>% 
  mutate(.preds = exp(.preds)) %>% 
  bind_cols(.,bike_test) %>% 
  select(datetime,.pred) %>% 
  rename(count= .pred) %>% 
  mutate(count = pmax(0,count)) %>% 
  mutate(datetime= as.character(format(datetime)))

vroom_write(x = preds, file="./BikePreds.csv", delim=",")


# penalized regression ----------------------------------------------------

penalized_model <- linear_reg(penalty = 0, mixture = 1) %>% 
  set_engine("glmnet")

bike_workflow <- workflow() %>% 
  add_recipe(my_recipe) %>% 
  add_model(penalized_model) %>% 
  fit(data = bike_train)

preds <- predict(bike_workflow, new_data = bike_test) %>% 
  mutate(.preds = exp(.preds)) %>% 
  bind_cols(.,bike_test) %>% 
  select(datetime,.pred) %>% 
  rename(count= .pred) %>% 
  mutate(count = pmax(0,count)) %>% 
  mutate(datetime= as.character(format(datetime)))

vroom_write(x = preds, file="./BikePreds.csv", delim=",")


# decision trees ----------------------------------------------------------

decision_tree_model <- decision_tree(tree_depth = 5, min_n = 5) %>%
  set_engine("rpart") %>% 
  set_mode("regression")

bike_workflow <- workflow() %>% 
  add_recipe(my_recipe) %>% 
  add_model(decision_tree_model) %>% 
  fit(data = bike_train)

preds <- predict(bike_workflow, new_data = bike_test) %>% 
  mutate(.preds = exp(.preds)) %>% 
  bind_cols(.,bike_test) %>% 
  select(datetime,.pred) %>% 
  rename(count= .pred) %>% 
  mutate(count = pmax(0,count)) %>% 
  mutate(datetime= as.character(format(datetime)))

vroom_write(x = preds, file="./BikePreds.csv", delim=",")


# random forest -----------------------------------------------------------

rf_model <- rand_forest(mtry = 30, min_n= 100, trees=100) %>%
  set_engine("ranger") %>%
  set_mode("regression")

bike_workflow <- workflow() %>% 
  add_recipe(my_recipe) %>% 
  add_model(rf_model) %>% 
  fit(data = bike_train)

preds <- predict(bike_workflow, new_data = bike_test) %>% 
  mutate(.preds = exp(.preds)) %>% 
  bind_cols(.,bike_test) %>% 
  select(datetime,.pred) %>% 
  rename(count= .pred) %>% 
  mutate(count = pmax(0,count)) %>% 
  mutate(datetime= as.character(format(datetime)))

vroom_write(x=preds, file="./bikerfpreds.csv", delim=",")


# BART --------------------------------------------------------------------

BART_model <- parsnip::bart(trees = 100) %>% 
  set_engine("dbarts") %>% 
  set_mode("regression")

BART_wf <- workflow() %>% 
  add_recipe(my_recipe) %>% 
  add_model(BART_model) %>% 
  fit(data = bike_train)

preds <- predict(BART_wf, new_data = bike_test) %>% 
  mutate(.preds = exp(.preds)) %>% 
  bind_cols(.,bike_test) %>% 
  select(datetime,.pred) %>% 
  rename(count= .pred) %>% 
  mutate(count = pmax(0,count)) %>% 
  mutate(datetime= as.character(format(datetime)))

vroom_write(x=preds, file="./bartpreds.csv", delim=",")


# stacking ----------------------------------------------------------------

# really messy, just pasted all the code I used, lots out of order

folds <- vfold_cv(bike_train1, v = 5, repeats=1)

untunedModel <- control_stack_grid()
tunedModel <- control_stack_resamples()

preg_model <- linear_reg(penalty=tune(), mixture=tune()) %>%
  set_engine("glmnet")

preg_wf <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(BART_model)

preg_tuning_grid <- grid_regular(penalty(),
                                 mixture(),
                                 levels = 5)

preg_models <- preg_wf %>%
  tune_grid(resamples=folds,
          grid=tuning_grid, #this not working
          metrics=metric_set(rmse, mae, rsq),
          control = untunedModel)

lin_reg_wf <- workflow() %>% 
  add_model(lin_reg_spec) %>%
  add_recipe(lin_reg_rec)

lin_reg_model <- fit_resamples(lin_reg_wflow,
                               resamples = folds,
                               metrics = metric,
                               control = tunedModel)

my_stack <- stacks() %>%
  add_candidates(preg_models) %>%
  add_candidates(lin_reg_model) %>%
  add_candidates(another_model)

stack_mod <- my_stack %>%
blend_predictions() %>%
  fit_members() 

stackData <- as_tibble(my_stack)

stack_mod %>%
  predict(new_data=my_new_data)

stack_wf <- workflow() %>% 
  add_model(linear_model) %>% 
  add_recipe(my_recipe)

stack_wf2 <- workflow() %>% 
  add_model(penalized_model) %>% 
  add_recipe(my_recipe)

stack_wf3 <- workflow() %>% 
  add_model(random_forest_model) %>% 
  add_recipe(my_recipe)

lin_model <- fit_resamples(stack_wf,
                           resamples = folds,
                           metrics = NULL,
                           control = untunedModel)


pen_model <- fit_resamples(stack_wf2,
                           resamples = folds,
                           metrics = NULL,
                           control = untunedModel)

rf_model <- fit_resamples(stack_wf3,
                          resamples = folds,
                          metrics = NULL,
                          control = untunedModel)

my_stack <- stacks() %>% 
  add_candidates(lin_model) %>% 
  add_candidates(pen_model) %>% 
  add_candidates(rf_model)

stack_mod <- my_stack %>% 
  blend_predictions() %>% 
  fit_members()

stack_data <- as_tibble(my_stack)

stack_preds <- stack_mod %>% predict(new_data = bike_test)
stack_preds <- exp(stack_preds)

kaggle_submission <- stack_preds %>%
  bind_cols(., bike_test) %>%
  select(datetime, .pred) %>%
  rename(count=.pred) %>%
  mutate(count=pmax(0, count)) %>%
  mutate(datetime=as.character(format(datetime)))

vroom_write(x=kaggle_submission, file="./Preds_stack.csv", delim=",")


