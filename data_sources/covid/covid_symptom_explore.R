library(tidyverse)
library(skimr)
library(parsnip)
library(yardstick)

# try some more interesting data
covid <- read_csv("../data_sources/covid/covid_for_CART_noasymp_age_sex_month_forJM.csv") %>%
  filter(Sex != "Unknown", AgeGrp != "Unknown") %>%
  mutate(across(everything(), as.factor))

write_csv(covid, "../data/covid_symptoms.csv")

# Hmm, we want the factors such that the baseline group is "No"
make_no_first_level <- function(levels) {
  is_no <- str_detect(levels, "^No")
  levels <- c(levels[is_no], levels[!is_no])
  levels
}

covid_relev <- covid %>% mutate(across(everything(), fct_relevel, make_no_first_level))

no_yes <- function(levels) {
  is_no <- str_detect(levels, "^No")
  if (sum(is_no) == 1 && length(levels) == 2) {
    levels <- c("No", "Yes")[(!is_no)+1]
  }
  levels
}

covid_relev %>%
  mutate(across(-Status, fct_relabel, no_yes)) %>%
  write_csv("../data/covid19/symptoms.csv")

# split into two
library(rsample)
set.seed(3)
split <- initial_split(covid_relev, prop=3/4, strata=Status)

covid.train <- training(split)
covid.test  <- testing(split)

# create a recipe for oversampling the minority class
library(recipes)
library(themis) # for SMOTE

rec <- recipe(Status ~ ., data=covid.train) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_smote(Status, over_ratio=1) %>%
  prep(covid.train)

covid.train.bk <- bake(rec, new_data=NULL)
covid.test.bk  <- bake(rec, new_data=covid.test)

# we wish to model the outcome variable 'Status' in terms of others.
covid.lr <- logistic_reg() %>%
  fit(Status ~ ., data=covid.train)

covid.lr %>% tidy() %>% as.data.frame()

covid_best <- covid.lr %>% augment(covid.test) %>%
  roc_curve(truth=Status, .pred_Confirmed, event_level='second') %>%
  autoplot()
  mutate(sum = specificity+sensitivity) %>%
  slice_max(sum, n=1)

covid.lr %>% augment(covid.test) %>%
  full_join(covid_best, by=character()) %>%
  mutate(.pred_thresh =
           if_else(.pred_Not_a_case > .threshold, "Not_a_case", "Confirmed")) %>%
  conf_mat(truth=Status, estimate=.pred_thresh)

  autoplot()

  ggplot() +
  geom_line(aes(x=1-specificity, y=sensitivity))

  conf_mat(truth=Status, estimate=.pred_class)


covid.lr %>% augment(covid.test.bk) %>%
  roc_auc(truth=Status, .pred_Not_a_case)

  conf_mat(truth=Status, estimate=.pred_class)
# This isn't the best, eh?
covid.lr %>% augment(covid.test)

# Try some other things...
# LDA, KDA are going to be poor as no numerics. NB assumes independence (highly unlikely) but mayaswell try?
# KNN might work, but again we don't have numerics.
library(discrim)
naive_Bayes(engine='naivebayes', Laplace = 0.1) %>%
  fit(Status ~ ., data=covid.train.bk) %>%
  augment(covid.test.bk) %>%
  conf_mat(truth=Status, estimate=.pred_class)

#library(kknn) # needed due to a bug
#nearest_neighbor(mode='classification', neighbors=20) %>%
#  fit(Status ~ ., data=covid.train) %>%
#  augment(covid.test) %>%
#  conf_mat(truth=Status, estimate=.pred_class)

decision_tree(mode='classification', cost_complexity = 0.00001) %>%
  set_engine('rpart') %>%
  fit(Status ~ ., data=covid.train.bk) %>%
  augment(covid.test.bk) %>%
  conf_mat(truth=Status, estimate=.pred_class)

rand_forest(mode='classification', engine='randomForest', mtry = 2) %>%
  fit(Status ~ ., data=covid.train.bk) %>%
  augment(covid.test.bk) %>%
  conf_mat(truth=Status, estimate=.pred_class)

mlp(mode='classification', hidden_units=16, epochs=1000) %>%
  fit(Status ~ ., data=covid.train.bk) %>%
  augment(covid.test.bk) %>%
  #roc_auc(truth=Status, .pred_Not_a_case)
  conf_mat(truth=Status, estimate=.pred_class)
