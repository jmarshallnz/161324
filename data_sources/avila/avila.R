library(tidyverse)
library(rsample)

# info:
read_csv("../data_sources/avila/avila-description.txt", col_names=FALSE)

# names:
#F1       intercolumnar distance 
#F2       upper margin 
#F3       lower margin 
#F4       exploitation (how much of the column is filled with ink)
#F5       number of rows
#F6       modular ratio (ratio of width to height of characters)
#F7       interlinear spacing 
#F8       weight  (how much a row is filled with ink)
#F9       peak number (a measure of vertical ink density across the page)
#F10     modular ratio/ interlinear spacing (ratio of modular ratio to the interline spacing)
#Class: A, B, C, D, E, F, G, H, I, W, X, Y

avila.names <- c("icd", "upmar", "lomar", "exploit", "numrow", "charratio", "ils", "weight", "numpeaks", "crils", "scribe")
avila.train <- read_csv("../data_sources/avila/avila-tr.txt", col_names=FALSE) %>%
  set_names(nm = avila.names) %>% mutate(scribe = factor(scribe, levels=c(LETTERS[1:9], 'W', 'X', 'Y')))
avila.test <- read_csv("../data_sources/avila/avila-ts.txt", col_names=FALSE) %>%
  set_names(nm = avila.names) %>% mutate(scribe = factor(scribe, levels=c(LETTERS[1:9], 'W', 'X', 'Y')))

# OK, let's just play with avila.train, and we'll subset it randomly. We need to be careful to not make
# it look like the original data, otherwise students can just use that for prediction.
set.seed(1234)
split <- initial_split(avila.train, prop=2/3)
training(split) %>% write_csv("../data/avila-train.csv")
testing(split) %>% write_csv("../data/avila-test.csv")

avila.train <- read_csv("../data/avila-train.csv") %>% mutate(scribe=factor(scribe))
avila.test <- read_csv("../data/avila-test.csv") %>% mutate(scribe=factor(scribe))
# test a model?
library(parsnip)
library(yardstick)
knn1 <- nearest_neighbor(mode='classification') %>% fit(scribe ~ ., data=avila.train)
knn1.pred <- knn1 %>% augment(avila.test)
knn1.pred %>%
  accuracy(truth='scribe', estimate=".pred_class")

rf1 <- rand_forest(mode='classification') %>% fit(scribe ~ ., data=avila.train)
rf1.pred <- rf1 %>% augment(avila.test)
rf1.pred %>%
  accuracy(truth='scribe', estimate=".pred_class")

rp1 <- decision_tree(mode='classification') %>% fit(scribe ~ ., data=avila.train)
rp1.pred <- rp1 %>% augment(avila.test)
rp1.pred %>%
  accuracy(truth='scribe', estimate=".pred_class")
