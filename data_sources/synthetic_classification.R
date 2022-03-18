library(tidyverse)

# Read in old data

syn2.train <- read_csv("https://www.massey.ac.nz/~jcmarsha/161223/data/syn2-train.csv")
syn2.test  <- read_csv("https://www.massey.ac.nz/~jcmarsha/161223/data/syn2-testx.csv") %>%
  bind_cols(class = scan("https://www.massey.ac.nz/~jcmarsha/161223/data/syn2-testy.txt"))

syn2.train %>%
  rename(group = class, x1 = X1, x2 = X2, x3 = X3) %>%
  mutate(group = factor(group, levels=1:3, labels=LETTERS[1:3])) %>%
  write_csv("../data/syn-class2-train.csv")

syn2.test %>%
  rename(group = class, x1 = X1, x2 = X2, x3 = X3) %>%
  mutate(group = factor(group, levels=1:3, labels=LETTERS[1:3])) %>%
  write_csv("../data/syn-class2-test.csv")

if (0) {
  # This is needed if we want NB to do better than QDA on number 2...
  # Transforms old syn2.train/syn2.test into new versions which aren't gaussian, but are independent:
  syn2.all <- bind_rows(lst(syn2.train, syn2.test), .id='set')
  syn2.all %>%
    mutate(x2 = exp(x2/3),
           x1 = exp(x1/3),
           x3 = exp(x3/3)) %>%
    group_by(group) %>%
    mutate(across(starts_with('x'), ~ (. - mean(.))/sqrt(sd(.)))) %>%
    ungroup() -> syn2_chk
  
  syn2_chk %>% filter(set == "syn2.train") %>% select(-set) %>% write_csv("../data/syn-class2-train.csv")
  syn2_chk %>% filter(set == "syn2.test") %>% select(-set) %>% write_csv("../data/syn-class2-test.csv")
}
