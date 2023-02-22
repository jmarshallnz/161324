library(tidyverse)
library(renderthis)

which_lectures <- 8:9

files = tibble(html = sprintf('lecture%02i.html', which_lectures))

files %>% mutate(file = file.path('../lectures', html)) %>%
  pull(file) %>% map(to_pdf)
