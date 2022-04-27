library(tidyverse)
library(xaringanBuilder)

to_pdf <- 3:7

files = tibble(html = sprintf('lecture%02i.html', to_pdf))

files %>% mutate(file = file.path('../lectures', html)) %>%
  pull(file) %>% map(build_pdf)
