library(tidyverse)
library(haven)

bank <- read_sas("../data_sources/sas/bank.sas7bdat")

# OK, convert to transactions database?
bank %>% write_csv("../data/bank-rules.csv")