library(tidyverse)

parties <- read_csv("../data_sources/nzelect/percentage-votes-for-registered-parties.csv",
                    skip=2) %>%
  select(Electorate, Act=3, Green=5, Labour=7, Maori=9, National=11, Other=13) %>%
  filter(!str_detect(Electorate, "Totals"))

candidates <- read_csv("../data_sources/nzelect/percentage-candidate-votes-for-registered-parties.csv",
                       skip=2) %>%
  select(Electorate, Act=3, Green=5, Labour=7, Maori=9, National=11, Other=13) %>%
  filter(!str_detect(Electorate, "Totals"))

parties %>% write_csv("../data/nzelect/2020_party.csv")
candidates %>% write_csv("../data/nzelect/2020_candidate.csv")
