library(tidyverse)
library(janitor)

# site: wonder.cdc.gov
# downloaded on Sept. 20
deaths <- read_delim("data/deaths-2010-2019_state_year_age_race_eth.txt", delim="\t")

# \t is a regular expression. universal way of indicating a tab
# crude rate: per 100,000

# remove notes at the bottom to a separate file
notes <- deaths %>% filter(!is.na(Notes)) %>% select(Notes)
deaths <- deaths %>% filter(is.na(Notes))

# take notes column out all together
deaths <- deaths %>% filter(is.na(Notes)) %>%
  select(-Notes)

# clean names
deaths <- deaths %>% clean_names()

# What year had the most deaths?
deaths %>% group_by(year) %>%
  summarise(deaths = sum(deaths)/sum(population)) %>%
  arrange(desc(deaths))
# 2019 had the most with 2,848,638

# Recreate crude rates for year - sum of deaths/sum of population * 100,000
deaths %>% count(population) %>% arrange(desc(population))
# Not applicable made it import as character column
# how to change: as.numeric

deaths %>% filter(population != "Not Applicable") %>%
  group_by(year) %>%
  summarise(deaths = sum(deaths)/sum(as.numeric(population))*100000)

write.csv(notes, "data/deaths-notes.csv", na="")
write.csv(deaths, "data/clean1-deaths-2010-2019_state_year_age_race_eth.csv", na="")

