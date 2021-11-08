library(tidyverse)
library(janitor)

?pandoc

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

#use write_csv not write.csv
write_csv(notes, "data/deaths-notes.csv", na="")
write_csv(deaths, "data/clean2-deaths-2010-2019_state_year_age_race_eth.csv", na="")

deaths <- read_csv("data/clean2-deaths-2010-2019_state_year_age_race_eth.csv")

# create new variable in our data for pop. as number

deaths %>% filter(population == "Not Applicable")

# general rule of thumb for excluding data: less than 1% is not a big problem

deaths <- deaths %>% mutate(new_pop = as.numeric(population))



# line chart for deaths by year - intro to ggplot2

death_by_year <- deaths %>% filter(!is.na(new_pop)) %>%
  group_by(year) %>% summarise(death_rate = sum(deaths)/sum(new_pop)*100000)

?ggplot

ggplot(data=death_by_year, aes(x=year, y=death_rate, group=1)) +
  geom_line()+
  geom_point()

# Have any age groups experienced a decrease in death rates

age_by_year <- deaths %>% filter(!is.na(new_pop)) %>%
  group_by(year, ten_year_age_groups) %>% summarise(death_rate = sum(deaths)/sum(new_pop)*100000)

ggplot(data=age_by_year, aes(x=year, y=death_rate, group=ten_year_age_groups, color=ten_year_age_groups)) +
  geom_line()+
  geom_point()

# CLASS 9/27  ----------------

# adding new_pop to new csv file

write_csv(deaths, "data/clean2-deaths-2010-2019_state_year_age_race_eth.csv", na="")
deaths <- read_csv("data/clean2-deaths-2010-2019_state_year_age_race_eth.csv")
deaths <- deaths %>% mutate(new_pop = as.numeric(population))
write_csv(deaths, "data/clean3-deaths-2010-2019_state_year_age_race_eth.csv", na="")

# create mini table with age groups by year
age_by_year <- deaths %>% filter(!is.na(new_pop)) %>%
  group_by(year, ten_year_age_groups) %>% summarise(death_rate = sum(deaths)/sum(new_pop)*100000)

# create line chart
ggplot(data=age_by_year, aes(x=year, y=death_rate, group=ten_year_age_groups, color=ten_year_age_groups)) +
  geom_line()+
  geom_point()

# demo of percent change charts

# vectors always have to have the c
# compare 2010 and 2019 death rate by STATE
state_year <- deaths %>% filter(!is.na(new_pop) & year %in% c("2010", "2019")) %>%
  group_by(state, year) %>% 
  summarise(death_rate = sum(deaths)/sum(new_pop)*100000)

state_year %>% pivot_wider(names_from = year, values_from = death_rate) %>% 
  mutate(pct_chg = (`2019`-`2010`)/`2010`) %>%
  arrange(pct_chg)

# ------

#compare 2010 and 2019 death rate by AgE GROUP
age_year <- deaths %>% filter(!is.na(new_pop) & year %in% c("2010", "2019")) %>%
  group_by(ten_year_age_groups, year) %>%
  summarise(death_rate2 = sum(deaths)/sum(new_pop)*100000)

age_year %>% pivot_wider(names_from = year, values_from = death_rate2) %>%
  mutate(pct_chg = (`2019`-`2010`)/`2010`) %>%
  arrange(desc(pct_chg))

# biggest age group decrease: 1-4
# biggest age group increase: 25-34

# compare death rate for all years for AGE GROUPS
age_year2 <- deaths %>% filter(!is.na(new_pop)) %>%
  group_by(ten_year_age_groups, year) %>%
  summarise(death_rate2 = sum(deaths)/sum(new_pop)*100000)

age_year2pivot <- age_year2 %>% pivot_wider(names_from = year, values_from = death_rate2) %>%
  mutate(pct_chg = (`2019`-`2010`)/`2010`) %>%
  arrange(desc(pct_chg))

# create a line chart for only these groups: 25-34, 35-44

age_year_pair <- deaths %>% filter(!is.na(new_pop) & ten_year_age_groups %in% c("25-34 years", "35-44 years")) %>%
  group_by(year, ten_year_age_groups) %>% summarise(death_rate = sum(deaths)/sum(new_pop)*100000)

ggplot(data=age_year_pair, aes(x=year, y=death_rate, group=ten_year_age_groups, color=ten_year_age_groups)) +
  geom_line()+
  geom_point()

## CLASS 9/29

## READ CSV FILE

deaths <- read_csv("data/clean3-deaths-2010-2019_state_year_age_race_eth.csv")

## Compare death rate for all years for AGE GROUPS

age_year2 <- deaths %>% filter(!is.na(new_pop)) %>%
  group_by(ten_year_age_groups, year) %>%
  summarise(death_rate2 = sum(deaths)/sum(new_pop)*100000) 

## NEW FUNCTION 
## re-coding a variable

## Merging age groups to create a 65+ variable
deaths %>% filter(ten_year_age_groups_code %in% c("65-74", "75-84", "85+"))

# check unique values of any category, "check frequency"
deaths %>% count(ten_year_age_groups_code)


## added new column for new age groups
deaths <- deaths %>% mutate(new_age = case_when(
  ten_year_age_groups_code %in% c("65-74", "75-84", "85+") ~ "65+",
  ten_year_age_groups_code %in% c("25-34", "35-44", "45-54", "55-64") ~ "25-64",
  ten_year_age_groups_code %in% c("1", "1-4", "5-14","15-24") ~ "Under 25",
  TRUE ~ ten_year_age_groups_code
)) 

write_csv(deaths, "data/clean3-deaths-2010-2019_state_year_age_race_eth.csv", na="")





