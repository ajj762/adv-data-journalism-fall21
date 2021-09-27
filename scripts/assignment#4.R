library(tidyverse)
library(janitor)

# site: cdc.wonder.gov

# Load text file
deaths <- read_delim("data/drug&alcohol_deaths_by_state-age-year.txt", delim="\t")

# Clean names
deaths <- deaths %>% clean_names()

# Removing the notes at the bottom a separate file
notes <- deaths %>% filter(!is.na(notes)) %>% select(notes)
deaths <- deaths %>% filter(is.na(notes))

# Take out notes column from deaths
deaths <- deaths %>% filter(is.na(notes)) %>%
  select(-notes)

# Create new_deaths and new_pop columns 
deaths <- deaths %>% mutate(new_deaths = as.numeric(deaths))
deaths <- deaths %>% mutate(new_pop = as.numeric(population))
write_csv(deaths, "data/clean1-drug&alcohol-deaths-2010-2019-state-age-year.csv", na="")

# Add new rate column
deaths <- deaths %>% filter(!is.na(new_deaths)) %>%
  mutate(rate = new_deaths/new_pop*100000) 

## INTEGRITY CHECKS

# How many different causes of death are in this data?
death_types <- deaths %>% select(mcd_drug_alcohol_induced_cause) %>%
  group_by(mcd_drug_alcohol_induced_cause) %>%
  summarise(death_type = n())
# There are 8 causes of death listed including all other non-drug and non-alcohol related deaths.

# How many years does it cover?
deaths %>% count(year)
# 2015 to 2019

# QUESTIONS

## 1 National: What year had the highest drug/alcohol death rate?
deaths %>% filter(mcd_drug_alcohol_induced_cause != "All other non-drug and non-alcohol causes") %>%
  group_by(year) %>% 
  summarise(year_rate = sum(new_deaths)/sum(new_pop)*100000) %>%
  arrange(desc(year_rate))
## 2019 at 40.4


## 2 National: What age group had the highest drug/alcohol death rate in 2019?

deaths %>% filter(mcd_drug_alcohol_induced_cause != "All other non-drug and non-alcohol causes" & year == "2019") %>%
  group_by(ten_year_age_groups) %>%
  summarise(year_rate = sum(new_deaths)/sum(new_pop)*100000) %>%
  arrange(desc(year_rate))
## The age 85+ had the highest drug and alcohol related death rate at 346 nationally. 

  
## 3 National: What state had the highest drug/alcohol death rate in 2019?
deaths %>% filter(mcd_drug_alcohol_induced_cause != "All other non-drug and non-alcohol causes" & year == "2019") %>%
  group_by(state) %>%
  summarise(state_rate = sum(new_deaths)/sum(new_pop)*100000) %>%
  arrange(desc(state_rate))
## Vermont had the highest drug and alcohol related deaths rate in 2019 at 141. Maine, Delaware and New Hampshire are also in the top ten highest rates for states. 


## 4 What year in Missouri had the highest drug/alcohol death rate?
missouri <- deaths %>% filter(state == "Missouri")

missouri %>% filter(mcd_drug_alcohol_induced_cause != "All other non-drug and non-alcohol causes") %>%
  group_by(year) %>%
  summarise(mo_year = sum(new_deaths)/sum(new_pop)*100000) %>%
  arrange(desc(mo_year))
## Missouri's rate for drug and alcohol related deaths was highest in 2018 (between 2015 and 2019). The rate has fluctuated every year. 
## The rate in Missouri went down from 59.6 in 2018 to 53.4 in 2019. This is not consistent with national data.

## 5 What was the percent change for drug or alcohol related deaths from 2018 to 2019 in Missouri?
rate_change <- missouri %>% filter(!is.na(new_deaths) & !is.na(new_pop) & year %in% c("2018", "2019") & mcd_drug_alcohol_induced_cause != "All other non-drug and non-alcohol causes") %>%
  group_by(year) %>%
  summarise(death_rate = sum(new_deaths)/sum(new_pop)*100000)

rate_change <- rate_change %>% pivot_wider(names_from = year, values_from = death_rate) %>%
  mutate(pct_chg = (`2019`-`2018`)/`2018`*100) %>%
  arrange(desc(pct_chg))
## The rate for deaths caused by drugs or alcohol in Missouri decreased by 10.4% from 2018 to 2019.

## 6 What was the national percent change for drug or alcohol related deaths from 2018 to 2019 in Missouri?

rate_changenatl <- deaths %>% filter(!is.na(new_deaths) & !is.na(new_pop) & year %in% c("2018", "2019") & mcd_drug_alcohol_induced_cause != "All other non-drug and non-alcohol causes") %>%
  group_by(year) %>%
  summarise(rate_change = sum(new_deaths)/sum(new_pop)*100000)

rate_changenatl <- rate_changenatl %>% pivot_wider(names_from = year, values_from = rate_change) %>%
  mutate(pct_chg = (`2019`-`2018`)/`2018`*100) %>%
  arrange(desc(pct_chg))
## 3.4% is the national percent change from 2018 to 2019.

## 7 What age group in Missouri had the highest drug/alcohol death rate?
missouri %>% filter(mcd_drug_alcohol_induced_cause != "All other non-drug and non-alcohol causes") %>%
  group_by(ten_year_age_groups) %>%
  summarise(mo_ages = sum(new_deaths)/sum(new_pop)*100000) %>%
  arrange(desc(mo_ages))
## The 85+ age group had the highest drug/alcohol related death rate at 788. It's consistent with national data, but still over double the national rate for 85+.

## 8 What state had the highest rate for drug overdoses categorized as suicides in 2019? Where is Missouri on the list?
state_suicide <- deaths %>% filter(mcd_drug_alcohol_induced_cause == "Drug poisonings (overdose) Suicide (X60-X64)" & year == "2019") %>%
  group_by(state) %>%
  summarise(state_rate = sum(new_deaths)/sum(new_pop)*100000) %>%
  arrange(desc(state_rate))
## Maine, with a rate of 6.3.
## Missouri is 13th highest at a rate of 3.03.

## 9 What was the national rate for drug overdoses categorized as suicides in 2019?
deaths %>% filter(mcd_drug_alcohol_induced_cause == "Drug poisonings (overdose) Suicide (X60-X64)" & year == "2019") %>%
summarise(natl_rate = sum(new_deaths)/sum(new_pop)*100000) %>%
  arrange(desc(natl_rate))
## The national rate was 2.1 in 2019, which is lower than Missouri's rate. 

## 10 What age group had the highest percent change for the drug overdoses categorized as suicide rate from 2015 to 2019?

age_suicide <- deaths %>% filter(!is.na(new_deaths) & !is.na(new_pop) & year %in% c("2015", "2019") & mcd_drug_alcohol_induced_cause == "Drug poisonings (overdose) Suicide (X60-X64)") %>%
  group_by(ten_year_age_groups, year) %>%
  summarise(death_rate = sum(new_deaths)/sum(new_pop)*100000)

age_suicide <- age_suicide %>% pivot_wider(names_from = year, values_from = death_rate) %>%
  mutate(pct_chg = (`2019`-`2015`)/`2015`*100) %>%
  arrange(desc(pct_chg))

## 15-24 year olds had a 50% increase in the rate of drug overdoses categorized as suicide from 2015 to 2019. 
## 75-84 year olds had a 10% increase. All other age groups percent change for rates decreased.

## 11 What drug or alcohol cause of death has the highest rate? How does Missouri's rate compare?

deaths %>% filter(mcd_drug_alcohol_induced_cause != "All other non-drug and non-alcohol causes") %>%
  group_by(mcd_drug_alcohol_induced_cause) %>%
  summarise(cause_rate = sum(new_deaths)/sum(new_pop)*100000) %>%
  arrange(desc(cause_rate))

## The overall cause with the highest death rate is all other drug-induced causes at 136. 

deaths %>% filter(state == "Missouri" & mcd_drug_alcohol_induced_cause != "All other non-drug and non-alcohol causes") %>%
  group_by(mcd_drug_alcohol_induced_cause) %>%
  summarise(cause_rate = sum(new_deaths)/sum(new_pop)*100000) %>%
  arrange(desc(cause_rate))

## The overall cause with the highest death rate is all other drug-induced causes at 216.
## Missouri's drug and alcohol related death rate is higher than the national rate. 

## Graf #1
## The rate for deaths related to drugs or alcohol on Missouri decreased by 10.4% from 2018 to 2019. In 2018, the rate for drug or alcohol related deaths was 59.6 deaths per 100,000. In 2019, it was 53.4 deaths per 100,000. This decline is inconsistent with the national rate — which increased by 3.4% from 2018 to 2019.

## Graf 2
## From 2015 to 2019, the rate of deaths from drug overdoses categorized as suicide increased by 50% in the nation for individuals ages 15 to 24. The age group with the second highest rate of deaths from drug overdoses, ages 75-84, categorized as suicide increased by 10%. All other age groups in the nation saw a decrease for suicide by drug overdose.





