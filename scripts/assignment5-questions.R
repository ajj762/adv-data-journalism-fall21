library(tidyverse)
library(janitor)

# Import the Provisional Deaths and the Population files
# Change the paths if needed
pop <- read_csv("data/pop-by-race-eth-age.csv")
deaths <- read_csv("data/Provisional_COVID-19_Deaths_by_Race_and_Hispanic_Origin__and_Age.csv")

# Clean up the data: alter the pop file (as we discussed in class)
# Clean up the column names in the deaths data
pop <- pop %>% mutate(race_eth = str_replace(race_eth, "Non Hispanic", "Non-Hispanic"))
deaths <- deaths %>% clean_names()

# Join the tables
combined <- deaths %>% inner_join(pop, by=c("state"="name","age_group","race_and_hispanic_origin_group"="race_eth"))

# Your job is to analyze the data and come up with the most compelling story idea to pitch to your editor. 
# I expect some thorough analysis here: explore several possible ideas. Walk me through those ideas 
# in your code. I should be able to run the code below and check your work and verify the numbers in your pitch. 
# Include a short pitch (a few sentences) with numbers from your analysis that outlines your story idea.
# You will be graded on the code you write, the level of your analysis, and the strength of your pitch 
# (don't pitch a story idea that simply states the death rate for one group, for example. Think in terms of comparisons.)

## INTEGRITY CHECKS

# how many NA's for covid-19 deaths?
combined %>% filter(is.na(covid_19_deaths)) %>% summarise(n = n())
# 625

# how many total covid deaths?
combined %>% filter(!is.na(covid_19_deaths)) %>%
  summarise(total_covid = sum(covid_19_deaths))
# 651946

## checking population totals by state
combined %>% filter(!is.na(covid_19_deaths)) %>%
  distinct(state,race_and_hispanic_origin_group,pop2019) %>%
  group_by(state) %>%
  summarise(population = sum(pop2019))
## populations match rough estimates for each state

## ANALYSIS

## These four queries show the which races have the highest death rates for covid-19, influenza, pneumonia and overall. 
## I am starting here to start broadly analyzing and look for ideas to narrow down on. 

## Looked at the overall covid-19 death rates by race
combined %>% filter(!is.na(covid_19_deaths)) %>% 
  group_by(race_and_hispanic_origin_group) %>%
  summarise(death_rate = sum(covid_19_deaths)/sum(pop2019)*1000) %>%
  arrange(desc(death_rate))
## Non-hispanic Black has the highest overall death rate for COVID-19 at 2.85

## Looked at overall total death rates by race
combined %>% filter(!is.na(total_deaths)) %>% 
  group_by(race_and_hispanic_origin_group) %>%
  summarise(death_rate = sum(total_deaths)/sum(pop2019)*1000) %>%
  arrange(desc(death_rate))
## Non-hispanic white has the highest overall death rate at 20.4

## Looked at overall pneumonia death rates by race
combined %>% filter(!is.na(pneumonia_deaths)) %>% 
  group_by(race_and_hispanic_origin_group) %>%
  summarise(death_rate = sum(pneumonia_deaths)/sum(pop2019)*1000) %>%
  arrange(desc(death_rate))
## Non-hispanic Black has the highest overall death rate at 2.2

## Looked at overall influenza death rates by race
combined %>% filter(!is.na(influenza_deaths)) %>% 
  group_by(race_and_hispanic_origin_group) %>%
  summarise(death_rate = sum(influenza_deaths)/sum(pop2019)*1000) %>%
  arrange(desc(death_rate))
## Non-hispanic White has the highest overall death rate at .0457

## Influenza unsurprisingly has extremely low death rates. 
## Black people have a higher pneumonia death rate, but only by .03. 
## Overall death rate for the time period in the data set is interesting. White, Black and AIAN people are on the higher end 
## for death rates, while Hispanic, Native Hawaiian or other Pacific Islander and Asian people are lower. 
## I think for overall death rate, it would be interesting to compare it over a time period — at least a decade. 
## Non-hispanic Black people have the highest rate for COVID-19 deaths in the country. 

## Looking at COVID-19 death rate by state

combined %>% filter(!is.na(covid_19_deaths)) %>% 
  group_by(state) %>%
  summarise(death_rate = sum(covid_19_deaths)/sum(pop2019)*1000) %>%
  arrange(desc(death_rate))

## North Dakota has the highest rate of COVID-19 deaths at 3.78.
## Other states with highest rates are New Jersey, Alabama and Connecticut. 

## I want to look at North Dakota's COVID-19 death rate broken down by race.
## North Dakota COVID-19 death rates by race
combined %>% filter(!is.na(covid_19_deaths) & state == "North Dakota") %>% 
  group_by(race_and_hispanic_origin_group) %>%
  summarise(death_rate = sum(covid_19_deaths)/sum(pop2019)*1000) %>%
  arrange(desc(death_rate))
## Non-hispanic American Indian or Alaska Native has the highest COVID-19 death rate in North Dakota at 4.26.
## Non-hispanic white is second at 4.06

## National COVID-19 death rate for AIAN people
combined %>% filter(!is.na(covid_19_deaths) & race_and_hispanic_origin_group == "Non-Hispanic American Indian or Alaska Native") %>% 
  group_by(race_and_hispanic_origin_group) %>%
  summarise(death_rate = sum(covid_19_deaths)/sum(pop2019)*1000) %>%
  arrange(desc(death_rate))
## 2.3
## I saw earlier in my analysis that this is the third highest COVID-19 death rate in the U.S.

## How does this compare to the overall COVID-19 death rate in the U.S.?
## Overall COVID-19 death rate in the U.S.
combined %>% filter(!is.na(covid_19_deaths)) %>%
  summarise(death_rate = sum(covid_19_deaths)/sum(pop2019)*1000) 
## The death rate for COVID-19 in the U.S is 2.36, so it's lower. 

## For this next query, I want to see COVID-19 death rates broken down by race and state, listed from highest to lowest
## I created a table so I can see all of the results. 
## My goal for this query is to see if the rates are higher for any particular race in different states.

## COVID-19 death rate for each race by state ordered from highest to lowest
state_race <- combined %>% filter(!is.na(covid_19_deaths)) %>%
  group_by(state, race_and_hispanic_origin_group) %>%
  summarise(death_rate = sum(covid_19_deaths)/sum(pop2019)*1000) %>%
  arrange(desc(death_rate))
## The highest COVID-19 death rate is Non-Hispanic American Indian or Alaska Native people in New Mexico 
## with a death rate of 9.23
## This is almost four times the death rate of the overall COVID-19 death rate in the U.S (2.36). 

## How does the high COVID-19 death rate of AIAN people in New Mexico compare to other races in the state?
combined %>% filter(!is.na(covid_19_deaths) & state == "New Mexico") %>%
  group_by(race_and_hispanic_origin_group) %>%
  summarise(death_rate = sum(covid_19_deaths)/sum(pop2019)*1000) %>%
  arrange(desc(death_rate))
## It's still extremely high compared to other races. The second highest is Hispanic at 2.44, then white at 2.3, then Black at 1.69.
## Asian and Native Hawaiian or Other Pacific Islander rates are zero in New Mexico. 

## The next three queries I calculated the death rates for three races showing the highest to lowest rate by state.
## I wanted to compare the highest death rates for white, Black and AIAN people and also see which states they are in. 

combined %>% filter(!is.na(covid_19_deaths) & race_and_hispanic_origin_group == "Non-Hispanic American Indian or Alaska Native") %>%
  group_by(state) %>%
  summarise(death_rate = sum(covid_19_deaths)/sum(pop2019)*1000) %>%
  arrange(desc(death_rate))
## Still highest in New Mexico, Arizona is second at 7.37.

combined %>% filter(!is.na(covid_19_deaths) & race_and_hispanic_origin_group == "Non-Hispanic Black") %>%
  group_by(state) %>%
  summarise(death_rate = sum(covid_19_deaths)/sum(pop2019)*1000) %>%
  arrange(desc(death_rate))
## The highest death rate is 5.68 in D.C.

combined %>% filter(!is.na(covid_19_deaths) & race_and_hispanic_origin_group == "Non-Hispanic White") %>%
  group_by(state) %>%
  summarise(death_rate = sum(covid_19_deaths)/sum(pop2019)*1000) %>%
  arrange(desc(death_rate))
## The highest death rate is 4.06 in North Dakota

## I'd like to look at D.C. data since that has come on the radar a couple of times. 

combined %>% filter(!is.na(covid_19_deaths) & state == "District of Columbia") %>%
  summarise(death_rate = sum(covid_19_deaths)/sum(pop2019)*1000)
## The COVID-19 death rate in D.C. is 3.32.

combined %>% filter(!is.na(covid_19_deaths) & state == "District of Columbia") %>%
  group_by(race_and_hispanic_origin_group) %>%
  summarise(death_rate = sum(covid_19_deaths)/sum(pop2019)*1000) %>%
  arrange(desc(death_rate))
## The COVID-19 death rate for Black people in D.C. is 5.68. For Hispanic people, it's 4.58.
## For Asian people, it's 1.83. For white people, it's .991. 
## I think this is really interesting. The death rate for Black people in D.C. is over 5 times the COVID-19 death rate of
## white people in D.C. Does this have to do with class? White politicians? 

## Story Pitch

## This story will be about the significantly higher rates of COVID-19 deaths of indigenous people in the United States. 
## The COVID-19 death rate for non-Hispanic American Indian or Alaskan Native people in New Mexico is 9.23 deaths per 1,000 people.
## This rate is the highest in the country when breaking down deaths by race within different states and is
## over three times the rate of Hispanic deaths, which is 2.44, and four times the rate of white deaths, 2.3, in the state. 
## This rate is also significantly higher than national rates — with the death rates for all race categories being 
## between 1 and 3. 


















