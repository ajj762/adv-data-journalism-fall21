library(tidyverse)
library(janitor)

crimes <- read_csv("data/Offenses_Known_to_Law_Enforcement_by_State_by_City_2019.csv")

# pipes thru clean names and rewrites the variable 
crimes <- crimes %>% clean_names()

# REVIEW
crimes %>% select(state, city, population) %>% arrange(desc(population))

crimes %>% 
  filter(state == "NEW YORK") %>% 
  select(state, city, population, violent_crime) %>% 
  arrange(desc(violent_crime))

crimes %>% group_by(state) %>% 
  summarise(sum_pop = sum(population)) %>% arrange(desc(sum_pop))

# NEW

crimes %>% filter(population > 100000) %>%
  mutate(murder_rate = murder_and_nonnegligent_manslaughter/population*100000) %>%
  select(state, city, murder_and_nonnegligent_manslaughter, population, murder_rate) %>%
  arrange(desc(murder_rate))

# IN-CLASS PRACTICE  

# Question 1, CORRECT

crimes %>% filter(population > 100000) %>%
  mutate(violent_crimes_rate = violent_crime/population*100000) %>%
  select(state, city, violent_crime, population, violent_crimes_rate) %>%
  arrange(desc(violent_crimes_rate))

# Detroit, Michigan with 1965/100000 violent crimes

# Question 2 How many motor vehicle thefts have there been in total?

crimes %>% filter(!is.na(motor_vehicle_theft)) %>%
  summarise(motor_theft_total = sum(motor_vehicle_theft))

# 509055 total motor vehicle thefts.

  # != means NOT equal to. !is.na does "does not equal na"

# Question 3 How many cities have more than 50 murders?

crimes %>% filter(murder_and_nonnegligent_manslaughter > 50) %>%
  summarise(number_cites = n())

# 34 cities have over 50 murders
