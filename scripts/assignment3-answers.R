## LOAD TIDYVERSE AND THE DATA FILE
library(tidyverse)
library(janitor)

balt <- read_csv("data/Baltimore_City_Employee_Salaries.csv")
balt <- balt %>% clean_names()

## INTEGRITY CHECKS

# Is any individual in here twice? Why?

balt %>% group_by(name) %>% 
  summarise(name_count = n()) %>% 
  arrange(desc(name_count))

# Yes, multiple individuals are in here more than once.

# How many years of data do we have?

balt %>% summarise(time_range = range(fiscal_year))

# 10 years of data from FY2011 to FY2020.

# What's the min and max annual salary given out in 2020? 

balt %>% filter(fiscal_year == "FY2020") %>%
  summarise(salary_range = range(annual_salary))
  
# The minimum salary was $0 and the maximum salary was $275,000 in FY2020.

# What jobs get paid $0?

balt %>% filter(annual_salary == "0") %>%
  group_by(job_title) %>%
  summarise(jobs = n())

# Aide Blue Chip, Community Health Nurse I and II, Election Judge (Regular), 
# Election Judges Regular, Recreation Leader II and School Health Aide.

# How clean are the job titles? Are there a lot of duplicates? 

jobtitles <- balt %>% count(job_title)

# Job titles are not clean at all. There are a lot of duplicates and differences with upper and lower case letters.

# Clean up the JobTitles by making everything lowercase 
# (hint: use mutate to overwrite the current JobTitle field, using the function str_to_lower())

balt <- balt %>% mutate(job_title = str_to_lower(job_title))

# Take a look at agency names; how clean are they? 

agency <- balt %>% count(agency_name)

# Agency names are also not clean. Some names are cut off and a lot have a three digit number at the end. 
# The three digit number could be a code for a specific department, I would have to ask the city. 
# Also, not all agency names have a three digit number at the end.

balt <- balt %>% mutate(agency_name = str_to_lower(agency_name))

## QUESTIONS OF THE DATA

# Who's the highest paid employee in FY2020?

balt %>% filter(fiscal_year == "FY2020") %>%
  select(name, annual_salary) %>%
  arrange(desc(annual_salary))

# Michael S Harrison was the highest paid employee earning an annual salary of $275,000.

# Which job title has the highest average salary in 2020? (hint: use mean() )

balt %>% filter(fiscal_year == "FY2020") %>%
  group_by(job_title) %>%
  summarise(avg_salary = mean(annual_salary)) %>%
  arrange(desc(avg_salary))

# The job title police commissioner has the highest average salary for 2020 at $275,000.

# Any potential problems with citing these results? 

# Yes, many of the top average salaries are for roles like police commissioner, state's attorney or even mayor.
# These are roles only one person holds so calculated the average salary for the role is just the salary for
# the one person who hold the position during FY2020.

# How many people work in the police department in 2020? 

police <- balt %>% filter(grepl("police", agency_name)) %>%
  count(agency_name)

balt %>%  filter(fiscal_year == "FY2020") %>%
  filter(grepl("police", agency_name)) %>% 
  count()

# In 2020, 3,209 people worked in the police department.

# How many are "police officers"? 

police_titles <- balt %>% filter(grepl("police", job_title)) %>%
  count(job_title)

balt %>% filter(fiscal_year == "FY2020") %>%
  filter(grepl("police", agency_name)) %>%
  filter(grepl("officer", job_title)) %>%
  count()

# 2,126 are police officers.

# What was their total salary?

balt %>% filter(fiscal_year == "FY2020") %>%
  filter(grepl("police", agency_name)) %>%
  filter(grepl("officer", job_title)) %>%
  summarise(total_salary = sum(annual_salary))

# Their total salary was $161,895,614.

