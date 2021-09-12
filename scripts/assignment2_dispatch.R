#### ASSIGNMENT 2 ####

# Answer the following questions using the dispatch.csv file 
# and the tidyverse functions we've learned 

library(tidyverse)

getwd()

setwd("/Users/anniejennemann/Documents/Github/Repositories/adv-data-journalism-fall21")

dis <- read.csv("dispatch.csv")

# Question 1: What's the timeframe that this dataset covers?  

# Your code: 

dis %>% summarise(time_frame = range(CallDateTime))

# The answer

# 8/23/2021 at 1:02:08 p.m. to 8/29/2021 at 9:58:38 a.m.

# Question 2: Which day of the week had the most incidents? 

# Your code: 

dis %>% group_by(DOW) %>% summarise(count_groups = n()) %>% arrange(desc(count_groups))

# The answer: 

# Tuesday had the most incidents at 342.

# Question 3:  Which hour of the day had the most incidents?

# Your code: 

dis %>% group_by(Hour) %>% summarise(count_hour = n()) %>% arrange(desc(count_hour))

# The answer: 

# The 13th hour (1 p.m.) had the most incidents at 118.

# Question 4:  Which police district had the most traffic stops?

# Your code: 

dis %>% filter(ExtNatureDisplayName == "TRAFFIC") %>% group_by(PolArea) %>% summarise(count_traffic = n()) %>% arrange(desc(count_traffic))

# The answer:

# The 10W police district had the most traffic stops at 4. 

# Question 5:  What's the most common reason police are dispatched to the airport? (11300 S AIRPORT DR)

# Your code: 

dis %>% filter(Address == "11300 S AIRPORT DR") %>% group_by(ExtNatureDisplayName) %>% summarise(count_reason = n()) %>% arrange(desc(count_reason))

# The answer:

# The most common reason police are dispatched to the airport is to "check area" with 53 dispatches. 

