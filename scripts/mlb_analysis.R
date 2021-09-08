# call the packages you want to use
library(tidyverse)

# set the working directory to the repository
setwd("/Users/anniejennemann/Documents/Github/Repositories/adv-data-journalism-fall21")

# load mlb data
MLB <- read.csv("Data/MLB_Salaries_1.csv")

# %>% called a pipe, takes output of one function and pipes it into next
# arrange is like order by in sql, default for arrange is ascending

class(MLB)
MLB %>% select(Name, Salary) %>% arrange(desc(Salary))
MLB %>% arrange(desc(Tenure))

MLB %>% filter(Team == "Kansas City Royals") %>% arrange(desc(Salary))

MLB %>% filter(POS == "SS") %>% arrange(desc(Salary))
MLB %>% filter(Tenure == 1)

# count, don't include a number b/c it's just a count

MLB %>% summarise(num = n())

MLB %>% summarise(total_salary = sum(Salary))
MLB %>% summarise(avg_salary = mean(Salary))

#group by
teams <- MLB %>% group_by(Team) %>% summarise(total_salary = sum(Salary))


