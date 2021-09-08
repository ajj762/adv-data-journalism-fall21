library(tidyverse)

MLB <- read_csv("/data/MLB_Salaries_1.csv")
MLB <- read_csv("adv-data-journalism-fall21/data/MLB_Salaries_1.csv")
MLB <- read_csv("/data/MLB_Salaries_1.csv")

# piping
# MLB %>% arrange(Salary)

MLB <- read_csv("MLB_Salaries_1.csv")

# RIGHT ONE ROW 15

MLB <- read_csv("data/MLB_Salaries_1.csv")

MLB %>% arrange(Salary)

MLB %>% select(Name, Salary) %>% arrange(Salary)

class(MLB$Salary) #by pulling out one column, it creates a vector
