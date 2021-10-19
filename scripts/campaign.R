library(tidyverse)

# fix the paths below as needed:
contrib <- read_csv("data/mo_zip/mo_contributions.csv")
cands <- read_csv("data/mo_zip/mo_candidates.csv")
comms <- read_csv("data/mo_zip/mo_committees.csv")


## include transformations and cleaning at the top of data


## counts the different types below
contrib %>% count(transaction_tp)

## create new date column and put it right after the old date in the data
contrib <- contrib %>% mutate(new_date = as.Date(transaction_dt, "%m%d%Y"), .after=transaction_dt) 

#type   description
#-------------------
# 15   | contrib from an individual
# 15C  | candidates contributing to themselves
# 15E  | earmarked from an intermediary
# 22Y  | refund to an individual
# 24K  | donation from a PAC to a candidate

### when transaction_tp == "24K", the filer (cmte_id) is a PAC giving to a candidate's principal campaign committee (other_id)
### for everything else, the filer (cmte_id) is the candidate's principal campaign committee, receiving individual contributions

### cmte_id 
### pcc 
### cand_id 
### other id

## candidates getting money from pacs
### 24K cands, other id to pcc

### cands getting individual contributions
## 15, 15E, 22Y, cands, cmte id to pcc

## which pac is giving the most money
## 24K, comms, ONLY cmte id b/c it's the same



## total contribution amount
contrib %>% summarise(total = sum(transaction_amt))
## 7,945,192

## filtering for certain transaction types
contrib %>% filter(transaction_tp %in% c("15", "15E", "22Y")) %>%
  summarise(total = sum(transaction_amt))
## 6,262,923

## PAC money
contrib %>% filter(transaction_tp == "24K") %>%
  summarise(total = sum(transaction_amt))
## 1,658,609

## candidate contributing to themselves
contrib %>% filter(transaction_tp == "15C") %>%
  summarise(total = sum(transaction_amt))
## 23,660

## Which Missouri candidate is getting the most individual contributions?

contrib %>% inner_join(cands, by=c("cmte_id"="pcc")) %>%
  filter(transaction_tp %in% c("15", "15E", "22Y")) %>%
  group_by(cand_name) %>%
  summarise(total = sum(transaction_amt)) %>%
  arrange(desc(total))

## Josh Hawley, $2,129,519


## Which candidates are getting the most PAC money

contrib %>% inner_join(cands, by=c("other_id"="pcc")) %>%
  filter(transaction_tp == "24K") %>%
  group_by(cand_name) %>%
  summarise(total = sum(transaction_amt)) %>%
  arrange(desc(total))


## what is the time range

contrib %>% summarise(range = range(new_date))
# or
range(contrib$new_date)

## 2019-11-08 to 2021-09-30


## which candidates are giving money to themselves

contrib %>% inner_join(cands, by=c("cmte_id"="pcc")) %>%
  filter(transaction_tp == "15C") %>%
  group_by(cand_name) %>%
  summarise(total = sum(transaction_amt)) %>%
  arrange(desc(total))

## eric greitens is giving himself the most money.

## which PAC/company is giving the most money

contrib %>% inner_join(comms, by="cmte_id") %>%
  filter(transaction_tp == "24K") %>%
  group_by(cmte_nm) %>%
  summarise(total = sum(transaction_amt)) %>%
  arrange(desc(total))

## Winred, $91,708


## class assignment 10/18 -------------------------------

## which cand is WINRED supporting the most?

contrib %>% inner_join(cands, by=c("other_id"="pcc")) %>% 
  filter(transaction_tp == "24K") %>% 
  filter(cmte_id == "C00694323") %>% 
  group_by(cand_name) %>% 
  summarise(total = sum(transaction_amt)) %>% 
  arrange(desc(total))

contrib %>% inner_join(comms, by="cmte_id") %>%
  filter(transaction_tp == "24K") %>%
  group_by(cmte_nm) %>%
  summarise(total = sum(transaction_amt)) %>%
  arrange(desc(total))

## Josh Hawley got the most from WINRED at $28,160
## The total from WINRED was $91,708

## which pacs are supporting Hawley the most?

contrib %>% inner_join(cands, by=c("other_id"="pcc")) %>% 
  filter(transaction_tp == "24K") %>% 
  filter(cand_id == "S8MO00160") %>%
  group_by(cand_name, cmte_id) %>%
  select(transaction_amt, cmte_id) %>% 
  summarise(sum = sum(transaction_amt)) 

## $28,160 from WINRED
## COMMUNITY BANCSHARES OF MISSISSIPPI INC. POLITICAL ACTION COMMITTEE with $5,000
## SENATE CONSERVATIVES FUND with $5,000
## MISSOURI SOYBEAN ASSOCIATION POLITICAL ACTION COMMITTEE (MO SOYPAC) with $2,900

## which pacs are contributing the most to eric greitens?
contrib %>% inner_join(cands, by=c("other_id"="pcc")) %>% 
  filter(transaction_tp == "24K") %>% 
  filter(cand_id == "S2MO00536") %>%
  group_by(cand_name, cmte_id) %>%
  select(transaction_amt, cmte_id) %>% 
  summarise(sum = sum(transaction_amt)) 
## winred with $14,005

## which pacs are contributing the most to ann wagner?

contrib %>% inner_join(cands, by=c("other_id"="pcc")) %>% 
  filter(transaction_tp == "24K") %>% 
  filter(cand_id == "H2MO02102") %>%
  group_by(cand_name, cmte_id) %>%
  select(transaction_amt, cmte_id) %>% 
  summarise(sum = sum(transaction_amt)) %>%
  arrange(desc(sum))

## winred with $25589

## What individual  outside of Missouri has given the most money (not PAC) To which candidate?

temp1 <- contrib %>% filter(transaction_tp %in% c("15", "15E", "22Y") & state != "MO") %>%
  group_by(name) %>%
  select(name, state, transaction_amt, other_id, cmte_id) %>%
  group_by(name) %>%
  arrange(desc(transaction_amt))

## Lisa Burrell fro Florida has donated $23,200 to Ann Wagner for congress. 

## Highest individual contributions total from columbia? which candidate?

contrib %>% inner_join(cands, by=c("cmte_id"="pcc")) %>%
  filter(transaction_tp %in% c("15", "15E", "22Y") & city.x == "COLUMBIA") %>%
  select(cand_name, transaction_amt) %>%
  group_by(cand_name) %>%
  summarise(sum = sum(transaction_amt)) %>%
  arrange(desc(sum))

## Josh Hawley has received $65,542 from donors in Columbia
  
## What cities has Josh Hawley received the most individual contributions from?

contrib %>% inner_join(cands, by=c("cmte_id"="pcc")) %>%
  filter(transaction_tp %in% c("15", "15E", "22Y") & cand_id == "S8MO00160") %>%
  select(city.x, transaction_amt) %>%
  group_by(city.x) %>%
  summarise(sum = sum(transaction_amt)) %>%
  arrange(desc(sum))
  
## Saint Louis - $171,486
## Springfield - 83,985

## What about Vicky Hartzler?

contrib %>% inner_join(cands, by=c("cmte_id"="pcc")) %>%
  filter(transaction_tp %in% c("15", "15E", "22Y") & cand_id == "S2MO00593") %>%
  select(city.x, transaction_amt) %>%
  group_by(city.x) %>%
  summarise(sum = sum(transaction_amt)) %>%
  arrange(desc(sum))

## Springfield, $35,500

## What about Eric Greitens?

contrib %>% inner_join(cands, by=c("cmte_id"="pcc")) %>%
  filter(transaction_tp %in% c("15", "15E", "22Y") & cand_id == "S2MO00536") %>%
  select(city.x, transaction_amt) %>%
  group_by(city.x) %>%
  summarise(sum = sum(transaction_amt)) %>%
  arrange(desc(sum))

## Most of the cities in the Eric Greitens top ten locations he gets indiviudal contributions from are not in Missouri.
## Naples is number one

## PAC in Missouri that gave the most money?

contrib %>% inner_join(comms, by="cmte_id") %>%
  filter(transaction_tp == "24K" & st == "MO") %>%
  select(cmte_nm, transaction_amt) %>%
  group_by(cmte_nm) %>%
  summarise(total = sum(transaction_amt)) %>%
  arrange(desc(total))

## Anheuser Busch - $14,000

  
### SHORT PITCH

### The candidate supported the most by individual campaign contributions in Columbia is Senator Josh Hawley, R-MO, 
### who isn't up for reelection until 2024. Hawley has made $65,542 from individual contributions in Columbia. The second
### highest candidate is Eric Schmitt with $42,100, then Vicky Hartzler with $12,450. Hawley has also made the most in
### individual donations throughout the state of Missouri — making $2,129,519. Again, Eric Schmitt has made the second largest
### amount in campaign contributions at $1,236,119. 











