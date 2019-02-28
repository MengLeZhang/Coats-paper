##  Coates data cleaning -----                  
##  Restarted : 16/5/2018
##  Goal: The original coats data is in longform and excludes senior staff. 
##  Everyone noted down is presumed to be a clerk as some point of data reporting
##  There errors in commencement data caused by genuine errors or missing salary
##  book
##  End goal is to get the data in long form with a tenure variable

##  Step one: Load in the data and get rid of useless columns ----
library(tidyverse)
coats.path <- 'Raw data/Glasgow excl senior.csv'

coats.raw <-
  read.csv(coats.path,
           na.strings = c('', 'NA'),
           stringsAsFactors = F)
summary(coats.raw) # Note the extra col
coats.raw$X<-NULL #extra no good col

##  Checks and comments
table(coats.raw$Commenced) #okay well if we jsut turn it into numeric then it ought to NA invalid values
table(coats.raw$Gender) #okay so this is of good quality
table(coats.raw$Job) #we would need to perhaps export this table to pdf and recode if we wanted just clerks; or use grep
table(coats.raw$Location) #okay this is good although let's just use glasgow for now (it is easily 80-90% of their workforce)



##  Step two: Create unique ID; turned commenced into numerical; leave salary as ----
##  it is
coats <- coats.raw %>%
  mutate(ID = 1:nrow(coats.raw), #replacing NA for names with a number
         Commenced = Commenced %>% as.numeric %>% replace_na(9999) ## replace NA with a large positive year
         #will introduce NAs for invalid non-numeric commencement date
  )


##  Step three: Wide to longform ----
sel.var <- c(paste('X', 1889:1930, sep = '')) #select wideform vars
sel.col <- which(names(coats.raw) %in% sel.var) # their cols

##  Make into long form
wages.long <- 
  coats[, c('ID', sel.var)] %>% gather(year, salary, - ID) #wide2long

wages.long <- 
  wages.long %>% left_join(coats.raw[, -1 * sel.col], by = 'ID') #merge with original to get time invariant data

wages.long <- 
  wages.long %>%
  mutate(year = year %>% gsub('X', '', x = .) %>% as.numeric) %>% # wages into numeric
  arrange(ID, year)##order by id and year

wages.long %>% head

##  Step four: Fixing missingness in salary ----
##  Salary currently has non-numerical values such as 'left' which should be NA
##  record for that year. It has others such as 'Russia' where basically a person
##  was still employed by coats and this record should contribute to tenure.
##  We must basically distinguish whether they had a genuine year at work or not

##  Load Cleaned salary values.csv which tells us whether a value is treated as
##  NA or not. Then created col called not.recorded which indexes whether 
##  a record doesn't count towards service at coats

##  There are times when they were enlist and we need to count these as contributing
##  to tenure / salary

##  Load in data and find index where there is a na value
cleaned.sal <- 'Cleaned salary values.csv' %>% read.csv
more.na <- cleaned.sal$Var1 %>% subset(cleaned.sal$is_na == T)


### For military enlistment it's a tad more complicated
more.na
wages.long %>% filter(salary == 'enlisted') # we can see that

## Enlistment
test <- 
  wages.long %>%
#  group_by(ID) %>%
  mutate(enlist_year = ifelse(salary == 'enlisted', year, NA)) %>%
  group_by(ID) %>%
  mutate(first_year = enlist_year %>% min(na.rm = T),
         salary.test = ifelse(is.na(salary) & (year > first_year & year < 1920),
                              'enlisted', salary)) #%>%
test %>% filter(ID == 33) %>% tail

##  Then create not.recorded and filter out NA from wages.long
wages.long$not.recorded <- is.na(wages.long$salary)
wages.long$not.recorded[wages.long$salary %in% more.na] <- T

wages.long <- wages.long %>% subset(not.recorded == F) # restrict to only recorded
wages.long

##  Step five: Sort out issues with commenced and first pay years ----
##  Basically for some records their first pay and commencement years are not 
##  the same.
##  We do 2 approaches to create a variable called base1/2 which tells us how many 
##  years of service they did before their first record sal incidence
##  First we substract first pay year from commenced then...
##  1) The diff is negative (i.e. commenced after 1st record) then set base1 to
##  0. Else set bas to the difference between commencement and first recorded sal
##  2) Base 1 has lots of records where the diff is huge (like 43 years). For
##  these records we might have very poor idea of tenure so we restrict data
##  to those where the absolute difference between start and comment is under 5.
##  One exception is if their first pay is in 1889 as their commencement can be
##  many years prior in one of the companies that make up Coats.


##  Table of first pay and commenced
check.begin <- aggregate(year ~ Name + ID + Commenced, wages.long, min)
check.begin #interesting 
check.begin$diff <- check.begin$year - check.begin$Commenced # many cases where
## the first year they commenced is not the first year they had a recorded sal

check.begin$diff %>% table # most cases are only 1 or 2 year out which shouldn't
##  make too much diff -- some are WAY out;

##  Create base1 variable
check.begin$base1 <- ifelse(check.begin$diff <= 0, 0, check.begin$diff) #we did this in the past 

##  Base2 variable
##  Select those who had a record sal in 1889
pre.coats <- (check.begin$year == 1889)
check.begin$base2 <- check.begin$base1
check.begin$base2[!(pre.coats | {abs(check.begin$diff) <= 4})] <- NA #189 records

##  Add the base1 and base2 variable (as well as diff) to the longcoats
wages.long <- wages.long %>% 
  merge(check.begin[, c('ID', 'diff', 'base1', 'base2')], by = 'ID')

##  Step six: Caclulate tenure variables
##  Tenure is a count of how many years of service plus their base

##  Create a table of IDs and how many times they appear then make vector 
##  counting appearances
IDs.tab <- aggregate(year ~ ID, wages.long, length)
count <- list(NULL)
for (i in 1:nrow(IDs.tab)){
  count[[i]] <- 0 : (IDs.tab$year[i] - 1)
}
wages.long$tenure.count <- unlist(count)
wages.long$tenure1 <- wages.long$tenure.count + wages.long$base1
wages.long$tenure2 <- wages.long$tenure.count + wages.long$base2

##  Step 7: Now turn the salary variable into a numeric variable which is cost of ----
##  living adjusted salary

f.index<-read.csv('Data/Coats Mar 2017/F index.csv',na.strings =c('','NA'), stringsAsFactors = F  )
names(f.index) <- names(f.index) %>% tolower
wages.long <- wages.long %>% merge(f.index)
wages.long$f.salary <- as.numeric(wages.long$salary) / wages.long$index * 100
wages.long <- wages.long[order(wages.long$ID, wages.long$year), ] #order by id and year

##  Final step: Save this data ----
save(wages.long, file='Data/Coats May 2018/cleaned long form coats.Rdata')

