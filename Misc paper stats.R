##  Misc Paper statistics
##  This is the part where we put in how we got specific numbers in our
##  paper

library(tidyverse)
library(Hmisc)

##  1) Female clerk job descriptions and break downs of hires as boys for males
##  As per referee comment
# Pages 5-6. Very good information is given on employment of males clerks at Coats for the period. However no numbers are given for males as they are for females. Could this please be provided.
# In addition, did Coats follow the practice of employing boy clerks at the age of 14 and then dismissing many of them at the age of 20 as was common in many offices in Britain and did this practice stop with the employment of women? This is extremely important as regards average tenure.


load(file='Generated data/cleaned long form coats.Rdata')


##  1a) Female clerks -----
femaleClerks_df <- 
  wages.long %>% 
  filter(Location=='Glasgow') %>%
  filter(Gender=='F')

femaleClerks_df$salary


femaleSummary <-
  femaleClerks_df %>% 
  mutate(age = year - Birth.Year) %>%
  group_by(Name) %>%
  summarise(job = Job[1] %>% tolower,
            startAge = min(age),
            startSalary = min(salary %>% as.numeric),
            obs = !is.na(salary) %>% sum
  )

femaleSummary %>% nrow # 645 total recorded
femaleSummary$job %>% table ## 54 as typists, 38 as deissections, 23 as Stocks
femaleSummary$startAge %>% summary ## IQ range but hard to quantify whether many leave
femaleSummary$startSalary %>% summary ## starting generally 39

jobSummary <-
  femaleSummary %>%
  group_by(job) %>%
  summarise(medAge = startAge %>% median(na.rm = T),
            medSalary = startSalary %>% median(na.rm = T)
            )
## Check table but overall very little variety in recording of what females did (could represent homogeneity in jobs)

##  1b) 'boys' ----
maleClerks_df <- 
  wages.long %>% 
  filter(Location=='Glasgow') %>%
  filter(Gender=='M')


maleSummary <-
  maleClerks_df %>% 
  mutate(age = year - Birth.Year) %>%
  group_by(Name) %>%
  summarise(job = Job[1] %>% tolower,
            startAge = min(age),
            endAge = max(age),
            startYear = min(year),
            obs = salary %>% length
  )

maleSummary %>% nrow # 645 total recorded
maleSummary$job %>% table ## 54 as typists, 38 as deissections, 23 as Stocks
maleSummary$startAge %>% summary ## IQ range but hard to quantify whether many leave
maleSummary$startYear %>% summary ## starting generally 39

boySummary <-
  maleSummary %>%
  filter(job == 'boy')

boySummary$startYear %>% table #Coats only really hired boys (year on year) between 1905 - 1916 before and after these dates
# it is literally 1-3 every few years (1889 -1902 is only 6)
##  1905 - 1916 hired 41
##   only 54 in the whole history

boySummary$startAge %>% table ## coats did hire boys under 18
boySummary$endAge %>% table # 22 were dismissed prior to 21



jobSummary <-
  femaleSummary %>%
  group_by(job) %>%
  summarise(medAge = startAge %>% median(na.rm = T),
            medSalary = startSalary %>% median(na.rm = T)
  )
