##  Replication of Heller and co 2014
##  They used subsets to control for tenure and ANOVA tests for year
##  Start: 14/5/2018

library(data.table)
library(lme4)
library(tidyverse)
library(ggplot2)

##  Step 1) Load in the cleaned data ----
##  The object is called wages.long
load(file='Data/Coats May 2018/cleaned long form coats.Rdata')

##  Step 1b) Coding up the periods; creating 10 year tenure bands
##  Periods
##  Putting x in front of period to make the last period the reference
##  This is for when the model drops rank deficient interactions later
names(wages.long); head(wages.long)
wages.long$period <- ifelse(wages.long$year %in% 1889:1900, 'x1889 - 1900',
                          ifelse(wages.long$year %in% 1901:1913, 'x1901 - 1913',
                                 ifelse(wages.long$year %in% 1914:1918, 'x1914 - 1918', '1919 - 1930')))

## Make bands for subsets of tenure
wages.long$tenureband1 <- ifelse(wages.long$tenure1 <= 9, '0 - 9',
                                ifelse(wages.long$tenure1 <= 19, '10 - 19', 
                                       '20 plus'))

wages.long$tenureband2 <- ifelse(wages.long$tenure2 <= 9, '0 - 9',
                                 ifelse(wages.long$tenure2 <= 19, '10 - 19', 
                                        '20 plus'))

## We can simply for loop our analysis for women and men

##  Step two: Calculate the F statistic for models with and without years after
##  accounting for tenure. We need to take into account the random effects
table(wages.long$tenureband1, wages.long$period)
## Female wages only for after 1901
df.m <- wages.long %>% subset(Location=='Glasgow' & Gender=='M')
df.f <- wages.long %>% subset(Location=='Glasgow' & Gender=='F' & period != 'x1889 - 1900')


##  Male F test for year given tenure
mod1am <- lmer(f.salary ~ (1|ID) + tenure1, df.m)
mod1bm <- lmer(f.salary ~ (1|ID) + tenure1 + as.factor(year), df.m)
anova(mod1am, mod1bm) # F ratio test

##  Female F test for year given tenure
mod1af <- lmer(f.salary ~ (1|ID) + tenure1, df.f)
mod1bf <- lmer(f.salary ~ (1|ID) + tenure1 + as.factor(year), df.f)
anova(mod1af, mod1bf) # F ratio test

##  Robustness F tests
mod2am <- lmer(f.salary ~ (1|ID) + tenure2, df)
mod2bm <- lmer(f.salary ~ (1|ID) + tenure2 + as.factor(year), df)
anova(mod2am, mod2bm) # F ratio test

##  Step three: The effects of tenure over time ----


mod3am <- lmer(f.salary ~ (1|ID) + (1|year) + period + tenureband1 * tenure1, df.m)

mod3bm <- lmer(f.salary ~ (1|ID) + (1|year) + tenureband1 * period * tenure1, df.m)
anova(mod3am, mod3bm) #yes for tenure period effect

mod3bm %>% summary

mod3af <- lmer(f.salary ~ (1|ID) + (1|year) + period + tenureband1 * tenure1, df.f)
mod3bf <- lmer(f.salary ~ (1|ID) + (1|year) + tenureband1 * period * tenure1, df.f)
anova(mod3af, mod3bf) #yes for tenure period effect

## Once again robustness tests using tenure2
mod4am <- lmer(f.salary ~ (1|ID) + (1|year) + period + tenureband2 * tenure2, df.m)
mod4bm <- lmer(f.salary ~ (1|ID) + (1|year) + tenureband2 * period * tenure2, df.m)
anova(mod4am, mod4bm) #yes for tenure period effect

mod4af <- lmer(f.salary ~ (1|ID) + (1|year) + period + tenureband2 * tenure2, df.f)
mod4bf <- lmer(f.salary ~ (1|ID) + (1|year) + tenureband2 * period * tenure2, df.f)
anova(mod4af, mod4bf) #yes for tenure period effect

##  Write out the model
summary(mod3bm)$coef %>% write.csv('./Results/Raw model results male.csv')
summary(mod3bf)$coef %>% write.csv('./Results/Raw model results female.csv')


##  Step four: Table of tenure effects -----
## As it currently stands the tables are quite hard to understand due to the
##  interaction effect. We will extract out the slope effects using a table
s##  template.

##  This routine we accept in a table template 

template.tab <- './Data/Coats May 2018/Template of interactions.csv' %>% read.csv

int.tab <- function(mod, x, template){
temp <- template    
temp$coef.nms <- NA

## Create a routine for reading in interactions and calculating the se and beta
for(i in 1:nrow(temp)){
mod.nms <- mod@vcov_beta %>% rownames
a <- mod.nms[grep(x, mod.nms)]
b <- a[grep(template.tab$tenureband[i], a)]
c <- b[grep(template.tab$period[i], b)]
d <- a[grep(template.tab$period[i], a)]

temp$coef.nms[i] <- paste(a[1], b[1], c[1], d[1])
ind <- which(mod.nms %in% c(a[1], b[1], c[1], d[1]))
temp$slope[i] <- mod@beta[ind] %>% sum
temp$se[i]  <- vcov(mod)[ind, ind] %>% sum %>% sqrt
}
return(temp)
}

##  Results and saving

int.tab(mod = mod3bm, x = 'tenure1', template.tab) %>% write.csv('./Results/Neater tenure male.csv')# 20+ for 1889 is just wrong
int.tab(mod = mod3bf, x = 'tenure1', template.tab) %>% write.csv('./Results/Neater tenure female.csv') # the 18889 results exist for female due to error they are not there
int.tab(mod = mod3bf, x = '', template.tab)
summary(mod1bm)

mod3bm %>% summary
##  they are a replication of the 1919-1930 results
int.tab(mod = mod3bm, x = '', template = template.tab)


## robustness check 
int.tab(mod = mod4bm, x = 'tenure2')
int.tab(mod = mod4bf, x = 'tenure2')

##  Step five: Random effects for tenure
mod1bm@beta
n.b1 <- mod1bm@beta %>% length
mod1.vars <- c(vcov(mod1bm)[1, 1], rep(NA, n.b1 - 1))
for (i in 2:n.b1){
  mod1.vars[i] <- vcov(mod1bm)[c(1, i), c(1, i)] %>% sum
}
mod1.vars
male.years <- data.frame(name = vcov(mod1bm) %>% row.names,
                         beta = c(0, mod1bm@beta[-1]),
                         se = mod1.vars %>% sqrt)
male.years

### Something is happening with the mean of the random effects


ranef(mod3bm)$year
t <- lmer(f.salary ~ 1 + (1|ID) + as.factor(year), df.m)
t
lm(as.factor(year), df.m)

aggregate(f.salary ~ year, df.m %>% subset(tenure1 == 0), mean)
mod3bm
