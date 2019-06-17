##  Replication of Heller and co 2014
##  They used subsets to control for tenure and ANOVA tests for year
##  Start: 18/5/2018 Exactly like Heller

library(data.table)
library(tidyverse)
library(ggplot2)
library(stargazer)

##  Step 1) Load in the cleaned data ----
##  The object is called wages.long
load(file='Generated data/cleaned long form coats.Rdata')

##  Step 1b) Coding up the periods; creating 10 year tenure bands
##  Periods
##  Putting x in front of period to make the last period the reference
##  This is for when the model drops rank deficient interactions later
names(wages.long); head(wages.long)

wages.long <-
  wages.long %>%
  mutate(period = ifelse(
    year %in% 1889:1900,
    'x1889 - 1900',
    ifelse(
      year %in% 1901:1913,
      'x1901 - 1913',
      ifelse(year %in% 1914:1918, 'x1914 - 1918', '1919 - 1930')
    )
  ))

## Make bands for subsets of tenure
wages.long <-
  wages.long %>%
  mutate(tenureband1 = ifelse(tenure1 <= 9,
                              '0 - 9',
                              ifelse(tenure1 <= 19, '10 - 19',
                                     '20 plus'))) %>%
  mutate(tenureband2 = ifelse(tenure2 <= 9,
                              '0 - 9',
                              ifelse(tenure2 <= 19, '10 - 19',
                                     '20 plus')))


## We can simply for loop our analysis for women and men

##  Step two: Calculate the F statistic for models with and without years after
##  accounting for tenure. We need to take into account the random effects
table(wages.long$tenureband1, wages.long$period)
## Female wages only for after 1901
df.m <- 
  wages.long %>% subset(Location == 'Glasgow' & Gender == 'M')

df.f <-
  wages.long %>% subset(Location == 'Glasgow' &
                          Gender == 'F' & period != 'x1889 - 1900')


##  2a) The models 



##  Male F test for year given tenure
mod1am <- 
  lm(f.salary ~ tenure1, df.m)
mod1bm <- 
  lm(f.salary ~ tenure1 + as.factor(year), df.m)
anova(mod1am, mod1bm) # F ratio test

mod1cm <- 
  lm(log(f.salary) ~ tenure1 + I(tenure1^2), df.m)
mod1dm <- 
  lm(log(f.salary) ~ tenure1 + + I(tenure1^2) + as.factor(year), df.m)
anova(mod1cm, mod1dm) # F ratio test -- still sig


##  Female F test for year given tenure
mod1af <- lm(f.salary ~ tenure1, df.f)
mod1bf <- lm(f.salary ~ tenure1 + as.factor(year), df.f)
anova(mod1af, mod1bf) # F ratio test

mod1cf <- 
  lm(log(f.salary) ~ tenure1 + I(tenure1^2), df.f)

mod1df <- 
  lm(log(f.salary) ~ tenure1 + + I(tenure1^2) + as.factor(year), df.f)
anova(mod1cf, mod1df) # F ratio test

##  Robustness F tests -- no idea
# mod2am <- lmer(f.salary ~ (1|ID) + tenure2, df)
# mod2bm <- lmer(f.salary ~ (1|ID) + tenure2 + as.factor(year), df)
# anova(mod2am, mod2bm) # F ratio test


##

regs.tab <- data.frame(year = 1889:1930, 
                       int = NA, int.se = NA,
                       slope = NA, slope.se = NA,
                       rsq = NA, n = NA)


heller.regs <- 
  ##  Function settings
  function(df,
           years = 1889:1930,
           template.tab,
           form = f.salary ~ tenure1) {
  ##  Actual block of what function does  
    for (i in 1:length(years)) {
      
      mod <-
        try(
        lm(form, df %>% subset(year == (years)[i])), silent = T
      )
      if(class(mod) == 'try-error')next
      
      mod.coef <- mod$coefficients
      mod.se <- mod %>% vcov %>% diag
      
      template.tab$int[i] = mod.coef[1]
      template.tab$slope[i] = mod.coef[2]
      
      template.tab$int.se[i] = mod.se[1]
      template.tab$slope.se[i] <- mod.se[2]
      
      template.tab$n[i] <- mod$fitted.values %>% length
      template.tab$rsq[i] <- summary(mod)$r.squared
    }
    return(template.tab)
  }


df.m$tenureband1 %>% table

heller.regs(df.m , template.tab = regs.tab) %>% 
  write.csv('Results/Whole group regression male.csv')

heller.regs(df.m %>% subset(tenureband1 == '0 - 9') , template.tab = regs.tab) %>% 
  write.csv('Results/0 - 9 regression male.csv')

heller.regs(df.m %>% subset(tenureband1 == '10 - 19') , template.tab = regs.tab) %>% 
  write.csv('Results/10 - 19 regression male.csv')

heller.regs(df.m %>% subset(tenureband1 == '20 plus') , template.tab = regs.tab) %>% 
  write.csv('Results/20 plus regression male.csv') 


##  Do for feamles post 1900 as well
female.tab <- 
  data.frame(
    year = 1901:1930,
    int = NA,
    int.se = NA,
    slope = NA,
    slope.se = NA,
    rsq = NA,
    n = NA
  )


heller.regs(df.f , template.tab = female.tab, years = 1901:1930) %>% write.csv('./Results/Whole group regression female.csv')
heller.regs(df.f %>% subset(tenureband1 == '0 - 9'), template.tab = female.tab, years = 1901:1930) %>% write.csv('./Results/0 - 9 regression female.csv')
heller.regs(df.f %>% subset(tenureband1 == '10 - 19'), template.tab = female.tab, years = 1901:1930) %>% write.csv('./Results/10 - 19 regression female.csv')
heller.regs(df.f %>% subset(tenureband1 == '20 plus'), template.tab = female.tab, years = 1901:1930)  # so few Ns



##  Female F test for year given tenure
mod1af <- lmer(f.salary ~ (1|ID) + tenure1, df.f)
mod1bf <- lmer(f.salary ~ (1|ID) + tenure1 + as.factor(year), df.f)
anova(mod1af, mod1bf) # F ratio test

##  Robustness F tests
mod2am <- lmer(f.salary ~ (1|ID) + tenure2, df)
mod2bm <- lmer(f.salary ~ (1|ID) + tenure2 + as.factor(year), df)
anova(mod2am, mod2bm) # F ratio test

##  Step three: The effects of tenure over time ----
library(lme4)

mod3am <- lmer(f.salary ~ (1|ID) + (1|year) + period + tenureband1 * tenure1, df.m)

mod3bm <- lmer(f.salary %>% log ~ (1|ID) + (1|year) + tenureband1 * period * tenure1, df.m)
anova(mod3am, mod3bm) #yes for tenure period effect


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


###
heller.regs(df.m , template.tab = regs.tab, form = log(f.salary) ~ tenure1) 
