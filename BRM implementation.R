library(brms)
brm(f.salary ~ (1|ID) + (1|year) + tenureband1 * period * tenure1, sigma ~ period, data= df.m)
brm(bf(f.salary ~ (1|ID) + period, sigma ~ period), chains = 1, data= df.m)

?brm
library(nlme)
library(lme4)

tt <- lmer(f.salary ~ (1 + period|ID) + period, df.f)
tt
library(nlme)
library(tidyr)
tt <- lme(f.salary ~ tenure1*period + period, data = df.m[, c('ID', 'f.salary', 'period', 'tenure1', 'year')] %>% na.omit, random =  ~ 1|ID ,
          weights = varIdent(form = ~ 1 + period))
?lme
summary(tt)
anova(tt)
tt
?lme
df.m[, c('ID', 'f.salary', 'period')] %>% na.omit
