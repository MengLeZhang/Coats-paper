##  Note need to amend re: enlistment which means that there is some odds stuff
##  going on
##  Need to do tables of in and outflows
##   Turnover analysis for males

library(data.table)
library(tidyverse)
library(ggplot2)

##  Step 1) Load in the cleaned data ----
##  The object is called wages.long
load(file='Generated data/cleaned long form coats.Rdata')

##  Step 2)
glas.df <- wages.long %>% subset(Location=='Glasgow')
summary(glas.df)
glas.df$Gender
##  Step 3)
male.tab <- data.frame(start.year = 1890:1930, into = NA, out = NA, total.now = NA,
                        total.last = NA,
                        ten.keep = NA, ten.out = NA)

for (i in 1:nrow(male.tab)){
  temp1 <- glas.df %>% subset(Gender == 'M' & year == (1889:1929)[i])
  temp2 <- glas.df %>% subset(Gender == 'M' & year == (1890:1930)[i])
  
  male.tab$total.last[i] <- nrow(temp1)
  male.tab$total.now[i] <- nrow(temp2)
  male.tab$out[i] <- nrow(temp1) - sum(temp1$ID %in% temp2$ID) #missing male
  male.tab$into[i] <- nrow(temp2) - sum(temp2$ID %in% temp1$ID)
  male.tab$ten.keep[i] <- temp1$tenure1[(temp1$ID %in% temp2$ID)] %>% mean
  male.tab$ten.out[i] <- temp1$tenure1[!(temp1$ID %in% temp2$ID)] %>% mean
}


male.tab$per.in <- male.tab$into / male.tab$total.last
male.tab$per.out <- male.tab$out / male.tab$total.last

male.tab %>% write.csv('./Results/Turnover male Glasgow.csv')

### Another step for female
female.tab <- data.frame(year = 1902:1930, into = NA, out = NA, total.now = NA,
                       total.last = NA,
                       ten.keep = NA, ten.out = NA)

for (i in 1:nrow(female.tab)){
  temp1 <- glas.df %>% subset(Gender == 'F' & year == (1901:1929)[i])
  temp2 <- glas.df %>% subset(Gender == 'F' & year == (1902:1930)[i])
  
  female.tab$total.last[i] <- nrow(temp1)
  female.tab$total.now[i] <- nrow(temp2)
  female.tab$out[i] <- nrow(temp1) - sum(temp1$ID %in% temp2$ID) #missing male
  female.tab$into[i] <- nrow(temp2) - sum(temp2$ID %in% temp1$ID)
  female.tab$ten.keep[i] <- temp1$tenure1[(temp1$ID %in% temp2$ID)] %>% mean
  female.tab$ten.out[i] <- temp1$tenure1[!(temp1$ID %in% temp2$ID)] %>% mean
}


female.tab$per.in <- female.tab$into / female.tab$total.last
female.tab$per.out <- female.tab$out / female.tab$total.last

female.tab %>% write.csv('./Results/Turnover female Glasgow.csv')

##  Convert to long or at least the wierd ggformat
temp.list1 <- list(NA)
for (i in 2:ncol(male.tab)){
  temp <- data.frame(year = male.tab$year, stat = male.tab[, i], 
                     type = names(male.tab)[i], Gender = 'Male')
  temp.list1[[i - 1]] <- temp
}

male.gg <- do.call(rbind, temp.list1)
##
temp.list2 <- list(NA)
for (i in 2:ncol(female.tab)){
  temp <- data.frame(year = female.tab$year, stat = female.tab[, i], 
                     type = names(female.tab)[i], Gender = 'Female')
  temp.list2[[i - 1]] <- temp
}

female.gg <- do.call(rbind, temp.list2)


all.gg <- rbind(male.gg, female.gg)
##  Step 4) Plotting the turn over and staff intake

ggplot(data = all.gg %>% subset(type %in% c('into', 'out', 'total.now')), 
       aes(x = year, y = stat, group = type)) + 
  geom_line(size = 1, aes(linetype = type)) +
  geom_point(aes(shape = type), size = 3) +
  scale_shape_discrete(name="",
                       breaks=c("into", "out", "total.now"),
                       labels=c("New clerks", "Clerks leaving", "Total numer of clerks")) +
  scale_linetype_discrete(name="",
                          breaks=c("into", "out", "total.now"),
                          labels=c("New clerks", "Clerks leaving", "Total numer of clerks")) +
  facet_grid(Gender ~ .) +
  xlab('Year') + ylab('Count') + ggtitle('Staff turnover of Coats Glasgow office')

ggplot(data = all.gg %>% subset(type %in% c('per.in', 'per.out')), 
       aes(x = year, y = stat, group = type)) + 
  geom_line(size = 1, aes(linetype = type)) +
  scale_linetype_discrete(name="",
                          breaks=c("per.in", "per.out"),
                          labels=c("Proportion new entrants", "% Clerks leaving last year")) +
  scale_y_continuous(labels= scales::percent) +
  facet_grid(Gender ~ .) +
  xlab('Year') + ylab('') + ggtitle('Staff turnover of Coats Glasgow office')
##