##  Need to do tables of in and outflows

library(data.table)
library(tidyverse)
library(ggplot2)

##  Step 1) Load in the cleaned data ----
##  The object is called wages.long
load(file='Data/Coats May 2018/cleaned long form coats.Rdata')

##  Step 2)
glas.df <- wages.long %>% subset(Location=='Glasgow')
summary(glas.df)

##  Step 3)
staff.tab <- data.frame(year = 1890:1930, into = NA, out = NA, total.now = NA,
                        total.last = NA,
                        ten.keep = NA, ten.out = NA)

for (i in 1:nrow(staff.tab)){
  temp1 <- glas.df %>% subset(year == (1889:1929)[i])
  temp2 <- glas.df %>% subset(year == (1890:1930)[i])
  
  staff.tab$total.last[i] <- nrow(temp1)
  staff.tab$total.now[i] <- nrow(temp2)
  staff.tab$out[i] <- nrow(temp1) - sum(temp1$ID %in% temp2$ID) #missing staff
  staff.tab$into[i] <- nrow(temp2) - sum(temp2$ID %in% temp1$ID)
  staff.tab$ten.keep[i] <- temp1$tenure1[(temp1$ID %in% temp2$ID)] %>% mean
  staff.tab$ten.out[i] <- temp1$tenure1[!(temp1$ID %in% temp2$ID)] %>% mean
}


staff.tab$per.in <- staff.tab$into / staff.tab$total.last
staff.tab$per.out <- staff.tab$out / staff.tab$total.last

staff.tab %>% write.csv('./Results/Turnover Glasgow.csv')


##  Convert to long or at least the wierd ggformat
temp.list <- list(NA)
for (i in 2:ncol(staff.tab)){
  temp <- data.frame(year = staff.tab$year, stat = staff.tab[, i], 
                    type = names(staff.tab)[i])
  temp.list[[i - 1]] <- temp
}

staff.gg <- do.call(rbind, temp.list)
staff.gg
##  Step 4) Plotting the turn over and staff intake

ggplot(data = staff.gg %>% subset(type %in% c('into', 'out', 'total.now')), 
       aes(x = year, y = stat, group = type)) + 
  geom_line(size = 1, aes(linetype = type)) +
  geom_point(aes(shape = type), size = 3) +
  scale_shape_discrete(name="",
                        breaks=c("into", "out", "total.now"),
                        labels=c("New clerks", "Clerks leaving", "Total numer of clerks")) +
  scale_linetype_discrete(name="",
                       breaks=c("into", "out", "total.now"),
                       labels=c("New clerks", "Clerks leaving", "Total numer of clerks")) +
  xlab('Year') + ylab('Count') + ggtitle('Staff turnover of Coats Glasgow office')

ggplot(data = staff.gg %>% subset(type %in% c('per.in', 'per.out')), 
       aes(x = year, y = stat, group = type)) + 
  geom_line(size = 1, aes(linetype = type)) +
  scale_linetype_discrete(name="",
                       breaks=c("per.in", "per.out"),
                       labels=c("Proportion new entrants", "% Clerks leaving last year")) +
  scale_y_continuous(labels= scales::percent) +
  xlab('Year') + ylab('') + ggtitle('Staff turnover of Coats Glasgow office')
?scale_y_continuous
?geom_line
()
##