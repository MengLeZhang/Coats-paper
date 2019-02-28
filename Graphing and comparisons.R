##  Plotting results -- intercepts

heller <- './Data/Coats May 2018/Heller 2014 regression table.csv' %>% read.csv(stringsAsFactors = F)
heller.int <- heller %>% subset(Var == 'B0') 
heller.int <- data.frame(year = heller.int$year, inter = heller.int$Overall,
                         source = 'H&K 2014')

seltzer <- './Data/Coats May 2018/Seltzer 2010 tables.csv' %>% read.csv(stringsAsFactors = F)
seltzer$WDB.North <- seltzer$WDB.North %>% substr(1, 5) %>% as.numeric
seltzer$WDB.London <- seltzer$WDB.London %>% substr(1, 5) %>% as.numeric

seltzer.int_temp <- seltzer %>% subset(Var %in% c('Constant', paste('year', 1890:1935, sep ='')))
seltzer.int[47, ]
seltzer.int1 <- data.frame(year = 1890:1935, 
                          inter = (seltzer.int_temp$WDB.North[47] + c(seltzer.int_temp$WDB.North[- 47])) %>% exp,
                          source = 'Seltzer 2010 WDB North')
seltzer.int2 <- data.frame(year = 1890:1935, 
                           inter = (seltzer.int_temp$WDB.London[47] + c(seltzer.int_temp$WDB.London[- 47])) %>% exp,
                           source = 'Seltzer 2010 WDB London')


tab.male <- './Results/Whole group regression male.csv' %>% read.csv
tab.male.int <- data.frame(year = 1889:1930, inter = tab.male$int,
                           source = 'Coats male')

tab.female <- './Results/Whole group regression female.csv' %>% read.csv
tab.female <- tab.female %>% subset(n >= 40) # only takes out the first row
tab.female.int <- data.frame(year = tab.female$year, inter = tab.female$int,
                           source = 'Coats female')

tabs <- do.call(rbind, list(heller.int, seltzer.int1, seltzer.int2, 
                            tab.male.int, tab.female.int))

ggplot(data = tabs, aes(x = year, y = inter, colour = source)) + geom_line(size = 2) + 
  ylab('Annual salary £') + ggtitle('Tenure adjusted basline salary')

##   plot of tenure effects at 5, 15 and so forth

tab.male.slope5 <- './Results/0 - 9 regression male.csv' %>% read.csv
tab.male.slope5 <- tab.male.slope5[- c(1:2), ]
tab.male.slope5 <- data.frame(year = tab.male.slope5$year, 
                              inter = tab.male.slope5$slope,
                            source = 'Coats male 0 - 9 years')
tab.male.slope15 <- './Results/10 - 19 regression male.csv' %>% read.csv
tab.male.slope15 <- tab.male.slope15[-c(1:11), ]
tab.male.slope15 <- data.frame(year = tab.male.slope15$year, 
                               inter = tab.male.slope15$slope,
                              source = 'Coats male 10 - 19 years')


## female

tab.female.slope5 <- './Results/0 - 9 regression female.csv' %>% read.csv
tab.female.slope5 <- tab.female.slope5[- 1, ]
tab.female.slope5 <- data.frame(year = tab.female.slope5$year, 
                                inter = tab.female.slope5$slope,
                                source = 'Coats female 0 - 9 years')

tab.female.slope15 <- './Results/10 - 19 regression female.csv' %>% read.csv
tab.female.slope15 <- tab.female.slope15[-c(1:15), ]
tab.female.slope15 <- data.frame(year = tab.female.slope15$year, 
                                 inter = tab.female.slope15$slope,
                                 source = 'Coats female 10 - 19 years')



##  Seltzer
seltzer.slope_temp <- seltzer %>% subset(Var %in% c('tenure', paste('yearten', 1890:1935, sep ='')))
seltzer.slope5.temp1 <- seltzer.slope_temp$WDB.North[1] + seltzer.slope_temp$WDB.North[-1]

seltzer.slope5.1 <- data.frame(year = 1890:1935, 
                               inter = (seltzer.int_temp$WDB.North[47] + c(seltzer.int_temp$WDB.North[- 47])),
                               source = 'Seltzer 2010 WDB North 5 years')
seltzer.slope5.1$inter <- exp(seltzer.slope5.1$inter + seltzer.slope5.temp1 * 5) - exp(seltzer.slope5.1$inter + seltzer.slope5.temp1 * 4)

seltzer.slope15.1 <- data.frame(year = 1890:1935, 
                               inter = (seltzer.int_temp$WDB.North[47] + c(seltzer.int_temp$WDB.North[- 47])),
                               source = 'Seltzer 2010 WDB North 15 years')
seltzer.slope15.1$inter <- exp(seltzer.slope15.1$inter + seltzer.slope5.temp1 * 15) - exp(seltzer.slope15.1$inter + seltzer.slope5.temp1 * 14)

## lnd
seltzer.slope5.temp2 <- seltzer.slope_temp$WDB.London[1] + seltzer.slope_temp$WDB.London[-1]

seltzer.slope5.2 <- data.frame(year = 1890:1935, 
                               inter = (seltzer.int_temp$WDB.London[47] + c(seltzer.int_temp$WDB.London[- 47])),
                               source = 'Seltzer 2010 WDB London 5 years')
seltzer.slope5.2$inter <- exp(seltzer.slope5.2$inter + seltzer.slope5.temp2 * 5) - exp(seltzer.slope5.2$inter + seltzer.slope5.temp2 * 4)

seltzer.slope15.2 <- data.frame(year = 1890:1935, 
                                inter = (seltzer.int_temp$WDB.London[47] + c(seltzer.int_temp$WDB.London[- 47])),
                                source = 'Seltzer 2010 WDB London 15 years')
seltzer.slope15.2$inter <- exp(seltzer.slope15.2$inter + seltzer.slope5.temp2 * 15) - exp(seltzer.slope15.2$inter + seltzer.slope5.temp2 * 14)

##  Heller
heller <- './Data/Coats May 2018/Heller 2014 regression table.csv' %>% read.csv(stringsAsFactors = F)
heller.slope_temp <- heller[grep('Btenure', heller$Var), ]
heller.slope5 <- data.frame(year = heller.slope_temp$year, 
                            inter = heller.slope_temp$X0.9.years,
                         source = 'H&K 2014 5 years')
heller.slope15 <- data.frame(year = heller.slope_temp$year, 
                            inter = heller.slope_temp$X10.19.years,
                            source = 'H&K 2014 15 years')

##  All
slope.tabs <- do.call(rbind, list(heller.slope5, heller.slope15,
                                  seltzer.slope5.1, seltzer.slope5.2,
                                  seltzer.slope15.1, seltzer.slope15.2,
                                  tab.male.slope15, tab.male.slope5,
                                  tab.female.slope15, tab.female.slope5))


slope.tabs

ggplot(data = slope.tabs[c(grep(' 9 ', slope.tabs$source), grep(' 5 ', slope.tabs$source)), ], aes(x = year, y = inter, colour = source)) + geom_line(size = 2) +
  ylab('Expected annual salary increase £') + ggtitle('Return on additional year of tenure at 0 - 9 years service')
ggplot(data = slope.tabs[c(grep(' 19 ', slope.tabs$source), grep(' 15 ', slope.tabs$source)), ], aes(x = year, y = inter, colour = source)) + geom_line(size = 2) +
  ylab('Expected annual salary increase £') + ggtitle('Return on additional year of tenure at 10 - 19 years service')

ggplot(data = slope.tabs[grep('Coats', slope.tabs$source), ], aes(x = year, y = inter, colour = source)) + 
  geom_line(size = 1, alpha = 1, aes(linetype = source)) +
  geom_point(aes(shape = source), size = 3) +
  ylab('Expected annual salary increase £') + ggtitle('Return on additional year of tenure at Coats')



diff1 <- aggregate(inter ~ source + year, slope.tabs[grep(' 5 ', slope.tabs$source), ], mean)
diff2 <- aggregate(inter ~ source + year, slope.tabs[grep(' 15 ', slope.tabs$source), ], mean)

ggplot(data = diff3, aes(x = year, y = inter, colour = source)) + geom_line(size = 2) 


#tab.female.int5 <- './Results/0 - 9 regression female.csv' %>% read.csv

##  Need to sort out stuff here figure 5
coat.sub <- slope.tabs %>% subset(grepl('Coats', source))
coat.sub$f <- ifelse(grepl('female', coat.sub$source), 'Female', 'Male')
coat.sub$Tenure <- ifelse(grepl('19', coat.sub$source), '10 - 19 years', '0 - 9 years')

ggplot(data = coat.sub, aes(x = year, y = inter, colour = Tenure)) + 
  geom_line(aes(linetype = Tenure), size = 2) +
  ylab('Expected annual salary increase £') + ggtitle('Return on additional year of tenure at Coats') + xlab('Year') +
  facet_grid(f ~ .)
