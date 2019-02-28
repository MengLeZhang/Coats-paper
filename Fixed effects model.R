#######################################################
##  Coats analysis: This is basically a simple model ##
##  that selzter used                               ###
##  Start: 27/1/2017                                ###
##  Update: 5/4/2017 the fixed effect model may be  ###
##  unsuitable so let's go with the random model    ###
##  We can greatly reduce the issues with the model ###
##  if we were willing to group periods; so I propose##
##  the period prior to 1914 (i.e. pre war), war time #
##  and after 1918                                  ###
##  update: 1/6/2017 we have an additional update to create
##  a table of tenure for men in the glasgow office

##  Load in the data
coats.path<-'Data/Coats Mar 2017/Glasgow excl senior.csv'
coats.raw<-read.csv(coats.path,na.strings =c('','NA'), stringsAsFactors = F  )
load(file='Data/Coats Mar 2017/cleaned long form coats.Rdata')
mean(coats.long$salary,na.rm=T)
library(ggplot2)
#
library(plm)
names(coats.long); head(coats.long)
coats.long$period<-'1889-1900'
coats.long$period[coats.long$year%in%1901:1913]<-'1901-1913'
coats.long$period[coats.long$year%in%1914:1918]<-'1914-1918'
coats.long$period[coats.long$year>1918]<-'zPost-war' #z to make sure it is last factor
coats.long$count <- 1

table(coats.long$period)
hist(coats.long$salary)
hist(coats.long$f.salary)

##  step one: make a table with tenure being previous years service at coate----
subset.vars <- c('tenure',  'f.salary')
male.coats <- subset(coats.long, 
                     subset=(Location=='Glasgow' & Gender=='M' & !is.na(salary)))

male.tab1 <- aggregate(male.coats[,subset.vars], by = list(male.coats$year), FUN = mean)
male.tab2 <- aggregate(male.coats$count, by = list(male.coats$year), FUN = sum)

write.csv(cbind(male.tab1, male.tab2), 'male descriptives.csv')

##  let's check who the big tenure guys are in 1918
check.1918 <- subset(male.coats, year == '1918' & tenure > 13)
check.1918 #okay so we have a number of people that commenced in cooats quite 
##  early; this was taken into account by us using commenced year
# take the names from the wide form
df.check <- subset(coats.raw, Name %in% check.1918$Name)


#FE.mod<-plm(log(salary)~as.factor(year)*(tenure),data=coats.long,index=c('Name'),model='within')
#random.mod<-plm(log(salary)~as.factor(year)*(tenure),data=coats.long,index=c('Name'),model='random')

##  Second model to see interaction during war time
random.mod<-plm(log(f.salary)~as.factor(year)+period*(tenure+I(tenure^2)),data=coats.long,index=c('Name'),model='random',subset=(Location=='Glasgow'&Gender=='M'))

#phtest(FE.mod,random.mod) #there is a diff here

##  Let's write out the models here
write.csv(round(summary(random.mod)$coef,5),'Results/random male.csv')

##  Step two: There things are more interpretable if we did predictions holding tenure constant at 1,5,10
names(random.mod$coef)
names(random.mod$coef)[1:42]
names(random.mod$coef)[c(43,45:47)]
names(random.mod$coef)[c(44,48:50)]

years.coef<-random.mod$coef[1]+c(0,random.mod$coef[2:42])
gg.df <- data.frame(Year = 1889:1930, Sal = exp(years.coef))
ggplot(data=gg.df, aes(x = Year, y = Sal)) + geom_line() +
  ylab('Adjusted real salary (£)') + xlab('Year') + 
  ggtitle('Predicted salary for new Coats employee (Men)') +
  theme(plot.title = element_text(hjust = 0.5))
rm(gg.df)

tenure.temp<-random.mod$coef[43]+c(0,random.mod$coef[45:47])
tenure.temp2<-1889:1930
tenure.coef<-1889:1930
tenure.coef[tenure.temp2%in%1889:1900]<-tenure.temp[1]
tenure.coef[tenure.temp2%in%1901:1913]<-tenure.temp[2]
tenure.coef[tenure.temp2%in%1914:1918]<-tenure.temp[3]
tenure.coef[tenure.temp2%in%1919:1930]<-tenure.temp[4]

tenure2.temp<-random.mod$coef[44]+c(0,random.mod$coef[48:50])
tenure2.temp2<-1889:1930
tenure2.coef<-1889:1930
tenure2.coef[tenure2.temp2%in%1889:1900]<-tenure2.temp[1]
tenure2.coef[tenure2.temp2%in%1901:1913]<-tenure2.temp[2]
tenure2.coef[tenure2.temp2%in%1914:1918]<-tenure2.temp[3]
tenure2.coef[tenure2.temp2%in%1919:1930]<-tenure2.temp[4]

##  Now predictions
pred.1<-log(100)+tenure.coef+tenure2.coef
pred.5<-log(100)+5*tenure.coef+25*tenure2.coef
pred.10<-log(100)+10*tenure.coef+100*tenure2.coef

plot(pred.1,x=1889:1930,type='l',col='red',ylim=c(4.5,6),
     ylab='Log real salary',
     xlab='Year')
lines(pred.5,x=1889:1930,type='l',col='blue')
lines(pred.10,x=1889:1930,type='l',col='green')

##  Plot of marginal increase in salary for up to 8 years
names(random.mod$coef)
tenure.vector <- random.mod$coef[-(1:42)]
main.terms <- tenure.vector[1] + c(0, tenure.vector[3:5])
squared.terms <- tenure.vector[2] + c(0, tenure.vector[6:8])

sq.pred <- squared.terms %*% t((1:7)^2)
percent.gain <- exp(c(sq.pred) + main.terms) - 1

years <- c(rep(1,4) %*% t(1:7))
gg.df <- data.frame(gain = percent.gain, years, 
                    period = c('1889-1900', '1901-1913', '1914-1918',
                               '1919 onwards'))
ggplot(data=gg.df, aes(x=years, y=gain, color=period)) + geom_line() +
  ylab('Percentage salary increase') + xlab('Years worked at Coats') + 
  ggtitle('Salary increase associated with one extra year of tenure (Men)') + 
  scale_y_continuous(labels = scales::percent) +
  theme(plot.title = element_text(hjust = 0.5))

rm(list=ls())
