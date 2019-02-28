#######################################################
##  Coats analysis: This is basically a simple model ##
##  that selzter used; this time we do it for women

##  Load in the data
coats.path<-'Data/Coats Mar 2017/Glasgow excl senior.csv'
coats.raw<-read.csv(coats.path,na.strings =c('','NA'), stringsAsFactors = F  )
load(file='Data/Coats Mar 2017/cleaned long form coats.Rdata')
mean(coats.long$salary,na.rm=T)

#
library(ggplot2)
library(plm)
names(coats.long); head(coats.long)
coats.long$period<-'1889-1900'
coats.long$period[coats.long$year%in%1901:1913]<-'1901-1913'
coats.long$period[coats.long$year%in%1914:1918]<-'1914-1918'
coats.long$period[coats.long$year>1918]<-'zPost-war' #z to make sure it is last factor
coats.long$count <- 1

##  step one: make a table with tenure being previous years service at coate----
subset.vars <- c('tenure',  'f.salary')
female.coats <- subset(coats.long, 
                     subset=(Location=='Glasgow' & Gender=='F' & !is.na(salary)))

female.tab1 <- aggregate(female.coats[,subset.vars], by = list(female.coats$year), FUN = mean)
female.tab2 <- aggregate(female.coats$count, by = list(female.coats$year), FUN = sum)

write.csv(cbind(female.tab1, female.tab2), 'female descriptives.csv')

##  Number of females pre 1901 is terrible better exclude from model
#FE.mod<-plm(log(salary)~as.factor(year)*(tenure),data=coats.long,index=c('Name'),model='within')
#random.mod<-plm(log(salary)~as.factor(year)*(tenure),data=coats.long,index=c('Name'),model='random')

##  Second model to see interaction during war time
model.df <- subset(coats.long,
                   Location=='Glasgow' & Gender=='F' & period!='1889-1900')
random.mod<-plm(log(f.salary)~as.factor(year)+period*(tenure+I(tenure^2)),
                data=model.df,index=c('Name'),
                model='random')
#phtest(FE.mod,random.mod) #big differnece

#phtest(FE.mod,random.mod) #there is a diff here

##  Let's write out the models here
write.csv(round(summary(random.mod)$coef,5),'Results/random female.csv')

##  Step two: There things are more interpretable if we did predictions holding tenure constant at 1,5,10
names(random.mod$coef)
names(random.mod$coef)[1:30]
names(random.mod$coef)[c(31,33:34)]
names(random.mod$coef)[c(32,35:36)]


##  Graph for year on year wages
timeline<-1901:1930
years.coef<-random.mod$coef[1]+c(0,random.mod$coef[2:30])
plot(y=exp(years.coef),type='l',x=timeline,
     ylab='Adjusted real salary (£)',
     xlab='Year')

gg.df <- data.frame(Year = 1901:1930, Sal = exp(years.coef))
ggplot(data=gg.df, aes(x = Year, y = Sal)) + geom_line() +
  ylab('Adjusted real salary (£)') + xlab('Year') + 
  ggtitle('Predicted salary for new Coats employee (Women)') +
  theme(plot.title = element_text(hjust = 0.5))
rm(gg.df)

####

tenure.temp<-random.mod$coef[31]+c(0,random.mod$coef[33:34])
tenure.temp2<-1901:1930
tenure.coef<-1901:1930
tenure.coef[tenure.temp2%in%1901:1913]<-tenure.temp[1]
tenure.coef[tenure.temp2%in%1914:1918]<-tenure.temp[2]
tenure.coef[tenure.temp2%in%1919:1930]<-tenure.temp[3]

tenure2.temp<-random.mod$coef[32]+c(0,random.mod$coef[35:36])
tenure2.temp2<-1901:1930
tenure2.coef<-1901:1930
tenure2.coef[tenure2.temp2%in%1901:1913]<-tenure2.temp[1]
tenure2.coef[tenure2.temp2%in%1914:1918]<-tenure2.temp[2]
tenure2.coef[tenure2.temp2%in%1919:1930]<-tenure2.temp[3]


##  Now predictions
pred.1<-log(100)+tenure.coef+tenure2.coef
pred.5<-log(100)+5*tenure.coef+25*tenure2.coef
pred.10<-log(100)+10*tenure.coef+100*tenure2.coef

plot(pred.1,x=1901:1930,type='l',col='red',ylim=c(4.5,6),
     ylab='Log real salary',
     xlab='Year')
lines(pred.5,x=1901:1930,type='l',col='blue')
lines(pred.10,x=1901:1930,type='l',col='green')

##  Probably does show use that the effects of an extra year of tenure
names(random.mod$coef)
tenure.vector
tenure.vector <- random.mod$coef[-(1:30)]
main.terms <- tenure.vector[1] + c(0, tenure.vector[3:4])
squared.terms <- tenure.vector[2] + c(0, tenure.vector[5:6])

sq.pred <- squared.terms %*% t((1:7)^2)
percent.gain <- exp(c(sq.pred) + main.terms) - 1

years <- c(rep(1,3) %*% t(0:6))
gg.df <- data.frame(gain = percent.gain, years, 
                    period = c('1901-1913', '1914-1918',
                               '1919 onwards'))

### width = 700; maintain aspect ratio 
ggplot(data=gg.df, aes(x=years, y=gain, color=period)) + geom_line() +
  ylab('Percentage salary increase') + xlab('Years worked at Coats') + 
  ggtitle('Salary increase associated with one extra year of tenure (Women)') + 
  scale_y_continuous(labels = scales::percent) +
  theme(plot.title = element_text(hjust = 0.5))
rm(gg.df)

