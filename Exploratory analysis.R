####################################################
##  Step one: Data exploration                    ##
##  right this is step one and it is basically us ##
##  exploring the dataset used in the coats paper ##
##  it feels like now all the dataset are there   ##
##  though                                        ##
##  Start: 12/12/2016                             ##
####################################################

##  Step one: First read in the data
coats.test<-read.csv('Data/Coats Travellers Salaries 1889-1929testfile_Sheet1Extract_v1a.csv',na.strings =c('','NA'), stringsAsFactors = F  )
coats.test## different NA characters but I think this is all okay.
lapply(coats.test,unique) ## hmmm bloody hell this is a wierd mess## in the years i can see some movers though
coats.test$Job
##  Some dismissed and died instead of salary.. great..
##  Start dates:
start.dates
start.dates<-list(NULL)
for (i in 1:nrow(coats.test)){
  not.nas<-which(!is.na(coats.test[i,9:49]))

  min<-not.nas[1] #first will always be the earliest
  max<-not.nas[length(not.nas)]
  year=c(1889:1929)

  if(length(not.nas)>1){start.dates[[i]]<-data.frame(start=year[min],end=year[max])}else{start.dates[[i]]<-data.frame(start=NA,end=NA)} #skips ones where there is nothing reported
}
#the 59th guy just has no wages

start.dates<-do.call(rbind,start.dates)
coats.test<-cbind(coats.test,start.dates)

##  Step two: Now we can try to get some things out; first convert to long forms; jsut for some testing.
names(coats.test)
coats.temp<-list(NULL)
years<-9:49 #this is the yearly salary
for (i in 1:length(years)){
  j<-years[i]
  coats.temp[[i]]<-coats.test[,-years]
  coats.temp[[i]]<-data.frame(coats.temp[[i]],salary=coats.test[,j],year=c(1889:1929)[i])
}
head(coats.temp[[i]])
coats.long<-do.call(rbind,coats.temp)
head(coats.long)

##  Step three: We can ty to do tenure; this is basically a function of looking at their stat data minus the year and then added to prior firms (if prior firm was coats)
head(coats.long)
table(coats.long$Prior.Firm) # there is C here-- is this short for coas?? Anyway only one spelling of Coats so good
coats.long$age<-coats.long$year-coats.long$Birth.Year

##  I think commenced year is it
prev.coats<-(coats.long$Prior.Firm=='Coats') #wil ladd previos coats employment later
coats.long$tenure<-coats.long$year-coats.long$start+1 #will get negative but only for years where there is no salary anyway. Add one becuse everyone was there one year

cor.name<-list(NULL)
for (i in 1:length(unique(coats.long$Name))){
  names<-unique(coats.long$Name)
  temp<-subset(coats.long,Name==names[i])
  cor.name[[i]]<-cor(temp$tenure,temp$age,use='complete.obs') 
}
cor.name #dafaq
cor(coats.long$tenure,coats.long$age,use='complete.obs') 

