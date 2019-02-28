## simulation demonstration
N<-10000
years<-1:30
commenced<-sample(1:10,N,replace=T) #this means years in service so far

sim.coats<-data.frame(names=1:N,year=c(sapply(years,rep,times=N)),commenced=commenced)
sim.coats$tenure<-sim.coats$year+sim.coats$commenced
sim.coats$salary<-0.4*sim.coats$tenure+0.5*sim.coats$year+0.2*(sim.coats$year*sim.coats$tenure)+rnorm(nrow(sim.coats))

##  We will have an increasing tenure to year relationship 0.1*tenure*year
lm(salary~tenure*as.factor(year),sim.coats)


lm(salary~tenure,sim.coats,subset=(year==1))
