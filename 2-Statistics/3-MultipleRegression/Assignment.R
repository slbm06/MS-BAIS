rm(list=ls())
library(rio)
allflightdata=import("Assignment Data.xlsx")
colnames(allflightdata)=tolower(make.names(colnames(allflightdata)))

allflightdata
str(allflightdata)
head(allflightdata)
tail(allflightdata)

flightdata=subset(allflightdata,origin=="LAS")
flightdata=rbind(flightdata,subset(allflightdata,origin=="LAX"))
flightdata=rbind(flightdata,subset(allflightdata,origin=="BWI"))
flightdata=rbind(flightdata,subset(allflightdata,origin=="LGA"))
flightdata=rbind(flightdata,subset(allflightdata,origin=="MCI"))
flightdata=rbind(flightdata,subset(allflightdata,origin=="MCO"))
flightdata=rbind(flightdata,subset(allflightdata,origin=="ATL"))
flightdata=rbind(flightdata,subset(allflightdata,origin=="BNA"))

set.seed(6933)
flightdatasample=flightdata[sample(1:nrow(flightdata),50),]

flightdatasample$origin=as.factor(flightdatasample$origin)
flightdatasample$destination=as.factor(flightdatasample$destination)
flightdatasample$marketleadingairline=as.factor(flightdatasample$marketleadingairline)
flightdatasample$lowpriceairline=as.factor(flightdatasample$lowpriceairline)

#Analysis
str(flightdatasample)
head(flightdatasample)
tail(flightdatasample)

attach(flightdatasample)
table(origin)

continuousflightdata=flightdatasample[,c(3,4,5,7,9)]
plot(continuousflightdata,pch=19,upper.panel=NULL)

reg.flight=lm(price~averagefare+distance+avgweeklypassengers+routemarketshare+origin,data=flightdatasample)
summary(reg.flight)

par(mfrow=c(2,2))
#Linearity
plot(price,reg.flight$fitted.values, pch=19,main="Linearity Plot with Reg Line")
abline(reg.flight,lwd=3,col="magenta")
#Normality
qqnorm(rstandard(reg.flight),pch=19, main="Normality Residual Plot")
qqline(rstandard(reg.flight),lwd=3,col="magenta")
hist(reg.flight$residuals,col="steelblue", main="Normality Residual Histogram", xlim=c(-70,60),probability=TRUE)
curve(dnorm(x,mean(reg.flight$residuals), sd(reg.flight$residuals)), from=-70, to=60, lwd=3,col="magenta",add=TRUE) 
#Equality of Variance
plot(rstandard(reg.flight), ylim=c(-3.5,3.5),pch=19,main="Equality of Variances Plot")
abline(0,0,lwd=3,col="magenta")
abline(-3,0,lwd=3,col="coral4")
abline(3,0,lwd=3,col="coral4")
par(mfrow=c(1,1))

#Independence
car::durbinWatsonTest(reg.flight)

flightdatasample[which.max(abs(reg.flight$residuals)),c(1,2,9)]
flightdatasample[which.min(abs(reg.flight$residuals)),c(1,2,9)]

