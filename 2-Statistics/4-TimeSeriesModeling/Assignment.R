rm(list=ls())
library(rio)
library(car)
passcount=import("Assignment Data.xlsx")
colnames(passcount)=tolower(make.names(colnames(passcount)))
passcount

str(passcount)
head(passcount)
tail(passcount)

passcount$timeindex=seq(1:nrow(passcount))
str(passcount)
head(passcount)
tail(passcount)
attach(passcount)

#Question 1
plot(timeindex,passengers,pch=19,type="o",main="Time Series of Passengers")

#Question 2
passcount.out=lm(passengers~timeindex,data=passcount)
summary(passcount.out)

#Question 3
plot(timeindex,passengers,pch=19,type="o",main="Time Series of Passengers w/ Regression Line")
abline(passcount.out,col="magenta",lwd=3)

#Question 4
durbinWatsonTest(passcount.out)

#Question 5
indices=data.frame(month=1:12,average=0,index=0)
for(i in 1:12) {
  count=0
  for(j in 1:nrow(passcount)) {
    if(i==passcount$month[j]) {
      indices$average[i]=indices$average[i]+passcount$passengers[j]
      count=count+1
    }
  }
  indices$average[i]=indices$average[i]/count
  indices$index[i]=indices$average[i]/mean(passcount$passengers)
}

for(i in 1:12){
  for(j in 1:nrow(passcount)){
    if(i==passcount$month[j]){
      passcount$deseason.passengers[j]=passcount$passengers[j]/indices$index[i]
    }
  }
}

tail(passcount)
indices
#Question 6
#Linear model with deseasonalized data
plot(timeindex,passcount$deseason.passengers,type="o",pch=19,main="Deseasonalized Time Series of Passengers w/ Regression Line")
passcount.baselm=lm(deseason.passengers~timeindex,data=passcount)
points(passcount$timeindex,passcount.baselm$fitted.values,type="o",pch=19,col="magenta")

tail(passcount)

# Question 7
# Reseasonalize the data
for(j in 1:nrow(passcount)) {
  xx=passcount$month[j]
  passcount$reseason.y.hat[j]=passcount.baselm$fitted.values[j]*indices$index[xx]
  passcount$reseason.error[j]=passcount$passengers[j]-passcount$reseason.y.hat[j]
}

plot(timeindex,passengers,type="o",lwd=2,data="passcount",main=paste("Time Series of Passenger Data vs. Reseasoned Fitted Values Regression Lines / r = ",round(cor(passengers,passcount$reseason.y.hat),3)))
points(timeindex,passcount$reseason.y.hat,type="l",col="red",lwd=2)

