rm(list=ls())
library(rio)
library(moments)
mydata=import("Assignment Data.xlsx")
attach(mydata)

mydata
str(mydata)
head(mydata)
tail(mydata)

#Create intermediate data frames for CA, FL, and NY or NJ
califcu=subset(mydata,state=="CA")
floridacu=subset(mydata,state=="FL")
nynjcu=subset(mydata,state=="NY"|state=="NJ")

# Create primary frame with random samples of intermediary frames
set.seed(6933)
casample=califcu[sample(1:nrow(califcu),20),]
flsample=floridacu[sample(1:nrow(floridacu),20),]
nynjsample=nynjcu[sample(1:nrow(nynjcu),20),]

#90% confidence interval for the members variable for flsample
t.test(flsample$members,conf.level =0.9)

#99% confidence interval for the members variable for flsample
t.test(flsample$members,conf.level =0.99)

#Check if the CA Credit Unions' average total assets greater than $170 million
t.test(casample$total.assets,mu=170,alternative = "greater")

#Compare the CA and NY/NJ Credit Union's mean total assets
t.test(casample$total.asset,nynjsample$total.asset,mu=0, alternative = c("two.sided"))

#Extract the name, city, members, total assets for the largest FL credit union in terms of total assets
flsample[which.max(flsample$total.assets),c(2,3,5,6)] 

