rm(list=ls())
library(rio)
library(car)
allcars=import("Assignment Data.xlsx")

head(allcars)
str(allcars)
table(allcars$region)

carscyl=allcars[(allcars$cylinders %in% c(4,6,8)), ]
table(carscyl$cylinders)

vermontcars=subset(carscyl,region=="vermont")
appletoncars=subset(carscyl,region=="appleton")
greenbaycars=subset(carscyl,region=="green bay")
indycars=subset(carscyl,region=="indianapolis")
worcestercars=subset(carscyl,region=="worcester")

# vermontcars[vermontcars == "vermont"] <- "VT"
# appletoncars[appletoncars == "appleton"] <- "Ap-ton"
# greenbaycars[greenbaycars == "green bay"] <- "GB"
# indycars[indycars == "indianapolis"] <- "Indy"
# worcestercars[worcestercars == "worcester"] <- "Worc"

set.seed(6933)
samplecars=vermontcars[sample(1:nrow(vermontcars),50),]
samplecars=rbind(samplecars,appletoncars[sample(1:nrow(appletoncars),50),])
samplecars=rbind(samplecars,greenbaycars[sample(1:nrow(greenbaycars),50),])
samplecars=rbind(samplecars,indycars[sample(1:nrow(indycars),50),])
samplecars=rbind(samplecars,worcestercars[sample(1:nrow(worcestercars),50),])

attach(samplecars)
colnames(samplecars)=tolower(make.names(colnames(samplecars)))
samplecars$region=as.factor(samplecars$region)
str(samplecars)

table(samplecars$region)
table(samplecars$cylinders)
nrow(samplecars)

#Question 1 - Determine if asking.price has an equal variance across the 5 regions
par(mfrow=c(1,2))
boxplot(asking.price,pch=19,col="steelblue",main="Asking Price Across Our 5 Regions",data=samplecars)
boxplot(asking.price~region,pch=19,col="steelblue",main="Asking Price Split by Region",data=samplecars)
par(mfrow=c(1,1))

leveneTest(asking.price~region,data=samplecars)

#Question 2 - Plot the TukeyHSD
samplecars$region=relevel(samplecars$region,"green bay")
askprice.out=aov(asking.price~region,data=samplecars)
askprice.thsd=TukeyHSD(askprice.out) 
askprice.thsd
par(mar=c(5.1,12,4.1,2.1)) 
plot(askprice.thsd,las=2)
par(mar=c(5.1,4.1,4.1,2.1))

#Question 3 - Same, with odomater instead of region
par(mfrow=c(1,2))
boxplot(odometer,pch=19,col="steelblue",main="Odometer Readings Across Our 5 Regions",data=samplecars)
boxplot(odometer~region,pch=19,col="steelblue",main="Odometer Readings Split by Region",data=samplecars)
par(mfrow=c(1,1))

leveneTest(odometer~region,data=samplecars)

samplecars$region=relevel(samplecars$region,"indianapolis")
odo.out=aov(odometer~region,data=samplecars)
odo.thsd=TukeyHSD(odo.out) 
odo.thsd
par(mar=c(5.1,12,4.1,2.1)) 
plot(odo.thsd,las=2)
par(mar=c(5.1,4.1,4.1,2.1))

#Question 4 - Same, with asking price and cylinders
samplecars$cylinders=as.factor(samplecars$cylinders)
par(mfrow=c(1,2))
boxplot(samplecars$asking.price,pch=19,col="steelblue",main="Asking Price by Cylinders",data=samplecars)
boxplot(samplecars$asking.price~cylinders,pch=19,col="steelblue",main="Asking Price Split by Cylinders",data=samplecars)
par(mfrow=c(1,1))

leveneTest(asking.price~cylinders,data=samplecars)

samplecars$cylinders=relevel(samplecars$cylinders,"8")
pricecyl.out=aov(asking.price~cylinders,data=samplecars)
pricecyl.thsd=TukeyHSD(pricecyl.out) 
pricecyl.thsd
par(mar=c(5.1,8,4.1,2.1)) 
plot(pricecyl.thsd,las=2)
par(mar=c(5.1,4.1,4.1,2.1))

