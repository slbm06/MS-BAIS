rm(list=ls())
library(rio)
library(conflicted)
library(dplyr)
library(car)
library(corrplot)
library(lubridate)
library(tidyverse)
library(lattice)
library(stargazer)
library(plm)
library(lme4)

rawproductsdf <- import("Final Exam - SnackChain.xlsx", sheet = "products")
colnames(rawproductsdf)=tolower(make.names(colnames(rawproductsdf)))
# View(rawproductsdf)
productsdf <- subset(rawproductsdf, category!="ORAL HYGIENE PRODUCTS")
productsdf <- cbind(productsdf, size_in_oz= gsub(' OZ','',productsdf$product_size))
productsdf$size_in_oz <- as.numeric(productsdf$size_in_oz)
productsdf = subset(productsdf, select = -c(product_size) )
rm(rawproductsdf)
# View(productsdf)
str(productsdf)

storesdf <- import("Final Exam - SnackChain.xlsx", sheet = "stores")
colnames(storesdf)=tolower(make.names(colnames(storesdf)))
storesdf = subset(storesdf, select = -c(parking) )
names(storesdf)[names(storesdf) == 'store_id'] <- 'store_num'
storesdf<-storesdf[!(storesdf$store_num=="4503" & storesdf$segment=="MAINSTREAM"),]
storesdf<-storesdf[!(storesdf$store_num=="17627" & storesdf$segment=="UPSCALE"),]
storesdf
unique(storesdf$store_num)

transactionssdf <- import("Final Exam - SnackChain.xlsx", sheet = "transactions")
colnames(transactionssdf)=tolower(make.names(colnames(transactionssdf)))
transactionssdf$week_end_date <- as.Date(transactionssdf$week_end_date)
str(transactionssdf)

# Remove the few rows with missing price data
which(colSums(is.na(transactionssdf))>0)
names(which(colSums(is.na(transactionssdf))>0))
transactionssdf <- transactionssdf[!(is.na(transactionssdf[["price"]])),]
transactionssdf <- transactionssdf[!(is.na(transactionssdf[["base_price"]])),]
transactionssdf <- transactionssdf[!(transactionssdf$price=="0"),]
which(colSums(is.na(transactionssdf))>0)
names(which(colSums(is.na(transactionssdf))>0))

# Calculate the year quarter 
#transactionssdf$year_quarter=year(transactionssdf$week_end_date), "/0",quarter(transactionssdf$week_end_date));
transactionssdf <- cbind(transactionssdf, year=0)
transactionssdf$year=year(transactionssdf$week_end_date)
transactionssdf <- cbind(transactionssdf, quarter=0)
transactionssdf$quarter=quarter(transactionssdf$week_end_date)
transactionssdf <- cbind(transactionssdf, month=0)
transactionssdf$month=month(transactionssdf$week_end_date)

# Create a time index based on the ordered week
weekindexdf <- unique(transactionssdf[,'week_end_date',drop = FALSE])
weekindexdf[order(as.Date(weekindexdf$week_end_date, format="%Y-%m-%d")),]
weekindexdf$weekid <- seq.int(nrow(weekindexdf))
indexedtrxdf <- merge(transactionssdf, weekindexdf, by = "week_end_date")

#write.csv(df,"df.csv", row.names = FALSE)

# Build the combined data set
df <- merge(productsdf, indexedtrxdf, by = "upc")
df <- cbind(df, price_per_oz = round(df$price / df$size_in_oz,2))
df <- merge(df, storesdf, by = "store_num")
rm(indexedtrxdf)
# rm(productsdf)  # needed at the end to get product details on elasticity
rm(transactionssdf)
rm(storesdf)
rm(weekindexdf)


which(colSums(is.na(df))>0)
names(which(colSums(is.na(df))>0))

df$store_num <- as.factor(df$store_num)
df$upc       <- as.factor(df$upc)
df$category  <- as.factor(df$category)
df$city     <- as.factor(df$city)
df$segment   <- as.factor(df$segment)
df$category  <- relevel(df$category, "BAG SNACKS")
df$segment   <- relevel(df$segment, "VALUE")

str(df)

# Looking for duplicate keys
is.pbalanced(df)
df$unique_id <- paste(df$store_num,df$upc,df$week_end_date) # concatenate to make unique ID
df$duplicate = duplicated(df$unique_id) # generate the duplicate variable
subset(df, duplicate=="TRUE") # find the duplicate

attach(df)

head(df[,c(8,18:20)])
tail(df[,c(8,18:20)])

# Test for relationship between time and spend/units/visits
spend_lm <- lm(spend ~ weekid, data=df)
summary(spend_lm)
units_lm <- lm(units ~ weekid, data=df)
summary(units_lm)
hhs_lm <- lm(hhs ~ weekid, data=df)
summary(hhs_lm)
# No autocorrelation
durbinWatsonTest(spend_lm)  # 1.789 -> no autocorrelation
pacf(df$spend)              # graph confirms
durbinWatsonTest(units_lm)  # 1.832 -> no autocorrelation
pacf(df$units)              # graph confirms
durbinWatsonTest(hhs_lm)    # 1.812 -> no autocorrelation
pacf(df$hhs)                # graph confirms


plot(spend ~ weekid, data=df,type="o")
abline(spend_lm, col="magenta",lwd=2)
par(mfrow=c(2,2))
plot(spend_lm)
plot(units_lm)
plot(hhs_lm)
par(mfrow=c(1,1))

# No seasonal trend detected
par(mfrow=c(2,2))
plot(weekid,spend,type="o")
plot(weekid,units,type="o")
plot(weekid,visits,type="o")
par(mfrow=c(1,1))

# Data is showing exponential/highly rightly skewed
hist(spend,breaks=50,xlim=c(0,500),ylim=c(0,500000))
mean(spend) #64
median(spend)  # 43
hist(units,breaks=75,xlim=c(0,200),ylim=c(0,420000))
mean(units) # 23
median(units) # 15
hist(hhs,breaks=50,xlim=c(0,500),ylim=c(0,420000))
mean(hhs) #20
median(hhs) # 13

# Correlation check
round(cor(cbind(df[, 9:22],df[,"msa"],df[, 28:29])), 3)
corcols <- cor(cbind(df[, 9:22],df[,"msa"],df[, 28:29]))
corrplot(corcols, method="circle",type="lower")

# Research for heterogeneity based on segment and category
densityplot(~spend | segment*category, data=df, col="steelblue")
densityplot(~units | segment*category, data=df, col="steelblue")
densityplot(~hhs | segment*category, data=df, col="steelblue")

bwplot(spend ~ segment | category, data=df)
bwplot(units ~ segment | category, data=df)
bwplot(hhs ~ segment | category, data=df)

xyplot(spend ~ segment | category, data=df)
xyplot(units ~ segment | category, data=df)
xyplot(hhs ~ segment | category, data=df)


##################################################################

#  1.	What is the effect of promotions, displays, or being featured in the circular on product sales (spend), unit sales, and the number of household purchasers? 
pooled_spend1 <- plm(spend ~ tpr_only + display + feature, data=df, model="pooling")
summary(pooled_spend1)

fixed_spend1 <-  plm(spend ~ tpr_only + display + feature, data=df, model="within")
summary(fixed_spend1)                                      
fixef(fixed_spend1)                         
summary(fixef(fixed_spend1))

random_spend1 <- plm(spend ~ tpr_only + display + feature, data=df, model="random")
summary(random_spend1)                                     
ranef(random_spend1)
summary(ranef(random_spend1))

plmtest(pooled_spend1)
plmtest(pooled_spend1, effect="twoways", type="bp")
pFtest(fixed_spend1, pooled_spend1)                               
phtest(fixed_spend1, random_spend1)       

stargazer(pooled_spend1, fixed_spend1, random_spend1, type="text", single.row=TRUE)

pooled_units1 <- plm(units ~ tpr_only + display + feature, data=df, model="pooling")
summary(pooled_units1)

fixed_units1 <-  plm(units ~ tpr_only + display + feature, data=df, model="within")
summary(fixed_units1)                                      
fixef(fixed_units1)                         
summary(fixef(fixed_units1))

random_units1 <- plm(units ~ tpr_only + display + feature, data=df, model="random")
summary(random_units1)                                     
ranef(random_units1)
summary(ranef(random_units1))

plmtest(pooled_units1)
plmtest(pooled_units1, effect="twoways", type="bp")
pFtest(fixed_units1, pooled_units1)                               
phtest(fixed_units1, random_units1)       

stargazer(pooled_units1, fixed_units1, random_units1, type="text", single.row=TRUE)

pooled_hhs1 <- plm(hhs ~ tpr_only + display + feature, data=df, model="pooling")
summary(pooled_hhs1)

fixed_hhs1 <-  plm(hhs ~ tpr_only + display + feature, data=df, model="within")
summary(fixed_hhs1)                                      
fixef(fixed_hhs1)                         
summary(fixef(fixed_hhs1))

random_hhs1 <- plm(hhs ~ tpr_only + display + feature, data=df, model="random")
summary(random_hhs1)                                     
ranef(random_hhs1)
summary(ranef(random_hhs1))

plmtest(pooled_hhs1)
plmtest(pooled_hhs1, effect="twoways", type="bp")
pFtest(fixed_hhs1, pooled_hhs1)                               
phtest(fixed_hhs1, random_hhs1)       

stargazer(pooled_hhs1, fixed_hhs1, random_hhs1, type="text", single.row=TRUE)

#  2.	How do the above effects vary by product categories (cold cereals, frozen pizza, bag snacks) and store segments (mainstream, upscale, value)? (

idxdf <- pdata.frame(df, index=c("weekid","store_num","upc"))

pooled_spend2 <- plm(spend ~ tpr_only + display + feature + size + price + category + segment, data=idxdf, model="pooling")
summary(pooled_spend2)

fixed_spend2 <-  plm(spend ~ tpr_only + display + feature + size + price + category + segment, data=idxdf, model="within")
summary(fixed_spend2)                                      
fixef(fixed_spend2)                         
summary(fixef(fixed_spend2))

random_spend2 <- plm(spend ~ tpr_only + display + feature + size + price + category + segment, data=idxdf, model="random")
# random_spend2  <- lmer(spend ~ tpr_only + display + feature + size + price + (1|category) + (1|segment), data=idxdf, REML=FALSE)
summary(random_spend2)                                     
ranef(random_spend2)
summary(ranef(random_spend2))

plmtest(pooled_spend2)
pFtest(fixed_spend2, pooled_spend2)                                 
phtest(fixed_spend2, random_spend2)       

stargazer(pooled_spend2, fixed_spend2, random_spend2, type="text", single.row=TRUE)
summary(fixef(fixed_spend2))

pooled_units2 <- plm(units ~ tpr_only + display + feature + size + price + category + segment, data=idxdf, model="pooling")
summary(pooled_units2)

fixed_units2 <-  plm(units ~ tpr_only + display + feature + size + price + category + segment, data=idxdf, model="within")
summary(fixed_units2)                                      
fixef(fixed_units2)                         
summary(fixef(fixed_units2))

random_units2 <- plm(units ~ tpr_only + display + feature + size + price + category + segment, data=idxdf, model="random")
summary(random_units2)                                     
ranef(random_units2)
summary(ranef(random_units2))

plmtest(pooled_units2)
pFtest(fixed_units2, pooled_units2)                                 
phtest(fixed_units2, random_units2)       

stargazer(pooled_units2, fixed_units2, random_units2, type="text", single.row=TRUE)
summary(fixef(fixed_units2))

pooled_hhs2 <- plm(hhs ~ tpr_only + display + feature + size + price + category + segment, data=idxdf, model="pooling")
summary(pooled_hhs2)

fixed_hhs2 <-  plm(hhs ~ tpr_only + display + feature + size + price + category + segment, data=idxdf, model="within")
summary(fixed_hhs2)                                      
fixef(fixed_hhs2)                         
summary(fixef(fixed_hhs2))

random_hhs2 <- plm(hhs ~ tpr_only + display + feature + size + price + category + segment, data=idxdf, model="random")
summary(random_hhs2)                                     
ranef(random_hhs2)
summary(ranef(random_hhs2))

plmtest(pooled_hhs2)
pFtest(fixed_hhs2, pooled_hhs2)                                 
phtest(fixed_hhs2, random_hhs2)       

stargazer(pooled_hhs2, fixed_hhs2, random_hhs2, type="text", single.row=TRUE)
summary(fixef(fixed_hhs2))

#  3.	What are the five most prices elastic and five least price-elastic products? 
#  Price elasticity is the change in sales for a unit change in product price. 
#  4.	As the retailer, which products would you lower the price to maximize (a) product sales and (b) unit sales, and why? 
rm(upcdf)
upcdf <- unique(df[,'upc',drop = FALSE])

upcdf$upc <- as.character(upcdf$upc)
upcdf <- cbind(upcdf, log_price_coef=0, log_units_coef=0)

View(upcdf)
head(upcdf)
str(upcdf)

for (i in 1:nrow(upcdf)) {
  upcnumber <- upcdf[i ,1]
  spenddf <- lm(log(spend) ~ log(price) + store_num,data = df, subset=(upc==upcnumber))
  spendcoeff <- spenddf$coefficients["log(price)"]
  upcdf$log_price_coef[upcdf$upc == upcnumber] <- round(spendcoeff,3)
  unitdf <- lm(log(units) ~ log(price) + store_num,data = df, subset=(upc==upcnumber))
  unitcoeff <- unitdf$coefficients["log(price)"]
  upcdf$log_units_coef[upcdf$upc == upcnumber] <- round(unitcoeff,3)
}

price_elasticity <- merge(productsdf, upcdf, by = "upc")
pricesort <- price_elasticity[order(price_elasticity$log_price_coef),]
upc_price_elasticity = subset(pricesort, select = -c(log_units_coef,category,sub_category) )
head(upc_price_elasticity)
tail(upc_price_elasticity,n=10)

unit_elasticity <- merge(productsdf, upcdf, by = "upc")
unitsort <- unit_elasticity[order(unit_elasticity$log_units_coef),] 
upc_units_elasticity = subset(unitsort, select = -c(log_price_coef,category,sub_category) )
head(upc_units_elasticity)
tail(upc_units_elasticity,n=10)

#Export to present in report nicely (with grids)
write.csv(upc_price_elasticity,file="price_elasticity.csv")
write.csv(upc_units_elasticity,file="unit_elasticity.csv")

