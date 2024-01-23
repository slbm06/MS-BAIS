rm(list=ls())
library(rio)
library(car)
library(moments)
library(dplyr)
library("Hmisc")
library(corrplot)
library(lmtest)
library(stargazer)
orig_sales <- import("Assignment 1 - Hunters Green.xlsx")

names(orig_sales)[names(orig_sales) == "adom_agentdaysonmarket"] <- "adom"
colnames(orig_sales)=tolower(make.names(colnames(orig_sales)))
# Removed columns not considered and spa (too many missing values)
orig_sales = subset(orig_sales, select = -c(slnoskm,status,address,bathstotal,spa,subdivn,cdom_cumuldaysmls,pendingdate,datesold) )
head(orig_sales)
str(orig_sales)
# remove NA - Replace missing garage info with 0 garages 
which(colSums(is.na(orig_sales))>0)
names(which(colSums(is.na(orig_sales))>0))

# Replace NA garage (4 records) with -1 to keep it numerical
#sales[["garages"]][is.na(sales[["garages"]])] <- -1
sales<-orig_sales[!(is.na(orig_sales[["garages"]])),]
attach(sales)

## Replace pool by numeric values: No Pool = 0, Any type of Pool = 1
sales$pool <- ifelse(sales$pool=='None', 0, 1)
head(sales)
# Replace roof by numeric values: Shingles = 0, NonStandard = 1
sales$roof <- ifelse(sales$roof=='Other', 0, 1)
head(sales)
# Replace splsale by numeric values: Regular Sales = 0, ShortSales/Auction = 1
sales$splsale <- ifelse(sales$splsale=='None', 0, 1)
head(sales)

sales[c(83),]
which(colSums(is.na(sales))>0)
names(which(colSums(is.na(sales))>0))
sum(is.nan(as.matrix(sales)))
sum(is.infinite(as.matrix(sales)))

head(sales)
View(sales)
str(sales)

# Overview of the listprice data
den_pricesold <- density(pricesold)
hist(pricesold, breaks=15, prob=T, main="Histogram of Sale Prices w/ Density Line")
lines(den_pricesold, col="magenta", lwd=2)

# Overview of the listprice data
min(adom)
max(adom)
den_adom <- density(sales$adom)
hist(adom, breaks=25, prob=T, xlim=c(0,700),main="Histogram of Avg Days on Mkt w/ Density Line")
lines(den_adom, col="steelblue", lwd=2)
quantile(adom)
#boxplot(adom,col="steelblue",main="Total adom Boxplot",pch=19)


plot(sales,main="All Sales Relationships")
salescorr <- rcorr(as.matrix(sales))  
round(salescorr$r,3)
round(salescorr$P,3)
salescorrmatrix <- round(cor(sales),3)
corrplot(salescorrmatrix, type="upper", tl.col = "black", tl.srt = 45)

# Remove "pool" for hclust 
plot(sales,main="All Sales Relationships")
salesnopool <- subset( sales, select = -pool )
salesnopool <- subset( salesnopool, select = -roof )
salesnopoolcorrmatrix <- round(cor(salesnopool),3)
corrplot(salesnopoolcorrmatrix, type="upper", order = "hclust", tl.col = "black", tl.srt = 45)

#sales$splsale=as.factor(sales$splsale)
#splsale=relevel(splsale,"0")
str(sales)
saleprice1 <- lm(pricesold ~ sqft + garages)
summary(saleprice1)
saleprice2 <- lm(pricesold ~ sqft + garages + yrblt)
summary(saleprice2)
saleprice3 <- lm(pricesold ~ sqft + garages + yrblt + roof + splsale)
summary(saleprice3)
vif(saleprice3)
stargazer(saleprice1, saleprice2, saleprice3, title="Summary of Sales Price Linear Regression Model", type="text")
par(mfrow=c(2,2))
plot(saleprice3, lwd=2, col="steelblue")
#Adjustments to improve the linearity assumption -- Worked fairly well -- The line is straighter even though some curvature remains
saleprice3b <- lm(pricesold ~ sqft + garages + yrblt + roof + splsale  + sqft*garages*roof*splsale)
summary(saleprice3b)
plot(saleprice3b, lwd=2, col="steelblue")
par(mfrow=c(1,1))


adom1 <- lm(adom ~ listprice)
summary(adom1)
adom2 <- lm(adom ~ listprice + sqft + yrblt)
summary(adom2)
adom3 <- lm(adom ~ listprice + sqft + yrblt + pool)
summary(adom3)
vif(adom3)
par(mfrow=c(2,2))
plot(adom3, lwd=2, col="darkblue")
#Remove highly correlated variables
adom4 <- lm(adom ~ yrblt + pool)
summary(adom4)
plot(adom4, lwd=2, col="darkblue")
par(mfrow=c(1,1))
# The R2 improves, but normality is much worse
# Fix for linearity
adom3b <- lm(adom ~ log(listprice) + log(sqft) + log(yrblt) + pool)
bptest(adom3b)                                     
summary(adom3b,cot=T)
vcov(adom3b) 
adom3b %>% vcov %>% diag() %>% sqrt()              
plot(adom3b, lwd=2, col="darkblue", ask=FALSE)
stargazer(adom1, adom2, adom3, title="Summary of  Agt Days on Mkt Linear Regression Model", type="text")

#Determine the relevance of the predictors
sp_relevance1 <- lm(pricesold ~ sqft)
sp_relevance2 <- lm(pricesold ~ garages)
sp_relevance3 <- lm(pricesold ~ yrblt)
sp_relevance4 <- lm(pricesold ~ roof)
sp_relevance5 <- lm(pricesold ~ splsale)
summary(sp_relevance1)$adj.r.squared
summary(sp_relevance2)$adj.r.squared
summary(sp_relevance3)$adj.r.squared
summary(sp_relevance4)$adj.r.squared
summary(sp_relevance5)$adj.r.squared

adom_relevance1 <- lm(adom ~ listprice)
adom_relevance2 <- lm(adom ~ sqft)
adom_relevance3 <- lm(adom ~ yrblt)
adom_relevance4 <- lm(adom ~ pool)
summary(adom_relevance1)$adj.r.squared
summary(adom_relevance2)$adj.r.squared
summary(adom_relevance3)$adj.r.squared
summary(adom_relevance4)$adj.r.squared

