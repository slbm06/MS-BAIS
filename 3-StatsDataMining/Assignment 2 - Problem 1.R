rm(list=ls())
library(rio)
library(conflicted) #prevent conflict with tidyverse
library(dplyr)
library(magrittr) # understand %<>%
library(caret)
library(InformationValue) #sensitivity and specificity 
library(tidyverse) # convert all chr to factor
library(stargazer)
library(ROCR)

orig_telco <- import("Assignment 2 - Problem 1 - TelcoChurn.xlsx")
colnames(orig_telco)=tolower(make.names(colnames(orig_telco)))
orig_telco = subset(orig_telco, select = -c(customerid) )
## Replace categorical values with 0 and 1
orig_telco$churn <- ifelse(orig_telco$churn=='No', 0, 1)
#orig_telco$churn=as.factor(orig_telco$churn)
orig_telco %<>% mutate(across(where(is.character),as_factor))
str(orig_telco)
cor(orig_telco$tenure,orig_telco$churn)

##################################################################################################################
################################################# Phone Only ##################################################
##################################################################################################################

telco_phone <- subset(orig_telco,phoneservice=="Yes" & internetservice=="No")
#Validate the split of data
unique(telco_phone$phoneservice)
unique(telco_phone$internetservice)

#Review values of monthlycharges to determine how to replace with categorical variables (range)
#min(telco_phone$monthlycharges)
#max(telco_phone$monthlycharges)
#mean(telco_phone$monthlycharges)
#median(telco_phone$monthlycharges)
#sum(telco_phone$monthlycharges < median(telco_phone$monthlycharges))
#sum(telco_phone$monthlycharges >= median(telco_phone$monthlycharges))

#Clean up the data by making monthlycharges categorical data instead of continuous data
telco_phone <- telco_phone %>% 
  mutate(mthlychrgcat = if_else(monthlycharges< median(telco_phone$monthlycharges), "Less than Median", "More than Median"))
telco_phone = subset(telco_phone, select = -c(internetservice,onlinesecurity,onlinebackup,deviceprotection,techsupport,streamingtv,streamingmovies,monthlycharges) )

#Check for NA rows and remove if small number of rows are affected
names(which(colSums(is.na(telco_phone))>0))
telco_phone<-telco_phone[!(is.na(telco_phone[["totalcharges"]])),]
names(which(colSums(is.na(telco_phone))>0))

#Calculate the percentage of customers who churn
sum(telco_phone$churn == "1")/nrow(telco_phone)*100
str(telco_phone)
cor(telco_phone$tenure,telco_phone$churn)
# Starting the analysis
# Logit Models

phoneglm1 <- glm(churn ~ tenure + multiplelines + contract + mthlychrgcat, family=binomial (link="logit"),data=telco_phone)
summary(phoneglm1)
# AIC = 643.4
phoneglm2 <- glm(churn ~ partner + dependents + tenure + contract, family=binomial (link="logit"),data=telco_phone)
summary(phoneglm2)
# AIC = 642.46
phoneglm3 <- glm(churn ~ tenure + contract, family=binomial (link="logit"),data=telco_phone)
summary(phoneglm3) 
# AIC = 639.66
phoneglm4 <- glm(churn ~ tenure + contract, family=binomial (link="probit"),data=telco_phone)
summary(phoneglm4)
# AIC = 644.3
stargazer(phoneglm1, phoneglm2, phoneglm3, phoneglm4, title="Logit/Probit Phone Only - Customer Churn Comparison", type="text")


set.seed(6933)
table(telco_phone$churn)
phonetrainIndex = sample(1:nrow(telco_phone), size=round(0.75*nrow(telco_phone)), replace=FALSE)
phonetrain <- telco_phone[phonetrainIndex,]
phonetest  <- telco_phone[-phonetrainIndex,]
dim(phonetrain);dim(phonetest)
# Train = 1,140 rows -- Test = 380 rows -- Total 1,520 rows

logitphonelm1 <- glm(churn ~ tenure + contract, family=binomial (link="probit"),data=phonetrain)
summary(logitphonelm1)
phonetestlm1 <- phonetest[ , c(1:13)]

predlogitphonelm1 <-predict(logitphonelm1, newdata=phonetestlm1,type="response")
hist(predlogitphonelm1,ylim=c(0,600),xlim = c(0,0.26))
predlogitphonelm1 <- ifelse(predlogitphonelm1>0.07, 1, 0)
predlogitphonelm1

table(phonetest$churn,predlogitphonelm1)

hist(predlogitphonelm1,ylim=c(0,1000),main="Churn Predictions on Test Data Set")
abline(h = 256, col="mediumpurple", lwd=3, lty=3)
abline(h = 124, col="mediumpurple", lwd=3, lty=3)

# Verified by hand as well
ClassificationErrorPhoneLM1 <- round(mean(predlogitphonelm1 != phonetest$churn),3)
print(paste("Accuracy = ", 1-ClassificationErrorPhoneLM1))   
phonelm1sensitivity <- round(InformationValue::sensitivity(phonetest$churn,predlogitphonelm1),3)
print(paste("Recall = ",phonelm1sensitivity))
phonelm1precision <- round(InformationValue::precision(phonetest$churn,predlogitphonelm1),3)
print(paste("Precision = ",phonelm1precision))

F1phone <- round((2 * phonelm1sensitivity * phonelm1precision) / (phonelm1sensitivity + phonelm1precision),3)
print(paste("F1 Score = ",F1phone))

prephonelm1 <- prediction(predlogitphonelm1, phonetest$churn)
perfphonelm1 <- performance(prephonelm1, measure = "tpr", x.measure = "fpr")
plot(perfphonelm1)                                

aucphonelm1 <- performance(prephonelm1, measure = "auc")
aucphonelm1 <- aucphonelm1@y.values[[1]]
aucphonelm1 <- round(aucphonelm1,3)
aucphonelm1

##################################################################################################################
################################################# Internet Only ##################################################
##################################################################################################################

telco_intnet <- subset(orig_telco,phoneservice=="No" & internetservice!="No")
unique(telco_intnet$phoneservice)
unique(telco_intnet$internetservice)
cor(telco_intnet$tenure,telco_intnet$churn)

#Clean up the data by making monthlycharges categorical data instead of continuous data
telco_intnet <- telco_intnet %>% 
  mutate(mthlychrgcat = if_else(monthlycharges< median(telco_intnet$monthlycharges), "Less than Median", "More than Median"))
telco_intnet$mthlychrgcat=as.factor(telco_intnet$mthlychrgcat)
telco_intnet = subset(telco_intnet, select = -c(phoneservice,multiplelines,monthlycharges) )

#Check for NA rows and remove if small number of rows are affected
names(which(colSums(is.na(telco_intnet))>0))
telco_intnet<-telco_intnet[!(is.na(telco_intnet[["totalcharges"]])),]
names(which(colSums(is.na(telco_intnet))>0))

#Calculate the percentage of customers who churn
sum(telco_intnet$churn == "1")/nrow(telco_intnet)*100
str(telco_intnet)

# Starting the analysis
# Logit Models

intnetglm1 <- glm(churn ~ tenure + contract + onlinesecurity + onlinebackup + deviceprotection + techsupport + streamingtv + streamingmovies, family=binomial (link="logit"),data=telco_intnet)
summary(intnetglm1)
# AIC = 596.46
intnetglm2 <- glm(churn ~ tenure + contract + partner + mthlychrgcat + dependents + totalcharges  + seniorcitizen + onlinesecurity +techsupport, family=binomial (link="logit"),data=telco_intnet)
summary(intnetglm2)
# AIC = 583.34
intnetglm3 <- glm(churn ~ tenure + contract, family=binomial (link="logit"),data=telco_intnet)
summary(intnetglm3) 
# AIC = 615.21
intnetglm4 <- glm(churn ~ tenure + contract, family=binomial (link="probit"),data=telco_intnet)
summary(intnetglm3) 
# AIC = 604.6
stargazer(intnetglm1, intnetglm2, intnetglm3,intnetglm4, title="Logit/Probit Internet Only Customer Churn Comparison", type="text")


set.seed(6933)
table(telco_intnet$churn)
intnettrainIndex = sample(1:nrow(telco_intnet), size=round(0.75*nrow(telco_intnet)), replace=FALSE)
intnettrain <- telco_intnet[intnettrainIndex,]
intnettest  <- telco_intnet[-intnettrainIndex,]
dim(intnettrain);dim(intnettest)
# Train = 510 rows -- Test = 170 rows -- Total 1,520 rows

logitintnetlm1 <- glm(churn ~ tenure  + contract+ techsupport, family=binomial (link="probit"),data=intnettrain)
summary(logitintnetlm1)
intnettestlm1 <- intnettest[ , c(1:18)]

predlogitintnetlm1 <-predict(logitintnetlm1, newdata=intnettestlm1,type="response")
hist(predlogitintnetlm1,ylim=c(0,100),xlim = c(0,0.7))
predlogitintnetlm1 <- ifelse(predlogitintnetlm1>0.25, 1, 0)
predlogitintnetlm1

table(intnettest$churn,predlogitintnetlm1)

hist(predlogitintnetlm1,ylim=c(0,1000),main="Churn Predictions on Test Data Set")
abline(h = 101, col="mediumpurple", lwd=3, lty=3)
abline(h = 69, col="mediumpurple", lwd=3, lty=3)

ClassificationErrorIntNetLM1 <- round(mean(predlogitintnetlm1 != intnettest$churn),3)
print(paste("Accuracy = ", 1-ClassificationErrorIntNetLM1))   
intnetlm1sensitivity <- round(InformationValue::sensitivity(intnettest$churn,predlogitintnetlm1),3)
print(paste("Recall = ",intnetlm1sensitivity))
intnetlm1precision <- round(InformationValue::precision(intnettest$churn,predlogitintnetlm1),3)
print(paste("Precision = ",intnetlm1precision))

F1internet <- round((2 * intnetlm1sensitivity * intnetlm1precision) / (intnetlm1sensitivity + intnetlm1precision),3)
print(paste("F1 Score = ",F1internet))

preintnetlm1 <- prediction(predlogitintnetlm1, intnettest$churn)
perfintnetlm1 <- performance(preintnetlm1, measure = "tpr", x.measure = "fpr")
plot(perfintnetlm1)                                                

aucintnetlm1 <- performance(preintnetlm1, measure = "auc")
aucintnetlm1 <- aucintnetlm1@y.values[[1]]
aucintnetlm1 <- round(aucintnetlm1,3)
aucintnetlm1


##################################################################################################################
################################################# Both Services ##################################################
##################################################################################################################

telco_both <- subset(orig_telco,phoneservice=="Yes" & internetservice!="No")
unique(telco_both$phoneservice)
unique(telco_both$internetservice)

#Clean up the data by making monthlycharges categorical data instead of continuous data
telco_both <- telco_both %>% 
  mutate(mthlychrgcat = if_else(monthlycharges< median(telco_both$monthlycharges), "Less than Median", "More than Median"))
telco_both$mthlychrgcat=as.factor(telco_both$mthlychrgcat)
telco_both = subset(telco_both, select = -c(monthlycharges) )

#Check for NA rows and remove if small number of rows are affected
names(which(colSums(is.na(telco_both))>0))
telco_both<-telco_both[!(is.na(telco_both[["totalcharges"]])),]
names(which(colSums(is.na(telco_both))>0))

#Calculate the percentage of customers who churn
sum(telco_both$churn == "1")/nrow(telco_both)*100
str(telco_both)
cor(telco_both$tenure,telco_both$churn)

# Starting the analysis
# Logit Models

bothglm1 <- glm(churn ~ tenure + contract + mthlychrgcat, family=binomial (link="logit"),data=telco_both)
summary(bothglm1)
# AIC = 4946
bothglm2 <- glm(churn ~ partner + dependents + tenure + contract, family=binomial (link="logit"),data=telco_both)
summary(bothglm2)
# AIC = 5088
bothglm3 <- glm(churn ~ tenure + contract, family=binomial (link="logit"),data=telco_both)
summary(bothglm3) 
# AIC = 5100.2
bothglm4 <- glm(churn ~ tenure + contract, family=binomial (link="probit"),data=telco_both)
summary(bothglm4) 
stargazer(bothglm1, bothglm2, bothglm3, bothglm4, title="Logit/Probit Both-Service Customer Churn Comparison", type="text")


set.seed(6933)
table(telco_both$churn)
bothtrainIndex = sample(1:nrow(telco_both), size=round(0.75*nrow(telco_both)), replace=FALSE)
bothtrain <- telco_both[bothtrainIndex,]
bothtest  <- telco_both[-bothtrainIndex,]
dim(bothtrain);dim(bothtest)
# Train = 3,624 rows -- Test = 1,208 rows -- Total 4,832 rows

logitbothlm1 <- glm(churn ~ tenure + contract + dependents + mthlychrgcat, family=binomial (link="probit"),data=bothtrain)
summary(logitbothlm1)
bothtestlm1 <- bothtest[ , c(1:20)]

predlogitbothlm1 <-predict(logitbothlm1, newdata=bothtestlm1,type="response")
hist(predlogitbothlm1,ylim=c(0,400),xlim = c(0,0.8))
predlogitbothlm1 <- ifelse(predlogitbothlm1>0.35, 1, 0)
predlogitbothlm1

table(bothtest$churn,predlogitbothlm1)

hist(predlogitbothlm1,ylim=c(0,1000),main="Churn Predictions on Test Data Set")
abline(h = 643, col="mediumpurple", lwd=3, lty=3)
abline(h = 565, col="mediumpurple", lwd=3, lty=3)

ClassificationErrorBothLM1 <- round(mean(predlogitbothlm1 != bothtest$churn),3)
print(paste("Accuracy = ", 1-ClassificationErrorBothLM1))   
bothlm1sensitivity <- round(InformationValue::sensitivity(bothtest$churn,predlogitbothlm1),3)
print(paste("Recall = ",bothlm1sensitivity))
bothlm1precision <- round(InformationValue::precision(bothtest$churn,predlogitbothlm1),3)
print(paste("Precision = ",bothlm1precision))

F1both <- round((2 * bothlm1sensitivity * bothlm1precision) / (bothlm1sensitivity + bothlm1precision),3)
print(paste("F1 Score = ",F1both))

prebothlm1 <- prediction(predlogitbothlm1, bothtest$churn)
perfbothlm1 <- performance(prebothlm1, measure = "tpr", x.measure = "fpr")
plot(perfbothlm1)                                               

aucbothlm1 <- performance(prebothlm1, measure = "auc")
aucbothlm1 <- aucbothlm1@y.values[[1]]
aucbothlm1 <- round(aucbothlm1,3)
aucbothlm1

