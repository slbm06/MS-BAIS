rm(list=ls())
library(rio)
library(survival)
library(survminer)
library(stargazer)
lungc <- import("Assignment 2 - Problem 2 - LungCancer.xlsx")
colnames(lungc)=tolower(make.names(colnames(lungc)))

str(lungc)
attach(lungc)
table(status)

# Question 1
kmlungc1 <- survfit(Surv(survivaldays, status) ~ 1)     
kmlungc1
# mark.time=TRUE shows the censored observations on the graph with tick marks
plot(kmlungc1, xlab="Survival Days", conf.int=F,ylab="Survival Probability",mark.time=TRUE, col="steelblue", main='Survival Probabilities for All Participants',lwd=2)
abline(v = 183, col="mediumpurple", lwd=2, lty=3)
abline(v = 365, col="mediumpurple", lwd=2, lty=3)
abline(h=0.5, col="mediumpurple",lty=2)

kmlungc2 <- survfit(Surv(survivaldays, status) ~ treatment)
kmlungc2
summary(kmlungc2)
# steelblue = treatment of 1 (standard)/ blue = treatment of 2 (test)
# mark.time=TRUE shows the censored observations on the graph with tick marks
plot(kmlungc2, xlab="Survival Days", conf.int=F,ylab="Survival Probability",mark.time=TRUE,main='Survival Probabilities Based on Treatment Type', col=c("steelblue","hotpink3"),lwd=2)
legend(400,1, legend=c("No Treatment","Test Treatment"), lty=1,lwd=2,col=c("steelblue","hotpink3"),bty="",cex=1.5)
abline(v = 183, col="mediumpurple", lwd=2, lty=3)
abline(v = 365, col="mediumpurple", lwd=2, lty=3)
abline(h=0.11, col="hotpink3",lty=2)
abline(h=0.07, col="steelblue",lty=2)
abline(h=0.215, col="magenta",lty=2)

# Log-Rank-Test / Ho: survival in the two groups is the same / Ha: survival is not the same
survdiff(Surv(survivaldays, status) ~ treatment)
# p-0.9 -> cannot reject null -> no evidence that survival is changed by the test treatment.

# mean number of days where a patient can be expected to survive if they are on the standard vs the test treatment
standard_treat <- subset(lungc,treatment=="1")
test_treat <- subset(lungc,treatment=="2")
mean(standard_treat$survivaldays)
mean(test_treat$survivaldays)

# Question 2
coxlungc <- coxph(Surv(survivaldays, status) ~ treatment + ageinyrs + monthsfromdiagnosis, data=lungc, method="breslow")
summary(coxlungc)

explungc <- survreg(Surv(survivaldays, status) ~ treatment + ageinyrs + monthsfromdiagnosis, data=lungc, dist="exponential")
summary(explungc)

weibulllungc <- survreg(Surv(survivaldays, status) ~ treatment + ageinyrs + monthsfromdiagnosis, data=lungc, dist="weibull")
summary(weibulllungc)

loglogisticlungc <- survreg(Surv(survivaldays, status) ~ treatment + ageinyrs + monthsfromdiagnosis, data=lungc, dist="loglogistic")
summary(loglogisticlungc)

stargazer(coxlungc, explungc, weibulllungc, loglogisticlungc, type="text")

