rm(list=ls())
#----------------importing data-----------------------------------------------------------
library(readxl)
master.dataset = read_excel("Lung_Cancer.xlsx")

#---------------Data Preprocessing -------------------------------------------------------

#converting columns names to lower case. 
colnames(master.dataset) = tolower(make.names(colnames(master.dataset)))
attach(master.dataset)
summary(master.dataset)
nrow(master.dataset)
ncol(master.dataset)

#checking for missing values
colnames(master.dataset)[colSums(is.na(master.dataset)) > 0]
colSums(is.na(master.dataset))

#---------------------converting into factors---------------------------------------
library(dplyr)
master.dataset$cell.type = recode_factor(master.dataset$cell.type, 
                                         "1"="squamous", "2"="small cell", "3"="adeno", "4"="large")
levels(master.dataset$cell.type)
  master.dataset$prior.chemotherapy = recode_factor(master.dataset$prior.chemotherapy, 
                                           "0"="no", "10"="yes")
  levels(master.dataset$prior.chemotherapy)

#---------------------exploratory data analysis---------------------------------------
length(unique(survival))                 # Count of unique values in the Time Column
unique(survival)                         
summary(survival)
table(treatment)
table(status)
hist(survival, col="skyblue")
hist(months.from.diagnosis)
hist(age)
table(prior.chemotherapy)

library(lattice)
xyplot(status ~ survival | cell.type, data=master.dataset)
densityplot(~survival | cell.type, data=master.dataset)
xyplot(survival ~ age | prior.chemotherapy, data=master.dataset)


#---------------------------non parametric------------------------------------------------
library(survival)
np <- survfit(Surv(survival, status) ~ treatment)      
summary(np)
plot(np, xlab="survival_days", ylab="Survival Probability",
     main = "Standard Treatment vs Test Treatment", col = 5:3, lwd = 3)
legend("topright", c("Standard Treatment","Test Treatment"), col=5:3, pch=19)

summary(np, times=365.00)
summary(np, times=183.00)

# Cox proportional hazard model - coefficients and hazard rates
  cox <- coxph(Surv(survival, status) ~ cell.type + treatment 
               + log(months.from.diagnosis) + age + prior.chemotherapy + karnofsky.score, method="breslow")
summary(cox)


# Exponential, Weibull, and log-logistic parametric model coefficients
exp <- survreg(Surv(survival, status) ~ cell.type + treatment 
               + log(months.from.diagnosis) + age + prior.chemotherapy + karnofsky.score, dist="exponential")
summary(exp)

loglogistic <- survreg(Surv(survival, status) ~ cell.type + treatment 
                       + log(months.from.diagnosis) + age + prior.chemotherapy + karnofsky.score, dist="loglogistic")
summary(loglogistic)

library(stargazer)
stargazer(cox, exp, loglogistic, type="text")
