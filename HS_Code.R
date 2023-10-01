rm(list=ls())
library(readxl)
library(Hmisc) 
##library(MuMIn)
##library(foreach)
library(dplyr)
#load data

setwd("C:/Users/16286/Desktop/MSBA/Information, Insights/HW2/HW2")
multdata <- read_excel("HW2_MultimediaHW.xlsx")

#obtain summary stats for the data
summary(multdata)
colnames(multdata)

plot(multdata)
pairs(multdata[2:12], panel = panel.smooth)


# We can drop Social Media as there is no data
multdata <- multdata[-c(9)]
View(multdata)
colnames(multdata)[2] <- c("Sales")


Med1 <- multdata$ADV_Offline
Med2 <- multdata$Catalogs_ExistCust

# Option 1a (log on all variables without intercept) {Best performance until now}

SqM1= log(1+(multdata$Catalogs_ExistCust))
SqM2=log(1+(multdata$Catalogs_NewCust))
SqM3=log(1+(multdata$Search))
SqM4=log(1+(multdata$Newsletter))
SqM5=log(1+(multdata$Portals))

Stm1<-Lag(multdata$Sales,shift=1)

LStm1 <- log(1+Stm1)

Model1 <- lm(multdata$Sales~SqM1 + SqM2 + SqM3 + SqM4+SqM5 +Stm1-1)
summary(Model1)
AIC(Model1)#659.7074
BIC(Model1)#671.7024
#.9832 and .9802 r2 and adjusted r2

#Option 1b (log on all with intercept)
Model2 <- lm(multdata$Sales~SqM1 + SqM2 + SqM3 + SqM4+SqM5 +LStm1)
summary(Model2)
AIC(Model2)#656.2948
BIC(Model2)#670.0034

#Option 1c

SqM6 = log(1+multdata$Mailings)
SqM7 = log(1+multdata$Catalogs_Winback)
SqM8 = log(1+multdata$Retargeting)

Model3 <- lm(multdata$Sales~SqM1 + SqM2 + SqM3 + SqM4+SqM5+SqM6 + SqM7 + SqM8 + LStm1-1)
summary(Model3)#.9886, .9854
AIC(Model3)#649.6314
BIC(Model3)#666.7672
anova(Model1, Model3)# Significant with p val as .005248



#Option 2a (Linearly related variables without intercept)

SqM1= (multdata$Catalogs_ExistCust)
SqM2= (multdata$Catalogs_NewCust)
SqM3= (multdata$Search)
SqM4= (multdata$Newsletter)
SqM5= (multdata$Portals)

Stm1<-Lag(multdata$Sales,shift=1)

LSales <- log(multdata$Sales)

LStm1 <- log(1+Stm1)

Model1 <- lm(LSales~SqM1 + SqM2 + SqM3 + SqM4+SqM5 +LStm1-1)
summary(Model1)#98.1,97.77%
AIC(Model1)#664.7335
BIC(Model1)#676.7285

#Option 2b (Linearly related with intercept)
Model2 <- lm(multdata$Sales~SqM1 + SqM2 + SqM3 + SqM4+SqM5 +LStm1)
summary(Model2)#25.99,12.93
AIC(Model2)#666.7148
BIC(Model2)#680.4235

#Option 2c (Comparing the performance by adding 3 more variables which have around 25-30 0 values)

SqM6 = multdata$Mailings
SqM7 = multdata$Catalogs_Winback
SqM8 = multdata$Retargeting

Model3 <- lm(multdata$Sales~SqM1 + SqM2 + SqM3 + SqM4+SqM5+SqM6 + SqM7 + SqM8 + LStm1-1)
summary(Model3)#98.24,97.74
AIC(Model3)#667.6533
BIC(Model3)#684.789
anova(Model1, Model3)#Not significant 0.4861

#Option 3a (Square root on all Sales Variable and log on the Sales lag term without intercept)

SqM1= sqrt(multdata$Catalogs_ExistCust)
SqM2= sqrt(multdata$Catalogs_NewCust)
SqM3= sqrt(multdata$Search)
SqM4= sqrt(multdata$Newsletter)
SqM5= sqrt(multdata$Portals)

Stm1<-Lag(multdata$Sales,shift=1)

LStm1 <- log(1+Stm1)

Model1 <- lm(multdata$Sales~SqM1 + SqM2 + SqM3 + SqM4+SqM5 +LStm1-1)
summary(Model1) #98.21, 97.91
AIC(Model1)#662.2044
BIC(Model1)#674.1994
 
#Option 3b (Square root on all Sales Variable and log on the Sales lag term with intercept)
Model2 <- lm(multdata$Sales~SqM1 + SqM2 + SqM3 + SqM4+SqM5 +LStm1)
summary(Model2)#30.73,18.5
AIC(Model2)#664.00
BIC(Model2)#677.71

# Option 3c (Using all the variables)
SqM6 = sqrt(multdata$Mailings)
SqM7 = sqrt(multdata$Catalogs_Winback)
SqM8 = sqrt(multdata$Retargeting)

Model3 <- lm(multdata$Sales~SqM1 + SqM2 + SqM3 + SqM4+SqM5+SqM6 + SqM7 + SqM8 + LStm1-1)
summary(Model3)#98.47,98.05
AIC(Model3)#661.7149
BIC(Model3)#678.8506
anova(Model1, Model3)#Not significant 0.1617

#Option 4a




