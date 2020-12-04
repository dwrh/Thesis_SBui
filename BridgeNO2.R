library(haven)
library(tidyverse)
library(lfe)
library(stargazer)
setwd("~/RProject/Data")
datab <- read.csv("NO2.csv")
#Fixed effects on for months and locations
#Positive estimate at 1% significance level for bridge on No2 column density
reg1 <- felm(NO2 ~ bridge| name + month, datab)
summary(reg1)

#Fixed effects on for months, locations, and year
#Negative but insignificant estimate for bridge on No2 column density
reg2 <- felm(NO2 ~ bridge|name + month + year, datab)
summary(reg2)

#Fixed effects on for months and year
#Negative estimate at 10% significant level for bridge on No2 column density
reg3 <- felm(NO2 ~ bridge| month + year, datab)
summary(reg3)
reg<- stargazer(reg1, reg2,reg3, type = "text")

