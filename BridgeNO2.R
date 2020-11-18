library(haven)
library(tidyverse)
library(lfe)
library(stargazer)
setwd("~/RProject/Data")

#Regression on NO2
datab <- read.csv("NO2.csv")
names(datab) <- c("NW", "W","SW", "N", "NO2", "S", "NE", "E", "SE", "date", "month", "name", "year", "bridge")
datab$meancontrol <- (datab$N + datab$NW + datab$W + datab$SW + datab$S + datab$NE + datab$E + datab$SE)/8
datab$meantotal <- (datab$N + datab$NW + datab$W + datab$SW + datab$S + datab$NE + datab$E + datab$SE + datab$NO2)/9

#Fixed effects on for months and locations
#negative estimate at 1% significance level for bridge on No2 column density
reg1 <- felm(NO2 ~ bridge + year| month, datab)
summary(reg1)
reg2 <- felm(NO2 ~ bridge + year| month + name, datab)
summary(reg2)
regfull <- lm(NO2~ bridge + year + factor(month)+factor(name), datab)
summary(regfull)
#Run control for pixels surrounding the bridge pixel, for each pixel and mean
#Negative but insignificant estimate for bridge on No2 column density
regcnw <- felm(NW ~ bridge + year|name + month, datac)
summary(regcnw)
regcw <- felm(W ~ bridge + year|name + month, datab)
summary(regcw)
regcsw <- felm(SW ~ bridge + year|name + month, datab)
summary(regcsw)
regcn <- felm(N ~ bridge + year|name + month, datab)
summary(regcn)
regcs <- felm(S ~ bridge+ year|name + month, datab)
summary(regcs)
regcne <- felm(NE ~ bridge+ year|name + month, datab)
summary(regcne)
regce <- felm(E ~ bridge+ year|name + month, datab)
summary(regce)
regcse <- felm(SE ~ bridge+ year|name + month, datab)
summary(regcse)
regmc <- felm(meancontrol ~ bridge+ year|name + month, datab)
summary(regmc)
regmt <- felm(meantotal ~ bridge+ year|name + month, datab)
summary(regmt)
#Fixed effects on for months and year
#Negative estimate at 10% significant level for bridge on No2 column density
reg3 <- felm(NO2 ~ bridge| month + year, datab)
summary(reg3)
reg<- stargazer(reg1, reg2,reg3, type = "text")

#Regression on Aerosol Optical Depth 049
dataa <- read.csv("Aerosol.csv")
rega1 <- felm(mean ~ year + bridge |name + month, dataa) 
summary(rega1)
rega2 <- felm(mean ~ year + bridge | month, dataa)
summary(rega2)

#Regression on Aerosol Optical Depth 055
dataa5 <- read.csv("Aerosol055.csv")
rega51 <- felm(mean ~ year + bridge |name + month, dataa5) 
summary(rega51)
rega52 <- felm(mean ~ year + bridge | month, dataa5) 
summary(rega52)
