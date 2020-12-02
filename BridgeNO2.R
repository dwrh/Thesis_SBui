library(haven)
library(tidyverse)
library(lfe)
library(stargazer)
setwd("~/RProject/Data")

#Regression on NO2
datab <- read.csv("NO2.csv")
names(datab) <- c("NW", "W","SW", "N", "NO2", "S", "NE", "E", "SE", "date", "month", "name", "year", "bridge")

# Make a dataset with the two highest averages in the neighborhood
n <- list("NW", "W","SW", "N", "S", "NE", "E", "SE")
b <- unique(datab$name)
print(b)
mn <- list()
datan <- data.frame()
for (i in b) {
  datasub <- datab[datab$name == i, ]
  for (i in n) {
    m <- mean(datasub[[i]])
    mn[[i]] <- m
  }
  m1 <- which.max(mn)
  m2 <- names(which.max(mn[- get("m1")]))
  keep <- c(names(m1), m2, "NO2", "date", "month", "name", "year", "bridge")
  datasub2 <- datasub[keep]
  names(datasub2) <- c("m1", "m2", "NO2", "date", "month", "name", "year", "bridge")
  datan <- rbind(datan,datasub2)
}
datan$meancontrol <- (datan$m1 + datan$m2)/2
datan$meantotal <- (datan$m1 + datan$m2 + datan$NO2)/3
datab$meancontrol <- (datab$N + datab$NW + datab$W + datab$SW + datab$S + datab$NE + datab$E + datab$SE)/8
datab$meantotal <- (datab$N + datab$NW + datab$W + datab$SW + datab$S + datab$NE + datab$E + datab$SE + datab$NO2)/9

#Fixed effects on for months and locations
#negative estimate at 1% significance level for bridge on No2 column density
reg1 <- felm(NO2 ~ bridge + year| month, datab)
summary(reg1)
reg2 <- felm(NO2 ~ bridge + year| month + name, datab)
summary(reg2)
regfull1 <- lm(NO2~ bridge + year + factor(month)+factor(name), datab)
regfull2 <- lm(NO2~ bridge + year + factor(month), datab)
#Run control for pixels surrounding the bridge pixel, for each pixel and mean
#Negative but insignificant estimate for bridge on No2 column density
regcnw <- felm(NW ~ bridge + year|name + month, datab)
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
regmcf <- lm(meancontrol~ bridge + year + factor(month)+factor(name), datab)
regmt <- felm(meantotal ~ bridge+ year|name + month, datab)
summary(regmt)
regmtf <- lm(meantotal~ bridge + year + factor(month)+factor(name), datab)

regm1a <- felm(m1 ~ bridge + year |name + month, datan)
regm1b <- felm(m1 ~ bridge + year |month, datan)
regm2a <- felm(m2 ~ bridge + year |name + month, datan)
regm2b <- felm(m2 ~ bridge + year |month, datan)
regm1af <- lm(m1~ bridge + year + factor(month)+factor(name), datan)
regm1bf <- lm(m1~ bridge + year + factor(month), datan)
regm2af <- lm(m2~ bridge + year + factor(month)+factor(name), datan)
regm2bf <- lm(m2~ bridge + year + factor(month), datan)
regmmtf <- felm(meantotal~ bridge + year | month + name, datan)
regmmt <- felm(meantotal~ bridge + year | month, datan)
regmmcf <- felm(meancontrol~ bridge + year | month + name, datan)
regmmc <- felm(meancontrol~ bridge + year | month , datan)
stargazer(reg1,reg2, type = "text")
stargazer(regmc, regmt, type = "text")
stargazer(regm1a, regm2a, regm2a, regm2b, regmmt, regmmc, type = "text")
stargazer(regmmtf, regmmt, regmmcf, regmmc, type = "text")



#Regression on Aerosol Optical Depth 049
dataa <- read.csv("Aerosol047.csv")
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

stargazer(rega1, rega2, rega51, rega52, type="text")
