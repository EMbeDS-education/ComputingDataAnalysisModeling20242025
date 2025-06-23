##load necessary packages (after having installed them from menu or using install.packages())

library(ggsurvfit)
library(readxl)
library(dplyr)
library(haven) 
library(survival)
library(survBootOutliers)
library(sjPlot)

##set working directory to read and save file without the need to always specify the path
##set you working directory
setwd("C:/Users/Utente/OneDrive - Scuola Superiore Sant'Anna/ASM2/AA2425")

##read the file importing stata dataset
addicts <- haven::read_dta("datasets/classes/addicts.dta")

##plot the cumulative survival 
survfit2(Surv(survt, status) ~ clinic, data = addicts) %>% 
  ggsurvfit( ) +
  labs(
    x = "Time (Days)",
    y = "Cumulative survival function"
  )

##and the cumulative risk function instead of cumulative survival specifying the function required for the ggsurvfit command
survfit2(Surv(survt, status) ~ clinic, data = addicts) %>% 
  ggsurvfit( type="risk") +
  labs(
    x = "Time (Days)",
    y = "Cumulative risk function"
  )

##fit the Cox model focusing on assessing the effect of methadone dose
m1<-coxph(Surv(survt, status) ~ dose, data = addicts)

summary(m1)

##check the PH assumption using hypothesis testing 
cox.zph(m1)

mres.m1<-resid(m1,type="martingale")
cov.m1<-addicts$dose
plot(mres.m1 ~ cov.m1, col = "darkgray",
     ylab = "Martingale Residual",
     xlab = "Methadone dose",
     main = "Residuals vs. Predictor")
abline(h = 0)
lines(smooth.spline(mres.m1 ~ cov.m1, df = 7), lty = 2, lwd = 2)

##fit the Cox model focusing on assessing the effect of methadone dose and clinic
m2<-coxph(Surv(survt, status) ~ dose+clinic, data = addicts)
summary(m2)

##check the PH assumption using hypothesis testing as we have already seen that clinic do not satisfy PH assumption
cox.zph(m2)
##nor clinic nor the overall model including methadone and clinic satisfy PH assumption
##to overcome the problem of try to fit the model using clinic as strata

ms1<-coxph(Surv(survt, status) ~ dose+strata(clinic), data = addicts)
summary(ms1)

cox.zph(ms1)

mres.ms1<-resid(ms1,type="martingale")
cov.ms1<-addicts$dose
plot(mres.ms1 ~ cov.ms1, col = "darkgray",
     ylab = "Martingale Residual",
     xlab = "Methadone dose",
     main = "Residuals vs. Predictor")
abline(h = 0)
lines(smooth.spline(mres.ms1 ~ cov.ms1, df = 7), lty = 2, lwd = 2)


###Competing risk analysis##

require(cmprsk)

##require(tidycmprsk)
bmtcrr <- read.csv("datasets/classes/bmtcrr.csv", sep=";")

cuminc(bmtcrr$ftime, bmtcrr$Status, cencode=0)

plot(cuminc(bmtcrr$ftime, bmtcrr$Status, cencode=0))

cuminc(Surv(ftime, Status) ~ 1, data = bmtcrr) %>% 
  ggcuminc() + 
  labs(
    x = "Months"
  ) + 
  add_confidence_interval() +
  add_risktable()

sph<-summary(survfit(Surv(bmtcrr$ftime, bmtcrr$Statusph)~ 1, data=bmtcrr),times=c(20,40,60,80,100,120))[8]

mc1<-crr(bmtcrr$ftime, bmtcrr$Status, bmtcrr$Age)

summary(mc1)


mph1<-coxph(Surv(ftime, Status) ~ Age,data=bmtcrr)

summary(mph1)

bmtcrr$Statusph<-bmtcrr$Status
bmtcrr$Statusph[bmtcrr$Status==2]<-0

mph1<-coxph(Surv(ftime, Statusph) ~ Age,data=bmtcrr)

summary(mph1)

bmtcrr$Sex01[bmtcrr$Sex=="M"]<-1
bmtcrr$Sex01[bmtcrr$Sex=="F"]<-0

mc2<-crr(bmtcrr$ftime, bmtcrr$Status, cbind(bmtcrr$Age,bmtcrr$Sex01))
summary(mc2)


mph2<-coxph(Surv(ftime, Statusph) ~ Age+Sex01,data=bmtcrr)
summary(mph2)

#Influential observations
m1.betares <- resid(m1, type = "dfbetas")

plot(m1.betares)
##fit the model using dose as covariate

survfit2(Surv(survt, status) ~ prison, data = addicts) %>% 
  ggsurvfit( type="risk") +
  labs(
    x = "Time (Days)",
    y = "Cumulative risk function"
  )

m2<-coxph(Surv(survt, status) ~ prison, data = addicts)
summary(m2)

m3<-coxph(Surv(survt, status) ~ clinic+prison, data = addicts)
summary(m3)

m4<-coxph(Surv(survt, status) ~ clinic*prison, data = addicts)
summary(m4)

m3<-coxph(Surv(survt, status) ~ dose, data = addicts)


