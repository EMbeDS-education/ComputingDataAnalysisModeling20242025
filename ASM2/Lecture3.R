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

##create the url where the example dataset is located
url <- "http://web1.sph.emory.edu/dkleinb/allDatasets/surv2datasets/addicts.dta"
##read the file from the url
addicts <- data.frame(read_dta(url))

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

##fith the Cox model focusing on assessing the effect of clinic
coxph(Surv(survt, status) ~ clinic, data = addicts)

m1<-coxph(Surv(survt, status) ~ factor(clinic), data = addicts)

summary(m1)

ci.m1<-exp(confint(m1))

##plot hazard ratio and CI

sjPlot::plot_model(m1,
                  axis.lim = c(min(ci.m1), max(ci.m1)),
                  auto.label = F)

##check the PH assumption using hypothesis testing 

cox.zph(m1)

##results suggest to reject the null hypothesis of proportionality of hazard (PH)

##PH assumption could be also assessed by visual check and evaluating than HR is constant over time

plot(cox.zph(m1))

##if the PH assumption is not met we could adapt a Cox model considering the intercation between the covariate and a function of time
##just consider the model with the interaction term between clinic and survival time (different function forms could be considered for time in the interaction)
##before interpreting coefficients in the model that is not straightforward check if PH assumption is met

cox.zph(coxph(Surv(survt, status) ~ clinic*survt, data = addicts))
##considering the interaction the PH assumption is not violated (!!!considering we are including in the model and effect for clinic that depends on time!!)


##fit the Cox model focusing on assessing the effect of prison and evaluate the PH assumption

m2<-coxph(Surv(survt, status) ~ prison, data = addicts)
summary(m2)
cox.zph(m2)

##evaluate the additive and the multiplicative model as general steps to be considered in model development to chech significance of the model and model assumptions
m3<-coxph(Surv(survt, status) ~ clinic+prison, data = addicts)
summary(m3)
cox.zph(m3)

m4<-coxph(Surv(survt, status) ~ clinic*prison, data = addicts)
summary(m4)
cox.zph(m4)

