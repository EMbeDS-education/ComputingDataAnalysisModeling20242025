##load necessary packages (after having installed them from menu or using install.packages())

library(ggsurvfit)
library(readxl)
library(dplyr)
library(haven) 
library(survival)
library(survBootOutliers)

##set working directory to read and save file without the need to always specify the path
##set you working directory
setwd("C:/Users/Utente/OneDrive - Scuola Superiore Sant'Anna/ASM2/AA2425")

##create the url where the example dataset is located
url <- "http://web1.sph.emory.edu/dkleinb/allDatasets/surv2datasets/addicts.dta"
##read the file from the url
addicts <- data.frame(read_dta(url))

##have a look to data
head(addicts)
str(addicts)

##create a survival object identifying time variable and event variable, the object created could be used in models
##R create an object made up of the survival time for each subject and identify with a "plus" subjects' censored
Surv(addicts$survt, addicts$status)

##quick look on the first 20 observation to better see data
Surv(addicts$survt, addicts$status) [1:20]

##use the survfit command to have the Kaplan-Meier estimator
##the survifit allow creating the Kaplan-Meier estimator overall and by groups, 
##and eventual covariates identifying groups are introduced after the tilde, use 1 to have the overall estimator

surv.dat<-survfit(Surv(addicts$survt, addicts$status)~ 1, data=addicts)

##have a look to the object created

str(surv.dat)

##evaluate median survival time

survfit(Surv(addicts$survt, addicts$status)~ 1, data=addicts)

##compare estimates of median survival time with the "incorrect" median of the time variable

median(addicts$survt)

median(addicts$survt[addicts$status==1])

addicts %>% 
  filter(status == 1) %>% 
  summarize(median.surv = median(survt))


##obtain estimates of the cumulative survival function at specified time-points
##we could evaluate the cumulative survival function at a single time point

summary(survfit(Surv(addicts$survt, addicts$status)~ 1, data=addicts),times=365.25)

##or at multiple time-points

summary(survfit(Surv(addicts$survt, addicts$status)~ 1, data=addicts),times=c(30,60,90,180,365.25))

##use the survfit2 function to create "nice" Kaplan-Meier curves
survfit2(Surv(survt, status) ~ 1, data = addicts) %>% 
  ggsurvfit() +
  labs(
    x = "Time (Days)",
    y = "Cumulative survival function"
  )

##obtain the plot also showing 95% confidence intervals
survfit2(Surv(survt, status) ~ 1, data = addicts) %>% 
  ggsurvfit() +
  labs(
    x = "Time (Days)",
    y = "Cumulative survival function"
  ) +
 add_confidence_interval() 

##obtain the plot also showing both 95% confidence intervals and risk table

survfit2(Surv(survt, status) ~ 1, data = addicts) %>% 
  ggsurvfit() +
  labs(
    x = "Time (Days)",
    y = "Cumulative survival function"
  ) +
  add_confidence_interval() +
  add_risktable()

##compare KM curves according to the clinic

survfit2(Surv(survt, status) ~ clinic, data = addicts) %>% 
  ggsurvfit() +
  labs(
    x = "Time (Days)",
    y = "Cumulative survival function"
  ) +
  add_confidence_interval() +
  add_risktable()

##test for differences in KM curves depending on clinic
survdiff(Surv(survt, status) ~ clinic, data = addicts)

##plot the cumulative risk function instead of cumulative survival specifying the function required for the ggsurvfit command
survfit2(Surv(survt, status) ~ clinic, data = addicts) %>% 
  ggsurvfit( type="risk") +
  labs(
    x = "Time (Days)",
    y = "Cumulative risk function"
  ) +
  add_confidence_interval() +
  add_risktable()

##compare KM curves according to being imprisoned or not

survfit2(Surv(survt, status) ~ prison, data = addicts) %>% 
  ggsurvfit() +
  labs(
    x = "Time (Days)",
    y = "Cumulative survival function"
  ) +
  add_confidence_interval() +
add_risktable()

##test for differences in KM curves depending on being imprisoned or not
survdiff(Surv(survt, status) ~ prison, data = addicts)
