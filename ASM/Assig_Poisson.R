# About Dataset
# The New York City Department of Transportation collects daily data about the 
# number of bicycles going over bridges in New York City. This data is used to 
# measure bike utilization as a part of transportation planning. This dataset 
# is a daily record of the number of bicycles crossing into or out of Manhattan 
# via one of the East River bridges (that is, excluding Bronx thruways and the 
# non-bikeable Hudson River tunnels) for 9 months.
#
# Content
# A count of the number of bicycles on each of the bridges in question is provided 
# on a day-by-day basis, along with information on maximum and minimum temperature and precipitation.
#
# Research Questions 
# In this dataset, how many bicycles cross into and out of Manhattan per day?
# How strongly do weather conditions affect bike volumes?
# What is the top bridge in terms of bike load?



# Step 1: Install and Load Required Packages
library(dplyr)
library(tidyverse)
library(GGally)
library(ggplot2)
library(pROC)
library(stats)
library(nnet)
library(MASS)    # For ordinal logistic regression
library(pscl)    # For calculating Pseudo R²
library(ggcorrplot)
library(smotefamily)
library(psych)


# POISSON REGRESSION - General / Over-dispersed / Zero-Saturated

# Step 2 - Load the db
db <- read.csv("/path_to/file.csv")


# Step 3 - Pre processing the db

################################################################################
# Step 4 - Exploratory Data Analysis
# Check for missing values
# Descriptive statistics: Rresponse Variable Vs Predictor/s
# Check Poisson Regression Assumption
# 1) Linearity Assumptions 2) 3)

################################################################################

# Step 5 - Poisson Regression Model: 

# Adding covariates and interactions
# Compare models
# Goodness of Fit

# Step 6 - Advanced Model
# Overdispersed - Negative binomial model - Zero Inflated
