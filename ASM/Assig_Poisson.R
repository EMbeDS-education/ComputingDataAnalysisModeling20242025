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