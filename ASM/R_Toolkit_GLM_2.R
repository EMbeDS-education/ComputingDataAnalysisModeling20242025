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

# Step 2 - Household Size in the Philippines>
# Load the db
db_original <- read.csv("/Users/claudiomazzi/Documents/PhD/Course_AppSTAT/2_GLM/db/Fam_Income_Exp.csv")
unique(db_original$Region)

# Pre processing the db
db_ph <- db_original %>% 
  dplyr::select(Region, 
                Age = Household.Head.Age, 
                Roof = Type.of.Roof, 
                NumLT5 =Members.with.age.less.than.5.year.old, 
                Total = Total.Number.of.Family.members) %>%
         mutate(Total = Total - 1)

db_ph <- db_ph %>% 
  mutate(Roof = case_when
                ( Roof == "Strong material(galvanized,iron,al,tile,
                            concrete,brick,stone,asbestos)" ~ "Pred. Strong Material",
                  Roof == "Light material (cogon,nipa,anahaw)" ~ "Pred. Light Material",
                  Roof == "Mixed but predominantly strong materials" ~ "Pred. Strong Material",
                  Roof == "Mixed but predominantly light materials" ~ "Pred. Light Material",
                  Roof == "Salvaged/makeshift materials" ~ "Pred. Light Material",
                  Roof == "Mixed but predominantly salvaged materials" ~ "Pred. Light Material"
                  )
                )
db_ph <- db_ph %>% 
  filter(Roof != "Not Applicable")

db_ph <- db_ph %>% 
  filter(Region %in% c("VII - Central Visayas", "III - Central Luzon", 
                  "XI - Davao Region", "I - Ilocos Region"))
db_ph <- db_ph %>% 
  mutate(Region = case_when
         ( Region == "VII - Central Visayas" ~ "Visayas",
           Region == "III - Central Luzon" ~ "Central Luzon",
           Region == "XI - Davao Region" ~ "Davao Region",
           Region == "I - Ilocos Region" ~ "Ilocos Region"
         )
  )
head(db_ph)
str(db_ph)
summary(db_ph)
dim(db_ph)

################################################################################
# Exploratory Data Analysis
# Check for missing values
sum(is.na(db_ph))

# Descriptive statistics Total(response Variable Vs Predictor)
describe(db_ph$Total)
stats_by_roof <- aggregate(Total ~ Roof, data = db_ph, FUN = function(x) 
  c(mean = mean(x, na.rm = TRUE), variance = var(x, na.rm = TRUE)))
stats_by_roof

stats_by_Region <- aggregate(Total ~ Region, data = db_ph, FUN = function(x) 
  c(mean = mean(x, na.rm = TRUE), variance = var(x, na.rm = TRUE)))
stats_by_Region

ggplot(db_ph, aes(x = Total, fill= after_stat(count))) +
  geom_histogram(binwidth = 1, color = "black", fill = "steelblue", position = "identity") +
  theme_minimal() +
  labs(
    title = "Poisson-like Distribution of Household Size",
    x = "Total number of people (excluding head)",
    y = "Count"
  ) 


# Distribution among Age classes
db_ph <- db_ph %>%
  mutate(Age_Group = cut(Age, 
                         breaks = c(15, 25, 35, 45, 55, 65, 75, Inf),
                         labels = c("15-25", "25-35", "35-45", "45-55", "55-65", "65-75", "75+"),
                         right = FALSE))  # Left closed intervals

# Plot
ggplot(db_ph, aes(x = Total, fill = ..count..)) +
  geom_histogram(binwidth = 1, color = "black", position = "identity") +
  theme_minimal() +
  labs(
    title = "Poisson-like Distribution of Household Size by Age Group",
    x = "Total number of people (excluding head)",
    y = "Count"
  ) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +  # Gradient color
  facet_wrap(~ Age_Group, ncol = 2) +  # Dividing in Age classes
  theme(
    legend.position = "right",  # No Legenda
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  )


# Mean - Variance by age groups
summary_table <- db_ph %>%
  group_by(Age_Group) %>%
  summarise(
    Mean_Total = mean(Total, na.rm = TRUE),
    Variance_Total = var(Total, na.rm = TRUE),
    Observations = n(),
    .groups = "drop"
  )
print(summary_table)

# Linearity Assumptions
# Mean 'Total' by 'agew'
log_means <- db_ph %>%
  group_by(Age) %>%
  summarise(
    Mean_Total = mean(Total, na.rm = TRUE), 
    Log_Mean_Total = log(Mean_Total)        
  ) %>%
  filter(!is.na(Log_Mean_Total)) # Deleting undefined log

# Plot
ggplot(log_means, aes(x = Age, y = Log_Mean_Total)) +
  geom_point(color = "black", size = 2) +                           
  geom_smooth(method = "loess", color = "blue", fill = "grey80", se = TRUE) +
  labs(
    title = "Log of Mean Household Sizes by Age of Head of Household",
    x = "Age of Head of Household",
    y = "Log of Mean Household Size"
  ) +
  theme_minimal(base_size = 14)+                       
  theme(
    plot.title = element_text(hjust = 0.5),             
    panel.grid.major = element_line(color = "grey80")   
  )


# Log(Total) Vs Age by Region
log_means_by_region <- db_ph %>%
  group_by(Region, Age) %>% 
  summarise(
    Mean_Total = mean(Total, na.rm = TRUE), 
    Log_Mean_Total = log(Mean_Total),      
    .groups = "drop"
  ) %>%
  filter(!is.na(Log_Mean_Total))            

# Plot
ggplot(log_means_by_region, aes(x = Age, y = Log_Mean_Total, color = Region, shape = Region)) +
  geom_point(alpha = 0.8, size = 3) + 
  geom_smooth(method = "loess", se = FALSE) + 
  labs(
    title = "Empirical log of the mean household sizes by age and region",
    x = "Age of head of the household",
    y = "Log of the empirical mean household size",
    color = "Region"
  ) +
  theme_minimal(base_size = 14) + # Tema minimale
  theme(
    plot.title = element_text(hjust = 0.5),             
    legend.position = "right",                         
    panel.grid.major = element_line(color = "grey80") 
  )

################################################################################

# Poisson Regression Model: Total ~ Age
model_poisson <- glm(Total ~ Age, family = poisson(link = "log"), data = db_ph)
summary(model_poisson)

# Adding quadratic term
db_ph <- db_ph %>%
  mutate(Age2 = Age^2)

model_poisson_quad <- glm(Total ~ Age + Age2, family = poisson(link = "log"), data = db_ph)
summary(model_poisson_quad)

# Comparison
anova(model_poisson, model_poisson_quad, test = "Chisq")

# Adding covariate: Region
model_poisson_region <- glm(Total ~ Age + Age2 + Region, family = poisson(link = "log"), data = db_ph)
summary(model_poisson_region)

# Comparison
anova(model_poisson_quad, model_poisson_region, test = "Chisq")

# Goodness of Fit
confint(model_poisson_region)
exp(confint(model_poisson_region))

# Plotting relation: Total Vs Age
db_ph$fitted_values <- predict(model_poisson, type = "response") # Predicted
db_ph$deviance_residuals <- residuals(model_poisson, type = "deviance") # Residuals

ggplot(db_ph, aes(x = fitted_values, y = deviance_residuals)) +
  geom_point(color = "black", alpha = 0.6) +
  geom_smooth(method = "loess", color = "blue", fill = "orange", se = TRUE, linetype = "dashed") + #  LOESS
  geom_hline(yintercept = 0, color = "red", linetype = "solid", linewidth = 1) + # y=0
  theme_minimal(base_size = 14) + 
  labs(
    title = "Deviance Residuals vs Fitted Values",
    x = "Fitted values",
    y = "Deviance Residuals"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5), # Title
    panel.grid.major = element_line(color = "grey80") # Grid
  )

# Save step for future improvement
saveRDS(model_poisson_region, "poisson_model.rds")


# Advanced Model
# Adding Interactions
adv_model <- glm(Total ~ Age + Age2 + Roof + Region + Roof:Region, family = poisson(link = "log"), data = db_ph)
# Overdispersed
quasi_model <- glm(Total ~ Age + Age2, family = quasipoisson(link = "log"), data = db_ph)
summary(quasi_model)
# Fit a negative binomial model
nb_model <- glm.nb(Total ~ Age + Roof, data = db_ph)
# Zero Inflated
zip_model <- zeroinfl(Total ~ Age + Roof | gender, data = db_ph, dist = "poisson")


