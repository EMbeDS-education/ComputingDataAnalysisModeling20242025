# *************************************************************
# ---------------- MULTILEVEL MODELS -------------------------
# *************************************************************

# Load necessary libraries
library(nlme)       # For mixed-effects models
library(ggplot2)    # For plots
library(dplyr)      # For data manipulation
library(corrr)      # For correlation matrix
library(readr)      # For data import
library(tidyr)      # For reshaping data
library(ggcorrplot)
library(ggplot2)

# *************************************************************
# ---------------- LOAD THE DATASET -------------------------
# *************************************************************

# Import the file 'nlschools.csv'
nlschools <- read.csv("/Users/claudiomazzi/Documents/PhD/Course_AppSTAT/3_Poisson_RandomEff/Multilevel/nlschools.csv")

# Add labels to the variables
colnames(nlschools) <- c("id", "lang", "IQ", "class", "CS", "SES", "COMB")
labels <- c(
  "lang" = "Language test score",
  "IQ" = "Verbal IQ",
  "class" = "Class ID",
  "CS" = "Class size",
  "SES" = "Family socio-economic status",
  "COMB" = "Dummy of multi-grade class"
)

# *************************************************************
# ------------------ PRELIMINARY ANALYSIS -------------------
# *************************************************************

# Display the first 10 rows of the dataset
head(nlschools, 10)

# Frequency distribution of 'class'
class_distribution <- nlschools %>%
  count(class) %>%
  arrange(desc(n))
print(class_distribution)

# Descriptive statistics
summary(nlschools)

# Correlation matrix
nls_corr <- cor(nlschools, method = "kendall")
ggcorrplot(nls_corr, type = "lower", lab = TRUE, lab_size = 3)

# *************************************************************
# -------------------- BOXPLOT ------------------------------
# *************************************************************

# Boxplot of 'lang' by 'class'
filtered_data <- nlschools %>%
  filter(as.numeric(class) %% 15 == 0) # binning

ggplot(filtered_data, aes(x = factor(class), y = lang, fill = factor(class))) +
  geom_boxplot(outlier.shape = 21, outlier.color = "red", alpha = 0.7) + # Outlier
  scale_fill_viridis_d(option = "C") + # Palette
  labs(
    title = "Boxplots of Language Test Scores by Class (Filtered)",
    x = "Class (Filtered)", y = "Language Test Score"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # 
    legend.position = "none" # 
  )

# *************************************************************
# --------- MULTILEVEL MODELS: USING LME4 PACKAGE -----------
# *************************************************************

# Empty model (1-way ANOVA random effects)
empty_model <- lme(lang ~ 1, random = ~ 1 | class, data = nlschools, method = "REML")
summary(empty_model)

# Random-intercept model with IQ
random_intercept_model <- lme(
  fixed = lang ~ IQ,                   # Fixed effects
  random = ~ 1 | class,                # Random intercept for 'class'
  data = nlschools,                    # Dataset
  method = "REML"                      # Restricted Maximum Likelihood
)
summary(random_intercept_model)

# Random-coefficient model with IQ and SES
# Random-coefficients model using nlme
random_coeff_model <- lme(
  fixed = lang ~ IQ + SES,          # Fixed effects
  random = ~ IQ | class,            # Random intercept and slope for 'IQ' by 'class'
  data = nlschools,                 # Dataset
  method = "REML"                   # Restricted Maximum Likelihood
)

summary(random_coeff_model)

# *************************************************************
# --------- PREDICTION AND RANDOM EFFECTS ANALYSIS ----------
# *************************************************************

# Extract random effects
random_effects <- ranef(random_coeff_model)
random_effects_df <- as.data.frame(random_effects)
colnames(random_effects_df) <- c("Intercept", "IQ")
random_effects_df$class <- rownames(random_effects)

# Dev Std
var_corr <- VarCorr(random_coeff_model)
sd_intercept <- sqrt(as.numeric(var_corr["(Intercept)", "Variance"]))
random_effects_df$sd.Intercept <- sd_intercept


# High-Low plot for random intercepts
random_effects_df <- random_effects_df %>%
  arrange(Intercept)
random_effects_df$id <- seq_along(random_effects_df$Intercept)

filtered_random_effects <- random_effects_df %>%
  filter(row_number() %% 5 == 0) 

ggplot(filtered_random_effects, aes(x = id, y = Intercept, color = Intercept)) +
  geom_errorbar(aes(ymin = Intercept - sd.Intercept, ymax = Intercept + sd.Intercept), width = 0.5, alpha = 0.7) +
  geom_point(size = 3, alpha = 0.8) +
  scale_color_viridis_c(option = "C") + # Palette 
  labs(
    title = "Random Effects Predictions (Filtered)",
    x = "Classes (Filtered)",
    y = "Random Intercepts"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(), # Label x-axis
    axis.ticks.x = element_blank(), # No tick x-axis
    legend.position = "right" # Legenda
  )

# *************************************************************
# -------- CROSS-LEVEL INTERACTION (IQ * CS) ---------------
# *************************************************************

cross_level_model <- lme(
  fixed = lang ~ IQ + SES + CS + IQ:CS, # Effetti fissi
  random = ~ IQ | class,                # Effetti casuali (intercetta e coefficiente di IQ)
  data = nlschools,                     # Dataset
  method = "REML"                       # Restricted Maximum Likelihood
)
summary(cross_level_model)

# *************************************************************
# ---------------- COMPARISON OF MODELS ----------------------
# *************************************************************

# Compare empty model and random-coefficient model
# Fixed Effects must be the same if using REML (differently, use ML)
compare_model <- anova(random_intercept_model, random_coeff_model)
print(compare_model)
