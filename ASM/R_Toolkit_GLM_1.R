# Step 1: Install and Load Required Packages
install.packages("nnet")
install.packages("dplyr")
install.packages(c("tidyverse", "GGally", "MASS", "pscl"))
install.packages("smotefamily")

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


# Step 2: Create a simulated dataset
set.seed(123)
X <- rnorm(100, mean = 5, sd = 2)

# Outcome as a function of X
prob <- 1 / (1 + exp(-0.5 * (X - 5)))  
Outcome <- rbinom(100, 1, prob = prob)
data_binary <- data.frame(Outcome, X)

# Display the first few rows of the dataset
head(data_binary)
 
# Step 3: Linear Regression vs Logistic Regression 
# Linear regression model
linear_model <- lm(as.numeric(Outcome) ~ X, data = data_binary)
summary(linear_model)

# Plot the linear regression fit
ggplot(data_binary, aes(x = X, y = as.numeric(Outcome))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(y = "Outcome")

################################################################################

# Step 3: Logistic Regression 
# Logistic regression model (logit)
logistic_model <- glm(Outcome ~ X, data = data_binary, family = binomial(link = "logit"))
summary(logistic_model)

# Plotting logistic regression
X_seq <- seq(min(data_binary$X), max(data_binary$X), length.out = 100)
predicted_probs <- predict(logistic_model, newdata = data.frame(X = X_seq), type = "response")

plot(data_binary$X, data_binary$Outcome, pch = 16, col = "blue",
     xlab = "X", ylab = "Probability of Outcome = 1")
lines(X_seq, predicted_probs, col = "red", lwd = 2)
legend("right", legend = c("Observed Data", "Predicted Probability"),
       col = c("blue", "red"), pch = c(16, NA), lty = c(NA, 1), lwd = c(NA, 2))

################################################################################

# Step 4: Logit and Probit Model on heartdisease db
# Load the dataset
data_heart <- read.csv("/Users/claudiomazzi/Documents/PhD/Course_AppSTAT/2_GLM/db/heart.csv")
head(data_heart)
str(data_heart)
unique(data_heart$ST_Slope)

# Preprocess the data
# Encode 'Sex' and 'ExerciseAngina' as binary variables
data_heart$Sex <- ifelse(data_heart$Sex == "M", 1, 0)
data_heart$ExerciseAngina <- ifelse(data_heart$ExerciseAngina == "Y", 1, 0)

# One-hot encode categorical variables for 'ChestPainType', 'RestingECG', and 'ST_Slope'
data_heart <- cbind(data_heart, model.matrix(~ ChestPainType + RestingECG + ST_Slope - 1, 
                                             data = data_heart))
# Drop original factor columns
data_heart$ChestPainType <- NULL
data_heart$RestingECG <- NULL
data_heart$ST_Slope <- NULL

head(data_heart)

# Logistic regression model (logit)
logit_model <- glm(HeartDisease ~ Age + Sex + RestingBP + Cholesterol + FastingBS + 
                     MaxHR + ExerciseAngina + Oldpeak + ChestPainTypeATA + 
                     ChestPainTypeNAP + RestingECGST + ST_SlopeFlat, 
                   family = binomial(link = "logit"), 
                   control = glm.control(trace = TRUE),
                   data = data_heart)

# Probit regression model
probit_model <- glm(HeartDisease ~ Age + Sex + RestingBP + Cholesterol + FastingBS + 
                      MaxHR + ExerciseAngina + Oldpeak + ChestPainTypeATA + 
                      ChestPainTypeNAP + ChestPainTypeTA + RestingECGST + 
                      RestingECGNormal + ST_SlopeFlat + ST_SlopeUp, 
                    family = binomial(link = "probit"), data = data_heart)

# Summaries of both models
summary(logit_model)
summary(probit_model)

# Calculating pseudo-R2 values for both models
# Pseudo-R2 for logit model
logit_pseudo_r2 <- pR2(logit_model)["McFadden"]
print(logit_pseudo_r2)

# Pseudo-R2 for probit model
probit_pseudo_r2 <- pR2(probit_model)["McFadden"]
print(probit_pseudo_r2)

# Predictions and confusion matrix
pred_logit <- ifelse(predict(logit_model, type = "response") > 0.5, 1, 0)
pred_probit <- ifelse(predict(probit_model, type = "response") > 0.5, 1, 0)

cm_logit <- table(Predicted = pred_logit, Actual = data_heart$HeartDisease)
cm_logit
cm_probit <- table(Predicted = pred_probit, Actual = data_heart$HeartDisease)
cm_probit

# ROC curve and AUC
roc_logit <- roc(data_heart$HeartDisease, predict(logit_model, type = "response"))
plot(roc_logit)
auc_logit <- auc(roc_logit)
# Add the AUC value as text on the plot
text(0.6, 0.2, labels = paste("AUC =", round(auc_logit, 6)), col = "blue", cex = 1.2)

roc_probit <- roc(data_heart$HeartDisease, predict(probit_model, type = "response"))
plot(roc_probit)
auc_probit <- auc(roc_probit)
# Add the AUC value as text on the plot
text(0.6, 0.2, labels = paste("AUC =", round(auc_probit, 6)), col = "blue", cex = 1.2)

################################################################################

# Step 5: Ordinal Logit on wine_quality database
# Load the dataset
df_wne <- read.csv("/Users/claudiomazzi/Documents/PhD/Course_AppSTAT/2_GLM/db/wine_quality.csv")
df_wne <- df_wne %>%
  mutate(quality = case_when(
    quality <= 5 ~ 3,
    quality == 6 ~ 2,
    quality > 6 ~ 1
  ))

# Explore the dataset
head(df_wne)
str(df_wne)

# Distribution of the quality variable
ggplot(df_wne, aes(x = quality)) + geom_bar(fill = "blue") + theme_minimal() +
  labs(title = "Distribution of Wine Quality")

# Correlation matrix (optional, for exploration purposes)
df_wne_numeric <- dplyr::select_if(df_wne, is.numeric)
df_corr <- cor(df_wne_numeric, method = "kendall")
ggcorrplot(df_corr, type = "lower", lab = TRUE, lab_size = 3)

# Generate pairplot colored by 'quality'
df_no_quality <- dplyr::select(df_wne, -quality)
quality_levels <- sort(unique(df_wne$quality))
quality_colors <- scales::hue_pal()(length(quality_levels))

custom_cor <- function(data, mapping, ...){
  ggally_cor(data, mapping, ..., size = 3, digits = 2, method = "kendall") +
    theme(text = element_text(size = 8))
}

ggpairs(df_no_quality, 
        aes(color = as.factor(df_wne$quality), alpha = 0.6),
        upper = list(continuous = custom_cor), 
        lower = list(continuous = wrap("points", alpha = 0.6)),
        diag = list(continuous = wrap("densityDiag", alpha = 0.6))) + 
  scale_color_manual(values = quality_colors, name = "Quality") + 
  scale_fill_manual(values = quality_colors, name = "Quality") +
  theme_minimal() +
  labs(title = "Pairplot of Wine Quality Data with Correlations") +
  theme(legend.position = "right")

# Ordinal logistic regression
df_wne$quality <- factor(df_wne$quality, levels = c(1, 2, 3), ordered = TRUE)
df_wne[, -which(names(df_wne) == "quality")] <- scale(df_wne[, -which(names(df_wne) == "quality")])
head(df_wne)
ordinal_model <- polr(as.factor(quality) ~ ., data = df_wne, Hess = TRUE)

# Display model summary
summary(ordinal_model)

# 1. Log-Likelihood and Likelihood Ratio Test
null_model <- polr(as.factor(quality) ~ 1, data = df_wne, Hess = TRUE)
likelihood_ratio_test <- anova(null_model, ordinal_model)
cat("Likelihood Ratio Test:")
print(likelihood_ratio_test)

# 2. BIC
bic_value <- BIC(ordinal_model)
cat("BIC:", bic_value, "")

# 4. Compute Accuracy, deviance and the confusion matrix
# Predicts the categories with the highest probability
pred_classes <- predict(ordinal_model, type = "class")
confusion_table <- table(pred_classes, df_wne$quality)
accuracy <- sum(diag(confusion_table)) / sum(confusion_table)

cat("Accuracy (Misclassification Rate):", accuracy, "")
cat("Confusion Matrix:")
print(confusion_table)


################################################################################

# Step 6: Multinomial Logit multinomial_logit_nation.csv
# Load dataset
df_multi <- read.csv("/Users/claudiomazzi/Documents/PhD/Course_AppSTAT/2_GLM/db/multinomial_logit_nation.csv") 
head(df_multi)
unique(df_multi$continent)
dim(df_multi)


# Put continent variable as factor
df_multi$continent <- as.factor(df_multi$continent)


# Multinomial logit execution
model_multi <- multinom(continent ~ life_expectancy + gdp_per_capita + 
                    health_expenditure + education_index, data = df_multi)
# Summary of results
summary(model_multi)

# Coefficients and standard errors
coefficients <- summary(model_multi)$coefficients
std_errors <- summary(model_multi)$standard.errors
# Z-values
z_values <- coefficients / std_errors
# p-value
p_values <- 2 * (1 - pnorm(abs(z_values)))
# Print p-value
print(p_values)

# Analysis of coefficients - computing odds ratio
exp(coef(model_multi))
# Predicted probabilities
predicted_probs <- predict(model_multi, type = "probs")
head(predicted_probs)

# Adding probablities to database
df_multi$predicted_continent <- colnames(predicted_probs)[apply(predicted_probs, 1, which.max)]

# Observed frequencies
observed_counts <- table(df_multi$continent)
observed_df <- as.data.frame(observed_counts)
colnames(observed_df) <- c("continent", "observed_count")

# Predicted frequencies
predicted_counts <- table(df_multi$predicted_continent)
predicted_df <- as.data.frame(predicted_counts)
colnames(predicted_df) <- c("continent", "predicted_count")

# Combine observed and predicted data
combined_df <- merge(observed_df, predicted_df, by = "continent")

# Plottin results
combined_long <- tidyr::pivot_longer(combined_df, 
                                     cols = c("observed_count", "predicted_count"),
                                     names_to = "Type",
                                     values_to = "Count")
combined_long$Type <- recode(combined_long$Type, "observed_count" = "Observed", "predicted_count" = "Predicted")

# Histogram with side-by-sed bars
ggplot(combined_long, aes(x = continent, y = Count, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.8) +
  labs(x = "Continent", y = "Count", title = "") +
  scale_fill_manual(name = "Legend", values = c("Observed" = "blue", "Predicted" = "red")) +
  theme_minimal()


################################################################################
# Definiamo la funzione
y_function <- function(x) {
  1 + 2 * x
}

# Generiamo un intervallo per x
x_values <- seq(-10, 10, by = 0.1)

# Calcoliamo i valori di y
y_values <- y_function(x_values)

# Creiamo il plot
plot(x_values, y_values, type = "l", col = "blue", lwd = 2,
     main = "Grafico della funzione y = 1 + 2x",
     xlab = "x", ylab = "y")
grid()  # Aggiunge una griglia per una migliore leggibilità

# Regressione logistica con tracing delle iterazioni
logit_model <- glm(y ~ x, family = binomial(link = "logit"),
                   control = glm.control(trace = TRUE))

