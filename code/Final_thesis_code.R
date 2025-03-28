# Load the haven package
library(haven)
library(dplyr)
library(survival)      # For survival analysis
library(survminer)     # For plotting Kaplan-Meier curves
library(ggsci)
library(writexl)
library(readxl)


# Define the file path
file_path <- "C:/Users/HP/Documents/HENRY/STRATHMORE/project/important datasets/new_data.xlsx"

# Read the Excel file
data7_20ch <- read_excel(file_path)

# View the imported data
View(data7_20ch)

###selecting variables

###select the 2 variables for mosquito net
new_data <- data7_20ch %>% select(ML0,V002, V459,S502AB,SZONE,S114,V171A,S502H,B8,S413A,S413B,S413C,S506,H22,HW57,B3,V008)
View(new_data)

####RENAME B8 TO child_age
new_data <- new_data %>%
  rename(child_age = B8)

new_data <- new_data %>%
  rename(hold = V002)

###ITN USE 
##recode the data to 1 for itn use and 0 no itn use
new_data <- new_data %>%
  mutate(
    ML0 = as.character(ML0),  # Ensure it's a character
    trtdnet = ifelse(trimws(ML0) == "1", 1, 0)  # Remove extra spaces before checking
  )

##give labels to the trtdnet(itn) variable
new_data <- new_data %>%
  mutate(
    ML0 = as.character(ML0),  # Ensure it's a character
    trtdnet_std = ifelse(trimws(ML0) == "1", 1, 0),  # Create flag variable
    trtdnet = factor(trtdnet_std, labels = c("No/Untreated Net", "Treated Net"))  # Assign labels
  )
View(new_data)


####Recoding ENDEMIC/NON-ENDEMIC


new_data$endemic <- ifelse(trimws(new_data$SZONE) == "1", "Non_endemic",
                           ifelse(trimws(new_data$SZONE) %in% c("2", "3", "4"), "Endemic",
                                  ifelse(trimws(new_data$SZONE) == "5", "Non_endemic", NA)))

new_data$endemic_std <- ifelse(new_data$endemic == "Endemic", 1, 0)

View(new_data)



###DIGITAL CONNECTIVITY
##give labels to the SMART PHONE variable
new_data <- new_data %>%
  mutate(
    S114 = as.character(S114),  # Ensure it's a character
    sm_phone_std = ifelse(trimws(S114) == "1", 1, 0),  # Create flag variable
    sm_phone = factor(sm_phone_std, labels = c("no_smart_phone", "has_smart_phone"))  # Assign labels
  )
View(new_data)


###INTERNET USE
new_data$int_use <- ifelse(trimws(new_data$V171A) == "0", "no_internet_use",
                            ifelse(trimws(new_data$V171A) %in% c("1", "2"), "internet_use",
                                   ifelse(trimws(new_data$V171A) == "3", "internet_use", NA)))

new_data$int_use_std <- ifelse(new_data$int_use == "internet_use", 1, 0)
View(new_data)

###SOCIAL MEDIA USAGE ON ITN MESSAGES

new_data <- new_data %>%
  mutate(
    S502H = as.character(S502H),
    S502AB = as.character(S502AB)# Ensure it's a character  # Assign labels
  )

###FILTERING TO SEE WHERE messages were seen on social media in regards to itn use
data_filtered <- subset(new_data, S502H == "1")
View(data_filtered)

new_data$soc_media <- ifelse(
  new_data$S502H == "1" & new_data$S502AB =="1",  # Condition
  "Usage",                          # Value if TRUE
  "No_usage"                            # Value if FALSE
)
View(new_data)

new_data$soc_media_std <- ifelse(new_data$soc_media == "Usage", 1, 0)
View(new_data)


###CHILD SYMPTOMS


new_data <- new_data %>%
  mutate(
    S506 = as.character(S506),
    H22 = as.character(H22),
    S413A = as.character(S413A),
    S413B = as.character(S413B),
    S413C = as.character(S413C),
    HW57 = as.character(HW57)# Ensure it's a character  # Assign labels
  )

###FILTERING TO SEE WHERE messages were seen on social media in regards to itn use
data_filteredc <- subset(new_data, S413A == "1")
View(data_filteredc)
nrow(data_filteredc)


####ACT_ADHERENCE
new_data$act_adherence <- ifelse(
  new_data$S413A == "1" | new_data$S413B == "1" | new_data$S413C == "1",
  "Received_ACT_Trtmt",    # Value if ANY of the conditions are TRUE
  "did_not_Receive_ACT_Trtmt"  # Value if ALL conditions are FALSE
)

new_data$act_adherence_std <- ifelse(new_data$act_adherence == "Received_ACT_Trtmt", 1, 0)
View(new_data)

###FILTERING TO SEE WHERE messages were seen on ACT ADHERENCE
data_filteredc <- subset(new_data5, S413C == "1")
View(data_filteredc)

###MALARIA SYMPTOMS

new_data$mal_symptoms <- ifelse(trimws(new_data$S506) == "1", "mild",
                                 ifelse(trimws(new_data$H22) == "1", "mild",
                                        ifelse(trimws(new_data$HW57) %in% c("1", "2","3"), "severe","no_symptoms"
                                        )))


new_data$mal_symptoms_std <- case_when(
  new_data$mal_symptoms == "severe"   ~ 2,  # Highest frequency
  new_data$mal_symptoms == "mild"  ~ 1,
  new_data$mal_symptoms == "no_symptoms"  ~ 0   # Lowest frequency
)
View(new_data)

####creating time to prevention ie time to itn usage
new_data$time_to_itnusage <- floor((new_data$V008-new_data$B3)/12)
View(new_data)
##SELECT NEWLY CREATED VARIABLES

new_data1 <- new_data %>% select(hold,trtdnet,trtdnet_std,endemic,endemic_std,sm_phone_std,sm_phone,
                                 int_use,int_use_std,soc_media,soc_media_std,child_age,
                                 act_adherence,act_adherence_std,mal_symptoms,mal_symptoms_std,
                                 time_to_itnusage)
View(new_data1)
library(writexl)
write_xlsx(new_data1, "C:/Users/HP/Documents/HENRY/STRATHMORE/project/important r codes for analyisis/final_data.xlsx")

dat2 <- nrow(na.omit(new_data1))
View(dat2)

nrow(new_data1)


data_filteredc <- subset(new_data1, soc_media_std == "1")
View(data_filteredc)




####handling missing data
library(naniar)
library(dplyr)  
library(ggplot2)  # Required for labs(), theme(), etc.

new_data1a <- new_data1 %>% select(hold,trtdnet,endemic,sm_phone,
                                 int_use,soc_media,child_age,
                                 act_adherence,mal_symptoms,
                                 time_to_itnusage)

# Get missingness summary for each variable
miss_var_summary(new_data1a)

###get total missing values of the dataset
n_miss(new_data1a)   # Total missing values: 7801
pct_miss(new_data1a) # Percentage missing: 13.45%


###get total complete  values of the dataset
n_complete(new_data1a)   # Total non-missing values: 50215
pct_complete(new_data1a) # Percentage complete: 86.55%

##visualize missingness per variable


# Create missingness plot with title
gg_miss_var(new_data1a) +
  labs(
    title = "Missing Values by Variable",
    x = "Variables",
    y = "Number of Missing Values"
  ) +
  theme_minimal()  # Optional: Add ggplot2 themes



new_data2 <- new_data1 %>% select(trtdnet_std,sm_phone_std,child_age,
                                 act_adherence_std,mal_symptoms_std)
View(new_data2)

###test for missingness whether it is MCAR 
mcar_test_result <- mcar_test(new_data2)
print(mcar_test_result)

#The p-value is < 0.05 (statistically significant), 
#so we reject the null hypothesis that the data is not Missing Completely at Random (MCAR).
#The large chi-squared statistic confirms significant deviations from MCAR.
#There are 11 distinct patterns of missingness in your dataset

##MAR test

glm(is.na(sm_phone_std) ~ child_age + trtdnet_std+act_adherence_std+mal_symptoms_std,
    data = new_data2, family = binomial) %>% summary()

glm(is.na(trtdnet_std) ~ child_age + sm_phone_std+act_adherence_std+mal_symptoms_std,
    data = new_data2, family = binomial) %>% summary()

glm(is.na(act_adherence_std) ~ child_age + sm_phone_std+trtdnet_std+mal_symptoms_std,
    data = new_data2, family = binomial) %>% summary()



#####multiple imputation done
library(mice)
# Perform multiple imputation
imputed_data <- mice(new_data1, m = 5, method = "pmm", seed = 123)

# Inspect the imputed data
summary(imputed_data)

# Extract the first completed dataset
completed_data <- complete(imputed_data, 1)

# View the imputed values
head(completed_data)
View(completed_data)
write_xlsx(completed_data, "C:/Users/HP/Documents/HENRY/STRATHMORE/project/important r codes for analyisis/complete_impute_data.xlsx")

# Compare original and imputed data
par(mfrow = c(1, 2))

# Original data (with NAs removed)
hist(new_data1$mal_symptoms_std[!is.na(new_data1$mal_symptoms_std)], 
     main = "Original Data", 
     xlab = "mal_symptoms_std", 
     col = "lightblue")

# Imputed data
hist(completed_data$mal_symptoms_std, 
     main = "Imputed Data", 
     xlab = "mal_symptoms_std", 
     col = "lightgreen")



# Example: Frequency table for a numeric variable
##numbers <- c(1, 2, 2, 3, 3, 3, 4, 4, 4, 4)
freq_table <- table(new_data1$act_adherence_std)

# Display the frequency table
print(freq_table)


freq_tablea <- table(completed_data$act_adherence_std)

# Display the frequency table
print(freq_tablea)




num_nas_age <- sum(is.na(new_data1$act_adherence_std))

# Print the result
print(num_nas_age)

num_nas_agea <- sum(is.na(completed_data$act_adherence_std))

# Print the result
print(num_nas_agea)
###start model


new_data3 <- completed_data
View
(new_data3)

new_data3 <- new_data3 %>% filter(!is.na(time_to_itnusage))
View(new_data3)

is.numeric(new_data3$hold)
# Convert to factor
new_data3$V002 <- as.factor(new_data3$hold)
is.factor(new_data3$hold)

colSums(is.na(new_data3))
length(unique(new_data3$hold))

coxph(Surv(time_to_itnusage, trtdnet_std) ~ endemic, data = new_data3)

new_data3_cleaned <- new_data3[!is.na(new_data3$trtdnet_std), ]

new_data2trial <- new_data3_cleaned %>% select(trtdnet_std,time_to_itnusage,endemic_std,
                                               hold)
View(new_data2trial)

# Example: Create a frequency table for a variable
freq_table <- table(new_data2trial$trtdnet_std, useNA = "ifany")

# Print the frequency table
print(freq_table)

freq_table <- new_data2trial %>%
  count(trtdnet_std)

# Print the frequency table
print(freq_table)



####DESCRIPTIVE STATISITICS

# Load necessary libraries
library(dplyr)

# Example: Load your dataset (replace this with your actual data loading code)
# malaria_data <- read.csv("path_to_your_data.csv")

# Summarize the data endemic vs itn use
summary_table <- new_data3_cleaned %>%
  group_by(endemic) %>%
  summarise(
    Households_n = n(),  # Count the number of households
    ITN_Use_Percent = mean(trtdnet_std, na.rm = TRUE) * 100  # Calculate percentage of ITN use
  )

# Rename columns for better readability
colnames(summary_table) <- c("Endemic Zone", "Households (n)", "ITN Use (%)")

# Display the table
print(summary_table)


##chisquare
# Load necessary libraries

# Ensuring the data is in the correct format
# Assuming:
# - endemic_zone: A categorical variable with levels "Endemic" and "Non-Endemic"
# - itn_use: A binary variable with levels 1 (Yes) and 0 (No)

# Create a contingency table
contingency_table <- table(new_data3_cleaned$endemic, new_data3_cleaned$trtdnet_std)

# Add meaningful row and column names
rownames(contingency_table) <- c("Non-Endemic", "Endemic")
colnames(contingency_table) <- c("No ITN Use", "ITN Use")

# Print the contingency table
print(contingency_table)

# Perform the chi-square test
chi_square_test <- chisq.test(contingency_table)

# Print the test results
print(chi_square_test)

# Extract and format the results for reporting
chi_square_statistic <- chi_square_test$statistic
p_value <- chi_square_test$p.value

# Print the formatted result
cat("A chi-square test showed a significant association between ITN usage and endemicity (χ² =", 
    round(chi_square_statistic, 1), ", p <", ifelse(p_value < 0.001, "0.001", round(p_value, 3)), 
    "), indicating that households in endemic zones are more likely to use treated nets.\n")

##digital connectivity vs itn usage

# Function to calculate ITN use percentage and perform chi-square test
analyze_digital_connectivity <- function(data, variable) {
  # Calculate ITN use percentage
  itn_percentage <- data %>%
    group_by(!!sym(variable)) %>%
    summarise(ITN_Use_Percent = mean(trtdnet_std, na.rm = TRUE) * 100)
  
  # Create a contingency table for chi-square test
  contingency_table <- table(data[[variable]], data$trtdnet_std)
  
  # Perform chi-square test
  chi_square_test <- chisq.test(contingency_table)
  
  # Return results
  list(
    ITN_Use_Percent = itn_percentage,
    Chi_Square_p_value = chi_square_test$p.value
  )
}

# Analyze each digital connectivity variable
smartphone_results <- analyze_digital_connectivity(new_data3_cleaned, "sm_phone")
internet_results <- analyze_digital_connectivity(new_data3_cleaned, "int_use")
social_media_results <- analyze_digital_connectivity(new_data3_cleaned, "soc_media")

# Combine results into a table
results_table <- data.frame(
  Variable = c("sm_phone", "int_use", "soc_media"),
  ITN_Use_Percent = c(
    paste0(round(smartphone_results$ITN_Use_Percent$ITN_Use_Percent[2], 1), "% vs ", 
           round(smartphone_results$ITN_Use_Percent$ITN_Use_Percent[1], 1), "%"),
    paste0(round(internet_results$ITN_Use_Percent$ITN_Use_Percent[2], 1), "% vs ", 
           round(internet_results$ITN_Use_Percent$ITN_Use_Percent[1], 1), "%"),
    paste0(round(social_media_results$ITN_Use_Percent$ITN_Use_Percent[2], 1), "% vs ", 
           round(social_media_results$ITN_Use_Percent$ITN_Use_Percent[1], 1), "%")
  ),
  Chi_Square_p_value = c(
    smartphone_results$Chi_Square_p_value,
    internet_results$Chi_Square_p_value,
    social_media_results$Chi_Square_p_value
  )
)

# Print the results table
print(results_table)


###kaplan meir curves time to itn usage vs child factors


# Ensure the data is in the correct format
# Assuming:
# - time_to_itnusage: Time to ITN usage (numeric)
# - itn_usage_event: Event indicator (1 = ITN used, 0 = censored)
# - malaria_symptoms: Binary variable (e.g., "Yes" or "No")
# - act_adherence: Binary variable (e.g., "Adherent" or "Non-Adherent")

# Fit Kaplan-Meier curves for malaria symptoms
km_malaria <- survfit(Surv(time_to_itnusage, trtdnet_std) ~ mal_symptoms, data = new_data3_cleaned)

# Fit Kaplan-Meier curves for ACT adherence
km_act <- survfit(Surv(time_to_itnusage, trtdnet_std) ~ act_adherence, data = new_data3_cleaned)

# Plot Kaplan-Meier curves for malaria symptoms
ggsurvplot(
  km_malaria,
  data = new_data3_cleaned,
  pval = TRUE,              # Add p-value to the plot
  pval.method = TRUE,       # Add method for p-value calculation
  risk.table = TRUE,        # Add risk table below the plot
  title = "Kaplan-Meier Curve for Time to ITN Usage by Malaria Symptoms",
  xlab = "Time (Days)",     # Label for the x-axis
  ylab = "Survival Probability", # Label for the y-axis
  legend.title = "Malaria Symptoms",
  legend.labs = c("No_Symptoms", "mild" , "severe"), # Custom legend labels
  palette = "jco"           # Color palette
)

# Plot Kaplan-Meier curves for ACT adherence
ggsurvplot(
  km_act,
  data = new_data3_cleaned,
  pval = TRUE,              # Add p-value to the plot
  pval.method = TRUE,       # Add method for p-value calculation
  risk.table = TRUE,        # Add risk table below the plot
  title = "Kaplan-Meier Curve for Time to ITN Usage by ACT Adherence",
  xlab = "Time (Days)",     # Label for the x-axis
  ylab = "Survival Probability", # Label for the y-axis
  legend.title = "ACT Adherence",
  legend.labs = c("Non-Adherent", "Adherent"), # Custom legend labels
  palette = "npg"           # Color palette
)


###END

##objective1

# Fit the Cox model
cox_model <- coxph(Surv(time_to_itnusage, trtdnet_std) ~ endemic_std + cluster(hold),
                   data = new_data2trial)
# Summary of the Cox model
summary(cox_model)
# Extract the log-likelihood
loglik <- logLik(cox_model)
loglik

# Number of parameters (df)
df <- attr(loglik, "df")

# Number of observations
n <- cox_model$n

# Calculate AIC
aic <- -2 * as.numeric(loglik) + 2 * df

# Calculate BIC
bic <- -2 * as.numeric(loglik) + log(n) * df

# Print results
cat("Log-Likelihood:", as.numeric(loglik), "\n")
cat("AIC:", aic, "\n")
cat("BIC:", bic, "\n")

library(parfm)  
#: dist = "gompertz"
model_ig <- parfm(Surv(time_to_itnusage, trtdnet_std) ~ endemic_std,
                  cluster = "hold",
                  data = new_data2trial,
                  dist = "gompertz",   # Baseline hazard: exponential, weibull, gompertz, loglogistic, lognormal
                  frailty = "ingau")     # Inverse Gaussian frailty
print(summary(model_ig))
summary(model_ig)

coef(model_ig)




str(model_ig)

coef_matrix <- model_ig

# Identify the relevant covariates (age, mobile_access)
covariates <- c("endemic_std")

# Extract estimates, standard errors, and p-values for covariates
results <- data.frame(
  Estimate = round(coef_matrix[covariates, "ESTIMATE"], 3),
  SE = round(coef_matrix[covariates, "SE"], 3),
  `z-value` = round(coef_matrix[covariates, "ESTIMATE"] / coef_matrix[covariates, "SE"], 2),
  `p-value` = signif(coef_matrix[covariates, "p-val"], 4)
)

# Extract frailty variance (theta), log-likelihood, AIC, and BIC
frailty_variance <- round(coef_matrix["theta", "ESTIMATE"], 3)
log_likelihood <- round(attr(model_ig, "loglik"), 2)
AIC_val <- round(-2 * log_likelihood + 2 * length(covariates), 2)  # AIC formula: -2LL + 2k
BIC_val <- round(-2 * log_likelihood + log(attr(model_ig, "nobs")) * length(covariates), 2)  # BIC formula

# Display the results
cat("Coefficients:\n")
print(results)
cat("\nFrailty Variance (θ):", frailty_variance, "\n")
cat("Log-likelihood:", log_likelihood, "\n")
cat("AIC:", AIC_val, "\n")
cat("BIC:", BIC_val, "\n")



###end model

#: dist = "gompertz" frailty=gamma
model_gam <- parfm(Surv(time_to_itnusage, trtdnet_std) ~ endemic_std,
                  cluster = "hold",
                  data = new_data2trial,
                  dist = "gompertz",   # Baseline hazard: exponential, weibull, gompertz, loglogistic, lognormal
                  frailty = "gamma")     # Inverse Gaussian frailty
print(summary(model_gam))

coef(model_gam)



str(model_gam)

coef_matrix <- model_gam

# Identify the relevant covariates (age, mobile_access)
covariates <- c("endemic_std")

# Extract estimates, standard errors, and p-values for covariates
results <- data.frame(
  Estimate = round(coef_matrix[covariates, "ESTIMATE"], 3),
  SE = round(coef_matrix[covariates, "SE"], 3),
  `z-value` = round(coef_matrix[covariates, "ESTIMATE"] / coef_matrix[covariates, "SE"], 2),
  `p-value` = signif(coef_matrix[covariates, "p-val"], 4)
)

# Extract frailty variance (theta), log-likelihood, AIC, and BIC
frailty_variance <- round(coef_matrix["theta", "ESTIMATE"], 3)
log_likelihood <- round(attr(model_gam, "loglik"), 2)
AIC_val <- round(-2 * log_likelihood + 2 * length(covariates), 2)  # AIC formula: -2LL + 2k
BIC_val <- round(-2 * log_likelihood + log(attr(model_gam, "nobs")) * length(covariates), 2)  # BIC formula

# Display the results
cat("Coefficients:\n")
print(results)
cat("\nFrailty Variance (θ):", frailty_variance, "\n")
cat("Log-likelihood:", log_likelihood, "\n")
cat("AIC:", AIC_val, "\n")
cat("BIC:", BIC_val, "\n")

###end model

##objective1

#: dist = "none"

library(flexsurv)

model_gomp <- flexsurvreg(Surv(time_to_itnusage, trtdnet_std) ~ endemic_std,
                          data = new_data2trial,
                          dist = "gompertz")

summary(model_gomp)
# Extract AIC
AIC(model_gomp)

# Extract BIC
BIC(model_gomp)

model_none <- parfm(Surv(time_to_itnusage, trtdnet_std) ~ endemic_std,
                  cluster = "hold",
                  data = new_data2trial,
                  dist = "gompertz",   # Baseline hazard: exponential, weibull, gompertz, loglogistic, lognormal
                  frailty = "none")     # Inverse Gaussian frailty
print(summary(model_none))

coef(model_none)



str(model_none)

coef_matrix <- model_none

# Identify the relevant covariates (age, mobile_access)
covariates <- c("endemic_std")

# Extract estimates, standard errors, and p-values for covariates
results <- data.frame(
  Estimate = round(coef_matrix[covariates, "ESTIMATE"], 3),
  SE = round(coef_matrix[covariates, "SE"], 3),
  `z-value` = round(coef_matrix[covariates, "ESTIMATE"] / coef_matrix[covariates, "SE"], 2),
  `p-value` = signif(coef_matrix[covariates, "p-val"], 4)
)

# Extract frailty variance (theta), log-likelihood, AIC, and BIC
#frailty_variance <- round(coef_matrix["theta", "ESTIMATE"], 3)
log_likelihood <- round(attr(model_none, "loglik"), 2)
AIC_val <- round(-2 * log_likelihood + 2 * length(covariates), 2)  # AIC formula: -2LL + 2k
BIC_val <- round(-2 * log_likelihood + log(attr(model_none, "nobs")) * length(covariates), 2)  # BIC formula

# Display the results
cat("Coefficients:\n")
print(results)
cat("\nFrailty Variance (θ):", frailty_variance, "\n")
cat("Log-likelihood:", log_likelihood, "\n")
cat("AIC:", AIC_val, "\n")
cat("BIC:", BIC_val, "\n")

###end model



# Fit separate Kaplan-Meier models for each factor
km_fit_endemic <- survfit(Surv(time_to_itnusage, trtdnet_std) ~ endemic_std, data = new_data2trial)


# Plot Kaplan-Meier curve for smartphone access
p1_a <- ggsurvplot(km_fit_endemic, data = new_data2trial,
                  pval = TRUE, conf.int = TRUE, risk.table = TRUE,
                  title = "Kaplan-Meier Curve for endemic/non-endemic",
                  xlab = "Time to ITN Usage", ylab = "Survival Probability",
                  legend.title = "Child age")




arrange_ggsurvplots(list(p1_a), ncol = 1, nrow = 1)






###start model objective 2


new_data3 <- completed_data
View
(new_data3)

new_data3 <- new_data3 %>% filter(!is.na(time_to_itnusage))
View(new_data3)

is.numeric(new_data3$hold)
# Convert to factor
new_data3$V002 <- as.factor(new_data3$hold)
is.factor(new_data3$hold)

colSums(is.na(new_data3))
length(unique(new_data3$hold))

coxph(Surv(time_to_itnusage, trtdnet_std) ~ endemic, data = new_data3)

new_data3_cleaned <- new_data3[!is.na(new_data3$trtdnet_std), ]

new_data3trial <- new_data3_cleaned %>% select(trtdnet_std,time_to_itnusage,sm_phone_std,int_use_std,soc_media_std,
                                               hold)
View(new_data3trial)

# Example: Create a frequency table for a variable
freq_table <- table(new_data3trial$trtdnet_std, useNA = "ifany")

# Print the frequency table
print(freq_table)

freq_table <- new_data3trial %>%
  count(trtdnet_std)

# Print the frequency table
print(freq_table)

# Fit the Cox model
cox_model1 <- coxph(Surv(time_to_itnusage, trtdnet_std) ~ sm_phone_std+int_use_std+soc_media_std + cluster(hold),
                   data = new_data3trial)
# Summary of the Cox model
summary(cox_model1)
# Extract the log-likelihood
loglik1 <- logLik(cox_model1)
loglik1

# Number of parameters (df)
df1 <- attr(loglik1, "df")

# Number of observations
n1 <- cox_model1$n

# Calculate AIC
aic <- -2 * as.numeric(loglik1) + 2 * df1

# Calculate BIC
bic <- -2 * as.numeric(loglik1) + log(n1) * df1

# Print results
cat("Log-Likelihood:", as.numeric(loglik1), "\n")
cat("AIC:", aic, "\n")
cat("BIC:", bic, "\n")

#Weibull: dist = "gompertz"
model_iga <- parfm(Surv(time_to_itnusage, trtdnet_std) ~ sm_phone_std+int_use_std+soc_media_std,
                   cluster = "hold",
                   data = new_data3trial,
                   dist = "gompertz",   # Baseline hazard: exponential, weibull, gompertz, loglogistic, lognormal
                   frailty = "ingau")     # Inverse Gaussian frailty
print(summary(model_iga))

coef(model_iga)



str(model_ig)

coef_matrixa <- model_iga

# Identify the relevant covariates (sm_phone_std+int_use_std+soc_media_std)
covariatesa <- c("sm_phone_std", "int_use_std","soc_media_std")

# Extract estimates, standard errors, and p-values for covariates
resultsa <- data.frame(
  Estimate = round(coef_matrixa[covariatesa, "ESTIMATE"], 3),
  SE = round(coef_matrixa[covariatesa, "SE"], 3),
  `z-value` = round(coef_matrixa[covariatesa, "ESTIMATE"] / coef_matrixa[covariatesa, "SE"], 2),
  `p-value` = signif(coef_matrixa[covariatesa, "p-val"], 4)
)

# Extract frailty variance (theta), log-likelihood, AIC, and BIC
frailty_variance <- round(coef_matrixa["theta", "ESTIMATE"], 3)
log_likelihood <- round(attr(model_iga, "loglik"), 2)
AIC_val <- round(-2 * log_likelihood + 2 * length(covariatesa), 2)  # AIC formula: -2LL + 2k
BIC_val <- round(-2 * log_likelihood + log(attr(model_iga, "nobs")) * length(covariatesa), 2)  # BIC formula

# Display the results
cat("Coefficients:\n")
print(resultsa)
cat("\nFrailty Variance (θ):", frailty_variance, "\n")
cat("Log-likelihood:", log_likelihood, "\n")
cat("AIC:", AIC_val, "\n")
cat("BIC:", BIC_val, "\n")

###end model
#Weibull: dist = "gompertz"
model_iga <- parfm(Surv(time_to_itnusage, trtdnet_std) ~ sm_phone_std,
                   cluster = "hold",
                   data = new_data3trial,
                   dist = "gompertz",   # Baseline hazard: exponential, weibull, gompertz, loglogistic, lognormal
                   frailty = "ingau")     # Inverse Gaussian frailty
print(summary(model_iga))

coef(model_iga)



str(model_ig)

coef_matrixa <- model_iga

# Identify the relevant covariates (sm_phone_std+int_use_std+soc_media_std)
covariatesa <- c("sm_phone_std")

# Extract estimates, standard errors, and p-values for covariates
resultsa <- data.frame(
  Estimate = round(coef_matrixa[covariatesa, "ESTIMATE"], 3),
  SE = round(coef_matrixa[covariatesa, "SE"], 3),
  `z-value` = round(coef_matrixa[covariatesa, "ESTIMATE"] / coef_matrixa[covariatesa, "SE"], 2),
  `p-value` = signif(coef_matrixa[covariatesa, "p-val"], 4)
)

# Extract frailty variance (theta), log-likelihood, AIC, and BIC
frailty_variance <- round(coef_matrixa["theta", "ESTIMATE"], 3)
log_likelihood <- round(attr(model_iga, "loglik"), 2)
AIC_val <- round(-2 * log_likelihood + 2 * length(covariatesa), 2)  # AIC formula: -2LL + 2k
BIC_val <- round(-2 * log_likelihood + log(attr(model_iga, "nobs")) * length(covariatesa), 2)  # BIC formula

# Display the results
cat("Coefficients:\n")
print(resultsa)
cat("\nFrailty Variance (θ):", frailty_variance, "\n")
cat("Log-likelihood:", log_likelihood, "\n")
cat("AIC:", AIC_val, "\n")
cat("BIC:", BIC_val, "\n")

###end model

#Weibull: dist = "gompertz"frailty=gamma for sm_phone only significant var
model_igama <- parfm(Surv(time_to_itnusage, trtdnet_std) ~ sm_phone_std,
                   cluster = "hold",
                   data = new_data3trial,
                   dist = "gompertz",   # Baseline hazard: exponential, weibull, gompertz, loglogistic, lognormal
                   frailty = "gamma")     # Inverse Gaussian frailty
print(summary(model_igama))

coef(model_igama)



str(model_igama)

coef_matrixaga <- model_igama

# Identify the relevant covariates (sm_phone_std+int_use_std+soc_media_std)
covariatesaga <- c("sm_phone_std")

# Extract estimates, standard errors, and p-values for covariates
resultsaga <- data.frame(
  Estimate = round(coef_matrixaga[covariatesaga, "ESTIMATE"], 3),
  SE = round(coef_matrixaga[covariatesaga, "SE"], 3),
  `z-value` = round(coef_matrixaga[covariatesaga, "ESTIMATE"] / coef_matrixaga[covariatesaga, "SE"], 2),
  `p-value` = signif(coef_matrixaga[covariatesaga, "p-val"], 4)
)

# Extract frailty variance (theta), log-likelihood, AIC, and BIC
frailty_variancegga <- round(coef_matrixaga["theta", "ESTIMATE"], 3)
log_likelihoodgga <- round(attr(model_igama, "loglik"), 2)
AIC_val <- round(-2 * log_likelihoodgga + 2 * length(covariatesaga), 2)  # AIC formula: -2LL + 2k
BIC_val <- round(-2 * log_likelihoodgga + log(attr(model_igama, "nobs")) * length(covariatesaga), 2)  # BIC formula

# Display the results
cat("Coefficients:\n")
print(resultsaga)
cat("\nFrailty Variance (θ):", frailty_variancegga, "\n")
cat("Log-likelihood:", log_likelihoodgga, "\n")
cat("AIC:", AIC_val, "\n")
cat("BIC:", BIC_val, "\n")

###end model


# Fit the Cox model  for sm_phone only significant var
cox_model1 <- coxph(Surv(time_to_itnusage, trtdnet_std) ~ sm_phone_std+ cluster(hold),
                    data = new_data3trial)
# Summary of the Cox model
summary(cox_model1)
# Extract the log-likelihood
loglik1 <- logLik(cox_model1)
loglik1

# Number of parameters (df)
df1 <- attr(loglik1, "df")

# Number of observations
n1 <- cox_model1$n

# Calculate AIC
aic <- -2 * as.numeric(loglik1) + 2 * df1

# Calculate BIC
bic <- -2 * as.numeric(loglik1) + log(n1) * df1

# Print results
cat("Log-Likelihood:", as.numeric(loglik1), "\n")
cat("AIC:", aic, "\n")
cat("BIC:", bic, "\n")

#Weibull: dist = "gompertz"frailty=none
model_gompdig <- flexsurvreg(Surv(time_to_itnusage, trtdnet_std) ~ sm_phone_std,
                          data = new_data3trial,
                          dist = "gompertz")

summary(model_gomp)
# Extract AIC
AIC(model_gomp)

# Extract BIC
BIC(model_gomp)

model_inone <- parfm(Surv(time_to_itnusage, trtdnet_std) ~ sm_phone_std+int_use_std+soc_media_std,
                     cluster = "hold",
                     data = new_data3trial,
                     dist = "gompertz",   # Baseline hazard: exponential, weibull, gompertz, loglogistic, lognormal
                     frailty = "none")     # Inverse Gaussian frailty
print(summary(model_inone))

coef(model_inone)



str(model_inone)

coef_matrixan <- model_inone

# Identify the relevant covariates (sm_phone_std+int_use_std+soc_media_std)
covariatesan <- c("sm_phone_std", "int_use_std","soc_media_std")

# Extract estimates, standard errors, and p-values for covariates
resultsan <- data.frame(
  Estimate = round(coef_matrixa[covariatesa, "ESTIMATE"], 3),
  SE = round(coef_matrixa[covariatesa, "SE"], 3),
  `z-value` = round(coef_matrixa[covariatesa, "ESTIMATE"] / coef_matrixa[covariatesa, "SE"], 2),
  `p-value` = signif(coef_matrixa[covariatesa, "p-val"], 4)
)

# Extract frailty variance (theta), log-likelihood, AIC, and BIC
frailty_variance <- round(coef_matrixa["theta", "ESTIMATE"], 3)
log_likelihoodn <- round(attr(model_inone, "loglik"), 2)
AIC_val <- round(-2 * log_likelihoodn + 2 * length(covariatesan), 2)  # AIC formula: -2LL + 2k
BIC_val <- round(-2 * log_likelihoodn + log(attr(model_inone, "nobs")) * length(covariatesan), 2)  # BIC formula

# Display the results
cat("Coefficients:\n")
print(resultsan)
cat("\nFrailty Variance (θ):", frailty_variance, "\n")
cat("Log-likelihood:", log_likelihoodn, "\n")
cat("AIC:", AIC_val, "\n")
cat("BIC:", BIC_val, "\n")

###end model
##kaplan-meir curve
# Kaplan-Meier survival estimation
table(new_data3trial$trtdnet_std)
new_data3trial_clean <- new_data3trial %>% filter(!is.na(trtdnet_std))
km_fit <- survfit(Surv(time_to_itnusage, trtdnet_std) ~ sm_phone_std+int_use_std+soc_media_std, data = new_data3trial_clean)



# Load necessary libraries
library(survival)
library(survminer)

# Fit separate Kaplan-Meier models for each factor
km_fit_phone <- survfit(Surv(time_to_itnusage, trtdnet_std) ~ sm_phone_std, data = new_data3trial_clean)
km_fit_intuse <- survfit(Surv(time_to_itnusage, trtdnet_std) ~ int_use_std, data = new_data3trial_clean)
km_fit_socmed <- survfit(Surv(time_to_itnusage, trtdnet_std) ~ soc_media_std, data = new_data3trial_clean)

# Plot Kaplan-Meier curve for smartphone access
p1 <- ggsurvplot(km_fit_phone, data = new_data3trial_clean,
                 pval = TRUE, conf.int = TRUE, risk.table = TRUE,
                 title = "Kaplan-Meier Curve for Smartphone Access",
                 xlab = "Time to ITN Usage", ylab = "Survival Probability",
                 legend.title = "Smartphone Access")

# Plot Kaplan-Meier curve for internet use
p2 <- ggsurvplot(km_fit_intuse, data = new_data3trial_clean,
                 pval = TRUE, conf.int = TRUE, risk.table = TRUE,
                 title = "Kaplan-Meier Curve for Internet Use",
                 xlab = "Time to ITN Usage", ylab = "Survival Probability",
                 legend.title = "Internet Access")

# Plot Kaplan-Meier curve for social media use
p3 <- ggsurvplot(km_fit_socmed, data = new_data3trial_clean,
                 pval = TRUE, conf.int = TRUE, risk.table = TRUE,
                 title = "Kaplan-Meier Curve for Social Media Use",
                 xlab = "Time to ITN Usage", ylab = "Survival Probability",
                 legend.title = "Social Media Use")

# Arrange the three plots in a single figure
arrange_ggsurvplots(list(p1, p2, p3), ncol = 3, nrow = 1)

arrange_ggsurvplots(list(p1), ncol = 1, nrow = 1)
arrange_ggsurvplots(list(p2), ncol = 1, nrow = 1)
arrange_ggsurvplots(list(p3), ncol = 1, nrow = 1)













##end

###start model  obj 3 ###


new_data3 <- completed_data
View
(new_data3)

new_data3 <- new_data3 %>% filter(!is.na(time_to_itnusage))
View(new_data3)

is.numeric(new_data3$hold)
# Convert to factor
new_data3$V002 <- as.factor(new_data3$hold)
is.factor(new_data3$hold)

colSums(is.na(new_data3))
length(unique(new_data3$hold))

coxph(Surv(time_to_itnusage, trtdnet_std) ~ endemic, data = new_data3)

new_data3_cleaned <- new_data3[!is.na(new_data3$trtdnet_std), ]

new_data4trial <- new_data3_cleaned %>% select(trtdnet_std,time_to_itnusage,child_age,mal_symptoms_std,act_adherence_std,
                                               hold)
View(new_data4trial)

# Example: Create a frequency table for a variable
freq_table <- table(new_data4trial$trtdnet_std, useNA = "ifany")

# Print the frequency table
print(freq_table)

freq_table <- new_data4trial %>%
  count(trtdnet_std)

# Print the frequency table
print(freq_table)

#Weibull: dist = "gompertz"
model_igb <- parfm(Surv(time_to_itnusage, trtdnet_std) ~ child_age+mal_symptoms_std+act_adherence_std,
                   cluster = "hold",
                   data = new_data4trial,
                   dist = "gompertz",   # Baseline hazard: exponential, weibull, gompertz, loglogistic, lognormal
                   frailty = "ingau")     # Inverse Gaussian frailty
print(summary(model_igb))

coef(model_igb)



str(model_igb)

coef_matrixb <- model_igb

# Identify the relevant covariates (sm_phone_std+int_use_std+soc_media_std)
covariatesb <- c("child_age", "mal_symptoms_std","act_adherence_std")

# Extract estimates, standard errors, and p-values for covariates
resultsb <- data.frame(
  Estimate = round(coef_matrixb[covariatesb, "ESTIMATE"], 3),
  SE = round(coef_matrixb[covariatesb, "SE"], 3),
  `z-value` = round(coef_matrixb[covariatesb, "ESTIMATE"] / coef_matrixb[covariatesb, "SE"], 2),
  `p-value` = signif(coef_matrixb[covariatesb, "p-val"], 2)
)

# Extract frailty variance (theta), log-likelihood, AIC, and BIC
frailty_variance <- round(coef_matrixb["theta", "ESTIMATE"], 3)
log_likelihood <- round(attr(model_igb, "loglik"), 2)
AIC_val <- round(-2 * log_likelihood + 2 * length(covariatesb), 2)  # AIC formula: -2LL + 2k
BIC_val <- round(-2 * log_likelihood + log(attr(model_igb, "nobs")) * length(covariatesa), 2)  # BIC formula

# Display the results
cat("Coefficients:\n")
print(resultsb)
cat("\nFrailty Variance (θ):", frailty_variance, "\n")
cat("Log-likelihood:", log_likelihood, "\n")
cat("AIC:", AIC_val, "\n")
cat("BIC:", BIC_val, "\n")

###end model


#Weibull: dist = "gompertz"
model_igbga <- parfm(Surv(time_to_itnusage, trtdnet_std) ~ child_age+mal_symptoms_std,
                   cluster = "hold",
                   data = new_data4trial,
                   dist = "gompertz",   # Baseline hazard: exponential, weibull, gompertz, loglogistic, lognormal
                   frailty = "ingau")     # Inverse Gaussian frailty
print(summary(model_igbga))

coef(model_igbga)



str(model_igbga)

coef_matrixbga <- model_igbga

# Identify the relevant covariates (sm_phone_std+int_use_std+soc_media_std)
covariatesbga <- c("child_age", "mal_symptoms_std")

# Extract estimates, standard errors, and p-values for covariates
resultsbga <- data.frame(
  Estimate = round(coef_matrixbga[covariatesbga, "ESTIMATE"], 3),
  SE = round(coef_matrixbga[covariatesbga, "SE"], 3),
  `z-value` = round(coef_matrixbga[covariatesbga, "ESTIMATE"] / coef_matrixbga[covariatesbga, "SE"], 2),
  `p-value` = signif(coef_matrixbga[covariatesbga, "p-val"], 4)
)

# Extract frailty variance (theta), log-likelihood, AIC, and BIC
frailty_variancega <- round(coef_matrixbga["theta", "ESTIMATE"], 3)
log_likelihoodga <- round(attr(model_igbga, "loglik"), 2)
AIC_val <- round(-2 * log_likelihoodga + 2 * length(covariatesbga), 2)  # AIC formula: -2LL + 2k
BIC_val <- round(-2 * log_likelihoodga + log(attr(model_igbga, "nobs")) * length(covariatesbga), 2)  # BIC formula

# Display the results
cat("Coefficients:\n")
print(resultsbga)
cat("\nFrailty Variance (θ):", frailty_variancega, "\n")
cat("Log-likelihood:", log_likelihoodga, "\n")
cat("AIC:", AIC_val, "\n")
cat("BIC:", BIC_val, "\n")

###end model


#Weibull: dist = "gompertz"
model_igbga <- parfm(Surv(time_to_itnusage, trtdnet_std) ~ child_age+mal_symptoms_std,
                     cluster = "hold",
                     data = new_data4trial,
                     dist = "gompertz",   # Baseline hazard: exponential, weibull, gompertz, loglogistic, lognormal
                     frailty = "gamma")     # Inverse Gaussian frailty
print(summary(model_igbga))

coef(model_igbga)



str(model_igbga)

coef_matrixbga <- model_igbga

# Identify the relevant covariates (sm_phone_std+int_use_std+soc_media_std)
covariatesbga <- c("child_age", "mal_symptoms_std")

# Extract estimates, standard errors, and p-values for covariates
resultsbga <- data.frame(
  Estimate = round(coef_matrixbga[covariatesbga, "ESTIMATE"], 3),
  SE = round(coef_matrixbga[covariatesbga, "SE"], 3),
  `z-value` = round(coef_matrixbga[covariatesbga, "ESTIMATE"] / coef_matrixbga[covariatesbga, "SE"], 2),
  `p-value` = signif(coef_matrixbga[covariatesbga, "p-val"], 4)
)

# Extract frailty variance (theta), log-likelihood, AIC, and BIC
frailty_variancega <- round(coef_matrixbga["theta", "ESTIMATE"], 3)
log_likelihoodga <- round(attr(model_igbga, "loglik"), 2)
AIC_val <- round(-2 * log_likelihoodga + 2 * length(covariatesbga), 2)  # AIC formula: -2LL + 2k
BIC_val <- round(-2 * log_likelihoodga + log(attr(model_igbga, "nobs")) * length(covariatesbga), 2)  # BIC formula

# Display the results
cat("Coefficients:\n")
print(resultsbga)
cat("\nFrailty Variance (θ):", frailty_variancega, "\n")
cat("Log-likelihood:", log_likelihoodga, "\n")
cat("AIC:", AIC_val, "\n")
cat("BIC:", BIC_val, "\n")

###end model

# Fit the Cox model
cox_model11 <- coxph(Surv(time_to_itnusage, trtdnet_std) ~ child_age+mal_symptoms_std+ cluster(hold),
                    data = new_data4trial)
# Summary of the Cox model
summary(cox_model11)
# Extract the log-likelihood
loglik11 <- logLik(cox_model11)
loglik11

# Number of parameters (df)
df11 <- attr(loglik11, "df")

# Number of observations
n11 <- cox_model11$n

# Calculate AIC
aic <- -2 * as.numeric(loglik11) + 2 * df11

# Calculate BIC
bic <- -2 * as.numeric(loglik11) + log(n1) * df11

# Print results
cat("Log-Likelihood:", as.numeric(loglik11), "\n")
cat("AIC:", aic, "\n")
cat("BIC:", bic, "\n")




new_data3 <- completed_data
View(new_data3)




# Fit separate Kaplan-Meier models for each factor
#km_fit_age <- survfit(Surv(time_to_itnusage, trtdnet_std) ~ child_age, data = new_data4trial)
km_fit_mals <- survfit(Surv(time_to_itnusage, trtdnet_std) ~ mal_symptoms_std, data = new_data4trial)
km_fit_act <- survfit(Surv(time_to_itnusage, trtdnet_std) ~ act_adherence_std, data = new_data4trial)

# Plot Kaplan-Meier curve for smartphone access
#p1a <- ggsurvplot(km_fit_age, data = new_data4trial,
                 #pval = TRUE, conf.int = TRUE, risk.table = TRUE,
                 #title = "Kaplan-Meier Curve for Child age",
                 #xlab = "Time to ITN Usage", ylab = "Survival Probability",
                 #legend.title = "Child age")

# Plot Kaplan-Meier curve for internet use
p2a <- ggsurvplot(km_fit_mals, data = new_data4trial,
                 pval = TRUE, conf.int = TRUE, risk.table = TRUE,
                 title = "Kaplan-Meier Curve for malaria symptoms",
                 xlab = "Time to ITN Usage", ylab = "Survival Probability",
                 legend.title = "malaria symptoms")

# Plot Kaplan-Meier curve for social media use
p3a <- ggsurvplot(km_fit_act, data = new_data4trial,
                 pval = TRUE, conf.int = TRUE, risk.table = TRUE,
                 title = "Kaplan-Meier Curve for ACT Adherence",
                 xlab = "Time to ITN Usage", ylab = "Survival Probability",
                 legend.title = "ACT Adherence")

# Arrange the three plots in a single figure
arrange_ggsurvplots(list(p2a, p3a), ncol = 2, nrow = 1)

arrange_ggsurvplots(list(p2a), ncol = 1, nrow = 1)
arrange_ggsurvplots(list(p3a), ncol = 1, nrow = 1)

###MODEL RUNNING

#new_data3 <- new_data3 %>% filter(!is.na(time_to_itnusage))
#View(new_data3)

#is.numeric(new_data3$hold)
# Convert to factor
#new_data3$hold <- as.factor(new_data3$hold)
#is.factor(new_data3$hold)

#colSums(is.na(new_data3))
#length(unique(new_data3$hold))

#cox_model <- coxph(Surv(time_to_itnusage, trtdnet_std) ~ endemic, data = new_data3)
#cox.zph(cox_model)

