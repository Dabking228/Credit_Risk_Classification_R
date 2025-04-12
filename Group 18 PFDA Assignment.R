# Group 18 - Programming for Data Analysis Assignment

# Group Members:
## Daryl Sim Wei Shern TP068964
## Choo Cheng Da TP068973
## Cheong Sheue Ling TP069004
## Ho Shane Foong TP068496
## John Har Wey Jon TP068348

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#

# Load the libraries and packages
library(dplyr)
library(readr)
library(ggplot2)
library(DataExplorer)
library(missForest)
library(tidyverse)
library(VIM)
library(caret)
library(caTools)
library(randomForest)
library(vcd)
library(plotly)
library(readxl)
library(httr)
library(broom)

# Set the working directory
## Enter your own path
setwd("C:/APU/Degree/Semester 1/Programming for Data Analysis/Assignment/PFDA Assignment")
getwd()


# Import the dataset
## Enter your own file path
filePath = "C:/APU/Degree/Semester 1/Programming for Data Analysis/Assignment/PFDA Assignment/5. credit_risk_classification.csv"

## Read the CSV file into a dataframe
pfda_df = read_csv(filePath)
View(pfda_df)


# Data Exploration
dim(pfda_df)
nrow(pfda_df)
ncol(pfda_df)
colnames(pfda_df)
spec(pfda_df)
sapply(pfda_df, class)
str(pfda_df)
glimpse(pfda_df)
summary(pfda_df)



# Data Cleaning and Preprocessing

## Delete the first column
pfda_df <- pfda_df[, -1]

## Rename the necessary columns 
## Exclude credit_amount, personal_status, other_parties, property_magnitude, age, other_payment_plans, existing_credits, num_dependents, own_telephone
pfda_df <- pfda_df %>% 
  rename(
    checking_account_status = checking_status,
    loan_duration_months = duration,
    credit_history_status = credit_history,
    loan_purpose = purpose,
    savings_account_status = savings_status,
    employment_years = employment,
    installment_rate_percent = installment_commitment,
    residence_years = residence_since,
    housing_type = housing,
    job_type = job,
    is_foreign_worker = foreign_worker,
    credit_risk_class = class
  )
## Check the column names after renaming
colnames(pfda_df)

## Check the total number of duplicated rows
sum(duplicated(pfda_df))

## Check the total number of missing values for each column
colSums(is.na(pfda_df))
plot_missing(pfda_df)



## Check and analyse the columns one by one 

## 1.0 checking_account_status column (previously known as checking_status)

# 1.1 Check the details of this column
summary(pfda_df$checking_account_status)

# 1.2 Check the frequency of each unique category
unique(pfda_df$checking_account_status)
as.data.frame(table(pfda_df$checking_account_status))



## 2.0 loan_duration_months column (previously known as duration)

# 2.1 Check the details of this column
summary(pfda_df$loan_duration_months)

# 2.2 Check all unique numbers
unique(pfda_df$loan_duration_months)

# 2.3 Check the distribution of the loan duration
hist(pfda_df$loan_duration_months)

# 2.4 Convert the loan duration in months from double to integer by using standard rounding
pfda_df$loan_duration_months <- round(pfda_df$loan_duration_months)
pfda_df$loan_duration_months <- as.integer(pfda_df$loan_duration_months)
unique(pfda_df$loan_duration_months)
as.data.frame(table(pfda_df$loan_duration_months))

# 2.5 Check the distribution of the loan duration
hist(pfda_df$loan_duration_months)



## 3.0 credit_history_status column (previously known as credit_history)

# 3.1 Check the details of this column
summary(pfda_df$credit_history_status)

# 3.2 Check the frequency of each unique category
unique(pfda_df$credit_history_status)
as.data.frame(table(pfda_df$credit_history_status))

# 3.3 Replace "no credits/all paid" values with "all paid"
pfda_df <- pfda_df %>% 
  mutate(credit_history_status = ifelse(credit_history_status == "no credits/all paid", "all paid", credit_history_status))

# 3.4 Double check the frequency of each unique category
unique(pfda_df$credit_history_status)
as.data.frame(table(pfda_df$credit_history_status))



## 4.0 loan_purpose column (previously known as purpose)
# 4.1 Check the frequency of each unique category
summary(pfda_df$loan_purpose)

# 4.2 Check the frequency of each unique category
unique(pfda_df$loan_purpose)
as.data.frame(table(pfda_df$loan_purpose))

# 4.3 Replace "radio/tv" values with "domestic appliance"
pfda_df <- pfda_df %>% 
  mutate(loan_purpose = ifelse(loan_purpose == "radio/tv", "domestic appliance", loan_purpose))

# 4.4 Double check the frequency of each unique category
unique(pfda_df$loan_purpose)
as.data.frame(table(pfda_df$loan_purpose))



## 5.0 credit_amount column

# 5.1 Check the details of this column
summary(pfda_df$credit_amount)

# 5.2 Check all unique numbers
unique(pfda_df$credit_amount)

# 5.3 Check the distribution of the credit amount
hist(pfda_df$credit_amount)

# 5.4 Convert the credit amount from double to integer by using standard rounding
pfda_df$credit_amount <- round(pfda_df$credit_amount)
pfda_df$credit_amount <- as.integer(pfda_df$credit_amount)
unique(pfda_df$credit_amount)
as.data.frame(table(pfda_df$credit_amount))

# 5.5 Check the distribution of the credit amount
hist(pfda_df$credit_amount)



## 6.0 savings_account_status column (previously known as savings_status)

# 6.1 Check the details of this column
summary(pfda_df$savings_account_status)

# 6.2 Check the frequency of each unique category
unique(pfda_df$savings_account_status)
as.data.frame(table(pfda_df$savings_account_status))



## 7.0 employment_years column (previously known as employment)

# 7.1 Check the details of this column
summary(pfda_df$employment_years)

# 7.2 Check the frequency of each unique category
unique(pfda_df$employment_years)
as.data.frame(table(pfda_df$employment_years))



## 8.0 installment_rate_percent column (previously known as installment_commitment)

# 8.1 Check the details of this column
summary(pfda_df$installment_rate_percent)

# 8.2 Check all unique numbers
unique(pfda_df$installment_rate_percent)

# 8.3 Check the distribution of the installment rate
hist(pfda_df$installment_rate_percent)

# 8.4 Convert the installment rate from double to integer by using standard rounding
pfda_df$installment_rate_percent <- round(pfda_df$installment_rate_percent)
pfda_df$installment_rate_percent <- as.integer(pfda_df$installment_rate_percent)
unique(pfda_df$installment_rate_percent)
as.data.frame(table(pfda_df$installment_rate_percent))

# 8.5 Check the distribution of the installment rate
hist(pfda_df$installment_rate_percent)



## 9.0 personal_status column

# 9.1 Check the details of this column
summary(pfda_df$personal_status)

# 9.2 Check the frequency of each unique category
unique(pfda_df$personal_status)
as.data.frame(table(pfda_df$personal_status))



## 10.0 other_parties column

# 10.1 Check the details of this column
summary(pfda_df$other_parties)

# 10.2 Check the frequency of each unique category
unique(pfda_df$other_parties)
as.data.frame(table(pfda_df$other_parties))



## 11.0 residence_years column (previously known as residence_since)

# 11.1 Check the details of this column
summary(pfda_df$residence_years)

# 11.2 Check all unique numbers
unique(pfda_df$residence_years)

# 11.3 Check the distribution of the residence period
hist(pfda_df$residence_years)

# 11.4 Convert the residence period from double to integer by using standard rounding
pfda_df$residence_years <- round(pfda_df$residence_years)
pfda_df$residence_years <- as.integer(pfda_df$residence_years)
unique(pfda_df$residence_years)
as.data.frame(table(pfda_df$residence_years))

# 11.5 Check the distribution of the residence period
hist(pfda_df$residence_years)



## 12.0 property_magnitude column

# 12.1 Check the details of this column
summary(pfda_df$property_magnitude)

# 12.2 Check the frequency of each unique category
unique(pfda_df$property_magnitude)
as.data.frame(table(pfda_df$property_magnitude))



## 13.0 age column

# 13.1 Check the details of this column
summary(pfda_df$age)

# 13.2 Check all unique numbers
unique(pfda_df$age)

# 13.3 Check the distribution of the age
hist(pfda_df$age)

# 13.4 Convert the age from double to integer by using standard rounding
pfda_df$age <- round(pfda_df$age)
pfda_df$age <- as.integer(pfda_df$age)
unique(pfda_df$age)
as.data.frame(table(pfda_df$age))

# 13.5 Check the distribution of the age
hist(pfda_df$age)



## 14.0 other_payment_plans column
# - Will handle after cleaning and preprocessing all the other columns
# - Please refer to the bottom


## 15.0 housing_type column (previously known as housing)

# 15.1 Check the details of this column
summary(pfda_df$housing_type)

# 15.2 Check the frequency of each unique category
unique(pfda_df$housing_type)
as.data.frame(table(pfda_df$housing_type))



## 16.0 existing_credits column

# 16.1 Check the details of this column
summary(pfda_df$existing_credits)

# 16.2 Check all unique numbers
unique(pfda_df$existing_credits)

# 16.3 Check the distribution of the existing credits
hist(pfda_df$existing_credits)

# 16.4 Convert the existing credits from double to integer by using standard rounding
pfda_df$existing_credits <- round(pfda_df$existing_credits)
pfda_df$existing_credits <- as.integer(pfda_df$existing_credits)
unique(pfda_df$existing_credits)
as.data.frame(table(pfda_df$existing_credits))

# 16.5 Check the distribution of the existing credits
hist(pfda_df$existing_credits)



## 17.0 job_type column (previously known as job)

# 17.1 Check the details of this column
summary(pfda_df$job_type)

# 17.2 Check the frequency of each unique category
unique(pfda_df$job_type)
as.data.frame(table(pfda_df$job_type))



## 18.0 num_dependants column

# 18.1 Check the details of this column
summary(pfda_df$num_dependants)

# 18.2 Check all unique numbers
unique(pfda_df$num_dependants)

# 18.3 Check the distribution of the number of dependants
hist(pfda_df$num_dependants)

# 18.4 Convert the number of dependants from double to integer by using standard rounding
pfda_df$num_dependants <- round(pfda_df$num_dependants)
pfda_df$num_dependants <- as.integer(pfda_df$num_dependants)
unique(pfda_df$num_dependants)
as.data.frame(table(pfda_df$num_dependants))

# 18.5 Check the distribution of the number of dependants
hist(pfda_df$num_dependants)



## 19.0 own_telephone column

# 19.1 Check the details of this column
summary(pfda_df$own_telephone)

# 19.2 Check the frequency of each unique category
unique(pfda_df$own_telephone)
as.data.frame(table(pfda_df$own_telephone))



## 20.0 is_foreign_worker column (previously known as foreign_worker)

# 20.1 Check the details of this column
summary(pfda_df$is_foreign_worker)

# 20.2 Check the frequency of each unique category
unique(pfda_df$is_foreign_worker)
as.data.frame(table(pfda_df$is_foreign_worker))



##  14.0 other_payment_plans column (Continuation from above)

# 14.1 Check the details of this column
summary(pfda_df$other_payment_plans)

# 14.2 Check the frequency of each unique category
unique(pfda_df$other_payment_plans)
as.data.frame(table(pfda_df$other_payment_plans))

# 14.3 Check the number of NA values
sum(is.na(pfda_df$other_payment_plans))

# 14.4 Convert the data type of all categorical columns from character to factor
# To prepare the data for Random Forest imputation
pfda_df <- pfda_df %>% 
  mutate(across(where(is.character), as.factor))
str(pfda_df)
sapply(pfda_df, class)

# 14.5 Replace missing or empty strings with NA
pfda_df[pfda_df == ""] <- NA

# 14.6 Convert pfda_df from tibble to data frame
pfda_df = as.data.frame(pfda_df)

# 14.7 Perform Random Forest imputation
imputed_data <- missForest(pfda_df)
pfda_df <- imputed_data$ximp

# 14.8 Check whether there are still any missing values in other_payment_plans column
sum(is.na(pfda_df$other_payment_plans))
unique(pfda_df$other_payment_plans)
as.data.frame(table(pfda_df$other_payment_plans))



# Data Validation

summary(pfda_df)

## Check the total number of missing values for each column
as.data.frame(colSums(is.na(pfda_df)))
plot_missing(pfda_df)


# Export the data frame containing the cleaned data to a new CSV file for individual analysis
current_directory = getwd()
export_path = file.path(current_directory, "(cleaned) credit_risk_classification.csv")
write.csv(pfda_df, export_path, row.names = FALSE)

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#

#                                                                      Data Analysis

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#

## Daryl Sim Wei Shern TP068964

# Duplicate the dataset for individual analysis
pfda_df_Daryl <- pfda_df
View(pfda_df_Daryl)

# Objective 1: To determine whether the loan duration impacts the credit risk classification of a customer.
# Independent Variable: loan_duration_months (previously known as duration)


# Analysis 1.1: Analyze the general information of the loan_duration_months and credit_risk_class columns

# Summarize the dataset
summary(pfda_df_Daryl$loan_duration_months)
table(pfda_df_Daryl$credit_risk_class)

# Summary statistics grouped by credit_risk_class
pfda_df_Daryl %>%
  group_by(credit_risk_class) %>%
  summarise(mean_duration = mean(loan_duration_months),
            median_duration = median(loan_duration_months),
            sd_duration = sd(loan_duration_months),
            count = n())

# Analysis 1.2: What is the distribution of loan_duration_months?

# Visualize the distribution of loan_duration_months
loan_duration_distribution <- ggplot(pfda_df_Daryl, aes(x = loan_duration_months)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  stat_bin(binwidth = 5, 
           geom = "text", 
           aes(label = ..count..), 
           vjust = -0.5, 
           size = 3.5) +
  labs(title = "Distribution of Loan Duration (Months)", 
       x = "Loan Duration (Months)", 
       y = "Count") +
  theme_minimal()
print(loan_duration_distribution)

# Save the plot
ggsave("Distribution of Loan Duration (Months).png", loan_duration_distribution, width = 12, height = 8, dpi = 300, bg="white")


# Analysis 1.3: Are there outliers in loan_duration_months by credit_risk_class?

# Visualize the loan_duration_months by credit_risk_class
loan_duration_box <- ggplot(pfda_df_Daryl, aes(x = credit_risk_class, y = loan_duration_months, fill = credit_risk_class)) +
  geom_boxplot() +
  labs(title = "Loan Duration by Credit Risk Class", 
       x = "Credit Risk Class", 
       y = "Loan Duration (Months)") +
  theme_minimal()
print(loan_duration_box)

# Save the plot
ggsave("Boxplot of Loan Duration by Credit Risk Class.png", loan_duration_box, width = 12, height = 8, dpi = 300, bg="white")


# Analysis 1.4: What is the distribution of loan_duration_months by credit_risk_class?

# Visualize the distribution of loan_duration_months by credit_risk_class
loan_duration_hist <- ggplot(pfda_df_Daryl, aes(x = loan_duration_months, fill = credit_risk_class)) +
  geom_histogram(position = "identity", alpha = 0.7, bins = 30) +
  labs(title = "Distribution of Loan Durations (Months) by Credit Risk Class",
       x = "Loan Durations (Months)",
       y = "Count") +
  theme_minimal()
print(loan_duration_hist)

# Save the plot
ggsave("Distribution of Loan Durations (Months) by Credit Risk Class.png", loan_duration_hist, width = 12, height = 8, dpi = 300, bg="white")


# Analysis 1.5: What is the distribution of loan_duration_months when it is sorted by ranges?

# Categorize the loan_duration_months for easier visualization
pfda_df_Daryl$loan_duration_category <- cut(pfda_df_Daryl$loan_duration_months, breaks = c(0, 12, 24, 36, Inf), 
                                            labels = c("0-12", "13-24", "25-36", "37+"))
summary(pfda_df_Daryl$loan_duration_category)

# Calculate the number of rows for each loan_duration_category
loan_category_count <- pfda_df_Daryl %>% 
  group_by(loan_duration_category) %>% 
  count()
View(loan_category_count)

# Visualize the distribution of loan_duration_category 
loan_duration_category_distribution <-
  ggplot(loan_category_count, aes(x = loan_duration_category, y = n, fill = loan_duration_category)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), fill = "skyblue") +
  geom_text(aes(label = n),
            position = position_dodge(width = 0.9),
            vjust = -0.5,
            size = 3.5) +
  labs(title = "Distribution of Loan Duration Categories", 
       x = "Loan Duration Category", 
       y = "Count") +
  theme_minimal()
print(loan_duration_category_distribution)

# Save the plot
ggsave("Distribution of Loan Duration Categories.png", loan_duration_category_distribution, width = 12, height = 8, dpi = 300, bg="white")


# Analysis 1.6: What is the distribution of good and bad credit_risk_class in each loan_duration_category?

# Calculate the number and percentage of good and bad credit_risk_class in each loan_duration_category
proportion_loan_duration_category <- pfda_df_Daryl %>% 
  group_by(loan_duration_category, credit_risk_class) %>% 
  summarise(count = n(), .groups = 'drop') %>% 
  group_by(loan_duration_category) %>% 
  mutate(percent = count / sum(count) * 100)
View(proportion_loan_duration_category)

# Visualize the distribution of good and bad credit_risk_class in each loan_duration_category 
loan_duration_category_bar <- 
  ggplot(proportion_loan_duration_category, aes(x = loan_duration_category, y = count, fill = credit_risk_class)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = count),
            position = position_dodge(width = 0.9),
            vjust = -0.5,
            size = 3.5) +
  labs(title = "Distribution of Good and Bad Credit Risk Class in each Loan Duration Category", 
       x = "Loan Duration Category", 
       y = "Count") +
  theme_minimal()
print(loan_duration_category_bar)

# Save the plot
ggsave("Distribution of Good and Bad Credit Risk Class in each Loan Duration Category.png", loan_duration_category_bar, width = 12, height = 8, dpi = 300, bg="white")

# Visualize the percentage of good and bad credit_risk_class in each loan_duration_category
proportion_loan_duration_category_bar <- 
  ggplot(proportion_loan_duration_category, aes(x = loan_duration_category, y = percent, fill = credit_risk_class)) + 
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) + 
  geom_text(aes(label = sprintf("%.1f%%", percent)),
            position = position_dodge(width = 0.9),
            vjust = -0.5,
            size = 3.5) +
  labs(title = "Percentage Distribution of Credit Risk Class in each Loan Duration Category", 
       x = "Loan Duration Category", 
       y = "Percentage (%)") + 
  scale_y_continuous(labels = scales::percent_format(scale = 1)) + 
  theme_minimal() + 
  theme(legend.position = "top")
print(proportion_loan_duration_category_bar)

# Save the plot
ggsave("Percentage Distribution of Credit Risk Class in each Loan Duration Category.png", proportion_loan_duration_category_bar, width = 12, height = 8, dpi = 300, bg="white")  


# Analysis 1.7: What is the distribution of good and bad credit_risk_class for loan duration less than or equal to 24 months and for loan terms greater than 24 months?

# Categorize the loan_duration_months for easier visualization
pfda_df_Daryl$loan_duration_category_less_than_24 <- cut(pfda_df_Daryl$loan_duration_months, breaks = c(0, 24, Inf), 
                                                         labels = c("0-24", "24+"))
summary(pfda_df_Daryl$loan_duration_category_less_than_24)
levels(pfda_df_Daryl$loan_duration_category_less_than_24)

# Calculate the percentage of good and bad credit_risk_class in 0-24 and 24+ months loan duration
proportion_loan_duration_category_less_than_24 <- pfda_df_Daryl %>% 
  group_by(loan_duration_category_less_than_24, credit_risk_class) %>% 
  summarise(count = n(), .groups = 'drop') %>% 
  group_by(loan_duration_category_less_than_24) %>% 
  mutate(percent = count / sum(count) * 100)
View(proportion_loan_duration_category_less_than_24)

# Visualize the percentage of good and bad credit_risk_class over the loan duration of 0-24 months and 24+ months
proportion_loan_duration_category_less_than_24_bar <- 
  ggplot(proportion_loan_duration_category_less_than_24, aes(x = loan_duration_category_less_than_24, y = percent, fill = credit_risk_class)) + 
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) + 
  geom_text(aes(label = sprintf("%.1f%%", percent)),
            position = position_dodge(width = 0.9),
            vjust = -0.5,
            size = 3.5) +
  labs(title = "Percentage of Good and Bad Credit Risk Class over the Loan Duration of 0-24 Months and 24+ Months", 
       x = "Loan Duration Category", 
       y = "Percentage (%)") + 
  scale_y_continuous(labels = scales::percent_format(scale = 1)) + 
  theme_minimal() + 
  theme(legend.position = "top")
print(proportion_loan_duration_category_less_than_24_bar)

# Save the plot
ggsave("Percentage of Good and Bad Credit Risk Class over the Loan Duration of 0-24 Months and 24+ Months.png", 
       proportion_loan_duration_category_less_than_24_bar, width = 12, height = 8, dpi = 300, bg="white")


# Analysis 1.8: What is the relationship between loan_duration_months and the probability of getting good credit_risk_class?

# Visualize the relationship between loan_duration_months and the probability of getting good credit_risk_class
loan_duration_vs_good_probability <- ggplot(pfda_df_Daryl, aes(x = loan_duration_months, y = as.numeric(credit_risk_class) - 1)) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  labs(title = "Loan Duration (Months) vs Probability of Getting Good Credit Risk Class",
       x = "Loan Duration (Months)",
       y = "Probability of Getting Good Credit Risk Class") +
  theme_minimal()
print(loan_duration_vs_good_probability)

# Save the plot
ggsave("Loan Duration (Months) vs Probability of Getting Good Credit Risk Class.png",
       loan_duration_vs_good_probability, width = 12, height = 8, dpi = 300, bg="white") 



# Form Hypothesis

# Null Hypothesis:
## Customers who apply for a loan with a duration of less than or equal to 24 months have a less than or equal to 50% probability of receiving
## a good credit risk classification compared to customers who apply for a loan with a duration of more than 24 months.

# Alternative Hypothesis: 
## Customers who apply for a loan with a duration of less than or equal to 24 months have a greater than 50% probability of receiving
## a good credit risk classification compared to customers who apply for a loan with a duration of more than 24 months.

# Accepting Criteria & Rejecting Criteria
## If p-value is less than or equal to 0.05, reject null hypothesis.
## If p-value is greater than 0.05, then fail to reject null hypothesis.


# Two sample t-test
loan_duration_t_test <- t.test(loan_duration_months ~ credit_risk_class, data = pfda_df_Daryl)
print(loan_duration_t_test)


# Chi-square test of independence
loan_duration_chisq_test <- chisq.test(table(pfda_df_Daryl$loan_duration_category, pfda_df_Daryl$credit_risk_class))
print(loan_duration_chisq_test)


# Cramér’s V
loan_duration_cramers_v <- assocstats(table(pfda_df_Daryl$loan_duration_category, pfda_df_Daryl$credit_risk_class))$cramer
print(loan_duration_cramers_v)


# To check the levels of credit_risk_class and to ensure that the order is "bad" to good"
levels(pfda_df_Daryl$credit_risk_class)


# Logistic Regression with all categories
logistic_loan_duration_category_log_model <- glm(credit_risk_class ~ loan_duration_category, data = pfda_df_Daryl, family = binomial)
logistic_loan_duration_category_log_model
summary(logistic_loan_duration_category_log_model)

## 0-12 months loan duration
# Calculate the odds ratio for 0-12 months loan duration
logistic_loan_duration_category_12_or <- exp(coef(logistic_loan_duration_category_log_model)["(Intercept)"])
print(paste("Odds Ratio for 0-12 months loan duration:", logistic_loan_duration_category_12_or))

# Good credit risk class probability for 0-12 months loan duration
logistic_duration_probability_0_12 <- logistic_loan_duration_category_12_or / (1 + logistic_loan_duration_category_12_or)
logistic_duration_probability_0_12 <- round(logistic_duration_probability_0_12 * 100, 1)
logistic_duration_probability_0_12_text <- paste0(logistic_duration_probability_0_12, "%")
print(paste("Good credit risk class probability for 0-12 months loan duration:", logistic_duration_probability_0_12_text))


## 13-24 months loan duration
## Calculate the log-odds for 13-24 months loan duration
logistic_loan_duration_category_24_or <- coef(logistic_loan_duration_category_log_model)["(Intercept)"] + 
  coef(logistic_loan_duration_category_log_model)["loan_duration_category13-24"]
# Convert the log-odds to odds ratio
logistic_loan_duration_category_24_or <- exp(logistic_loan_duration_category_24_or)
print(paste("Odds Ratio for 13-24 months loan duration:", logistic_loan_duration_category_24_or))

# Convert odds ratio to probability
logistic_duration_probability_13_24 <- logistic_loan_duration_category_24_or / (1 + logistic_loan_duration_category_24_or)
logistic_duration_probability_13_24 <- round(logistic_duration_probability_13_24 * 100, 1)

# Good credit risk class probability for 13-24 months loan duration
logistic_duration_probability_13_24_text <- paste0(logistic_duration_probability_13_24, "%")
print(paste("Good credit risk class probability for 13-24 months loan duration:", logistic_duration_probability_13_24_text))


## 25-36 months loan duration
## Calculate the log-odds for 25-36 months loan duration
logistic_loan_duration_category_36_or <- coef(logistic_loan_duration_category_log_model)["(Intercept)"] + 
  coef(logistic_loan_duration_category_log_model)["loan_duration_category25-36"]

# Convert the log-odds to odds ratio
logistic_loan_duration_category_36_or <- exp(logistic_loan_duration_category_36_or)
print(paste("Odds Ratio for 25-36 months loan duration:", logistic_loan_duration_category_36_or))

# Convert odds ratio to probability
logistic_duration_probability_25_36 <- logistic_loan_duration_category_36_or / (1 + logistic_loan_duration_category_36_or)
logistic_duration_probability_25_36 <- round(logistic_duration_probability_25_36 * 100, 1)

# Good credit risk class probability for 25-36 months loan duration
logistic_duration_probability_25_36_text <- paste0(logistic_duration_probability_25_36, "%")
print(paste("Good credit risk class probability for 25-36 months loan duration:", logistic_duration_probability_25_36_text))


## 37+ months loan duration
## Calculate the log-odds for 37+ months loan duration
logistic_loan_duration_category_37plus_or <- coef(logistic_loan_duration_category_log_model)["(Intercept)"] + 
  coef(logistic_loan_duration_category_log_model)["loan_duration_category37+"]

# Convert the log-odds to odds ratio
logistic_loan_duration_category_37plus_or <- exp(logistic_loan_duration_category_37plus_or)
print(paste("Odds Ratio for 37+ months loan duration:", logistic_loan_duration_category_37plus_or))

# Convert odds ratio to probability
logistic_duration_probability_37plus <- logistic_loan_duration_category_37plus_or / (1 + logistic_loan_duration_category_37plus_or)
logistic_duration_probability_37plus <- round(logistic_duration_probability_37plus * 100, 1)

# Good credit risk class probability for 37+ months loan duration
logistic_duration_probability_37plus_text <- paste0(logistic_duration_probability_37plus, "%")
print(paste("Good credit risk class probability for 37+ months loan duration:", logistic_duration_probability_37plus_text))


# Calculate the weighted probability for loan duration below 25 months
weighted_probability_0_12_13_24 <- (logistic_duration_probability_0_12 + logistic_duration_probability_13_24) / 2
weighted_probability_0_12_13_24

# Calculate the weighted probability for loan duration above 24 months
weighted_probability_25_36_37plus <- (logistic_duration_probability_25_36 + logistic_duration_probability_37plus) / 2
weighted_probability_25_36_37plus

# Calculate the percentage difference between the probability for loan duration below 25 months and above 24 months
percentage_difference_probability_loan_duration_all <- (weighted_probability_0_12_13_24 - weighted_probability_25_36_37plus) / weighted_probability_25_36_37plus
percentage_difference_probability_loan_duration_all <- round(percentage_difference_probability_loan_duration_all * 100, 1)
percentage_difference_probability_loan_duration_all_text <- paste0(percentage_difference_probability_loan_duration_all, "%")
print(paste("The percentage difference between loan duration below 25 months and above 24 months:", percentage_difference_probability_loan_duration_all_text))


# Logistic Regression with 0-24 and 24+ months categories
logistic_loan_duration_log_model <- glm(credit_risk_class ~ loan_duration_category_less_than_24, data = pfda_df_Daryl, family = binomial)
logistic_loan_duration_log_model
summary(logistic_loan_duration_log_model)

## 0-24 months loan duration
# Calculate the odds ratio for 0-24 months loan duration
logistic_loan_duration_0_24_odds_ratio <- exp(coef(logistic_loan_duration_log_model)["(Intercept)"])
print(paste("Odds Ratio for 0-24 months loan duration:", logistic_loan_duration_0_24_odds_ratio))

# Good credit risk class probability for 0-24 months loan duration
logistic_duration_probability_0_24 <- logistic_loan_duration_0_24_odds_ratio / (1 + logistic_loan_duration_0_24_odds_ratio)
logistic_duration_probability_0_24 <- round(logistic_duration_probability_0_24 * 100, 1)
logistic_duration_probability_0_24_text <- paste0(logistic_duration_probability_0_24, "%")
print(paste("Good credit risk class probability for 0-24 months loan duration:", logistic_duration_probability_0_24_text))


## 24+ months loan duration
## Calculate the log-odds for 24+ months loan duration
logistic_loan_duration_24plus_odds_ratio <- coef(logistic_loan_duration_log_model)["(Intercept)"] + 
  coef(logistic_loan_duration_log_model)["loan_duration_category_less_than_2424+"]

# Convert the log-odds to odds ratio
logistic_loan_duration_24plus_odds_ratio <- exp(logistic_loan_duration_24plus_odds_ratio)
print(paste("Odds Ratio for 24+ months loan duration:", logistic_loan_duration_24plus_odds_ratio))

# Convert odds ratio to probability
logistic_duration_probability_24plus <- logistic_loan_duration_24plus_odds_ratio / (1 + logistic_loan_duration_24plus_odds_ratio)
logistic_duration_probability_24plus <- round(logistic_duration_probability_24plus * 100, 1)

# Good credit risk class probability for 24+ months loan duration
logistic_duration_probability_24plus_text <- paste0(logistic_duration_probability_24plus, "%")
print(paste("Good credit risk class probability for 24+ months loan duration:", logistic_duration_probability_24plus_text))


# Calculate the percentage difference between the probability for 0-24 and 24+ months loan duration
percentage_difference_probability_loan_duration <- (logistic_duration_probability_0_24 - logistic_duration_probability_24plus) / logistic_duration_probability_24plus
percentage_difference_probability_loan_duration <- round(percentage_difference_probability_loan_duration * 100, 1)
percentage_difference_probability_loan_duration_text <- paste0(percentage_difference_probability_loan_duration, "%")
print(paste("The percentage difference between 0-24 and 24+ months loan duration:", percentage_difference_probability_loan_duration_text))


# Extra Feature 1 - One-Sample Proportion Test

# Filter the dataset for loan_duration_months <= 24
duration_less_than_24 <- pfda_df_Daryl %>% filter(loan_duration_months <= 24)

# Count the number of good and total cases for 0-24 months loan duration
good_count <- sum(duration_less_than_24$credit_risk_class == "good")
total_count <- nrow(duration_less_than_24)
good_count
total_count

# Perform one-sample proportion test
prop_test <- prop.test(good_count, total_count, p = 0.5, alternative = "greater")
print(prop_test)


# Extra Feature 2 - Two-Sample Proportion Test

# Count the good and total cases for 0-24 and 24+ months loan duration
two_sample_loan_counts <- pfda_df_Daryl %>% 
  group_by(loan_duration_category_less_than_24) %>% 
  summarise(
    two_sample_good_count = sum(credit_risk_class == "good"),
    two_sample_total_count = n()
  )
View(two_sample_loan_counts)

# Extract the numbers (counts) for 0-24 and 24+ months loan duration
two_sample_0_24_good <- two_sample_loan_counts$two_sample_good_count[two_sample_loan_counts$loan_duration_category_less_than_24 == "0-24"]
two_sample_0_24_total <- two_sample_loan_counts$two_sample_total_count[two_sample_loan_counts$loan_duration_category_less_than_24 == "0-24"]
two_sample_24plus_good <- two_sample_loan_counts$two_sample_good_count[two_sample_loan_counts$loan_duration_category_less_than_24 == "24+"]
two_sample_24plus_total <- two_sample_loan_counts$two_sample_total_count[two_sample_loan_counts$loan_duration_category_less_than_24 == "24+"]

# Perform two-sample proportion test
two_sample_prop_test <- prop.test(
  x = c(two_sample_0_24_good, two_sample_24plus_good),
  n = c(two_sample_0_24_total, two_sample_24plus_total),
  alternative = "greater"
)
print(two_sample_prop_test)





#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#

## Choo Cheng Da TP068973

# Objective 1: To determine if personal status will impact the good or bad credit risk classification of a customer.

# Duplicate the dataset for individual analysis
pfda_df_David <- pfda_df

# Summarize the dataset
summary(pfda_df_David$personal_status)
table(pfda_df_David$credit_risk_class)

# Analyse the credit risk classification
pfda_df_David <- pfda_df_David[order(pfda_df_David$personal_status, pfda_df_David$credit_risk_class), ]

# categorize the personal status.
pfda_df_David$personal_status_group <- ifelse(grepl("single", pfda_df_David$personal_status), "Male single",
                                        ifelse(grepl("mar|vid", pfda_df_David$personal_status), "Male Married/Widowed",
                                               ifelse(grepl("div|sep", pfda_df_David$personal_status), "Male Divorced/Separated",
                                                      ifelse(grepl("female div/dep/mar", pfda_df_David$personal_status), "Female Divorced/Dependent/Married", 
                                                             "Unknown"))))
# Summarize data to calculate the total people of each category
bar_chart <- pfda_df_David %>%
  group_by(personal_status, credit_risk_class) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(total = sum(count),
         percentage = (count / total) * 100)

# Bar Chart
plot <- ggplot(pfda_df_David, aes(x = personal_status, fill = credit_risk_class)) +
  geom_bar(position = "dodge") +
  geom_text(stat = "count", 
            aes(label = after_stat(count)), 
            position = position_dodge(width = 0.9),  # Aligns text with bars
            vjust = -0.5) +  # Places text above bars
  labs(title = "Credit Risk Classification by Personal Status",
       x = "Personal Status",
       y = "Number of Customers",
       fill = "Credit Risk") +
  theme_minimal()

print(plot)

# Normalize data to ensure each pie chart sums to 100%
pie_chart_data <- pfda_df_David %>%
  group_by(personal_status, credit_risk_class) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(personal_status) %>%
  mutate(total = sum(count),
         percentage = (count / total) * 100)

# Pie Chart
pie_chart <- ggplot(pie_chart_data, aes(x = "", y = percentage, fill = credit_risk_class)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +  # Transform to pie chart
  facet_wrap(~ personal_status) +  # Create one pie chart per personal status
  labs(title = "Credit Risk Distribution by Personal Status",
       fill = "Credit Risk",
       y = "",
       x = "") +
  theme_void() +  # Removes axis and gridlines for clean pie chart
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5),  # Center text inside slices
            size = 3)

print(pie_chart)

#Data analysis
#Test the data about credit risk classification for each group using a logistic regression model

# Fit logistic regression model
log_model <- glm(credit_risk_class ~ personal_status, data = pfda_df_David, family = binomial)
summary(log_model)


#Interpretation of the Results and convert to probability
odds <- exp(0.2897)            # Convert log-odds to odds
probability <- odds / (1 + odds)  # Convert odds to probability
print(probability)



#Null Hypothesis (H₀):
#There is no significant difference in the distribution of credit risk classification (Good vs. Bad) for the "male single" group.

#Alternative Hypothesis (H₁):
#There is a significant difference in the distribution of credit risk classification (Good vs. Bad) for the "male single" group.

# Create a contingency table
contingency_table <- table(pfda_df_David$personal_status, pfda_df_David$credit_risk_class)

# Display the table
print(contingency_table)

# Observed frequencies for male single
observed <- c(1068, 1715)

# Assign labels for clarity
names(observed) <- c("Bad", "Good")


#set the expected data
# Total count of male single individuals
total <- sum(observed)

# Expected frequencies under null hypothesis (50%-50%)
expected <- c(total / 2, total / 2)

# Display the expected data
print(expected)

# Perform Chi-Square Test
chi_result <- chisq.test(observed, p = c(0.5, 0.5)) # 50%-50% expected proportions

# Display the test results
print(chi_result)

# Test with different expected proportions
chi_result <- chisq.test(observed, p = c(0.4, 0.6))

# Display the results
print(chi_result)

#Conclusion:
#X-squared = 150.42: The test statistic
#if probability = 0.57, this means 57% of "male single" individuals are classified as "Bad."
#p-value < 0.05: Reject the null hypothesis, meaning the distribution of "Good" and "Bad" credit risks is significantly different for "male single."




#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#

## Cheong Sheue Ling TP069004

# Objective 1: To investigate whether property magnitude impacts the credit risk classification of a customer.

# Duplicate the dataset for individual analysis
pfda_df_Cheong <- pfda_df

# Count occurrences of each combination
joint_counts <- table(pfda_df_Cheong$property_magnitude,pfda_df_Cheong$credit_risk_class )

View(joint_counts)

# Extract the counts for "good" class
good_class_counts <- joint_counts[, "good"]
View(good_class_counts)

# List out the property magnitude, each category frequency,total number of good class and probability.
# Extract the frequency of each property magnitude
category_frequencies <- rowSums(joint_counts)
View(category_frequencies)


# Extract the probability of each category
category_probabilities <- good_class_counts / category_frequencies
View(category_probabilities)


total_good_class <- sum(good_class_counts)
View(total_good_class)

# Combine all information into a data frame
property_info <- data.frame(
  PropertyMagnitude = names(category_frequencies),
  CategoryFrequency = category_frequencies,
  GoodClassFrequency = good_class_counts,
  ProbabilityGoodClass = category_probabilities
)
View(property_info)


# Create a pie chart of the probability
probability<-c(0.4666048,0.4473350,0.3612774,0.7322275)
labels <- c("car","life insurance", "no known property", "real estate")
pie(probability, labels)


# Hypothesis
# Alternative Hypothesis: Users whose property magnitude is real estate have higher probability to get good class
# Null Hypothesis: Users whose property magnitude is real estate do not have higher probability to get good class
pfda_df_Cheong$class_binary <- ifelse(pfda_df_Cheong$credit_risk_class == "good", 1, 0)
propertymagnitude_category_log_model <- glm(class_binary ~ property_magnitude, data = pfda_df_Cheong, family = binomial)
summary(propertymagnitude_category_log_model)


# Yes, the users whose property magnitude is real estate have the highest probability to get a good class
# No, the null hypothesis is rejected and the alternative hypothesis is accepted.





#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#

## Ho Shane Foong TP068496

# Objective 1: To determine whether the credit amount has effect on the individual's credit risk class

# ---- EDA for credit_amount ----
## duplicate raw_data
raw_data_credit_amount <- pfda_df 

## add category to difference with 0-10000 and 10000+
raw_data_credit_amount$cato_by_credit_less_than_10k <- cut(raw_data_credit_amount$credit_amount, breaks = c(0, 10000, Inf), labels = c("0-10000", "10000+"))

## filter those row where credit amount is less than 10000
raw_data_credit_amount_below_10k <- raw_data_credit_amount %>%
  filter(credit_amount <= 10000)

#### Histogram for credit amount ####
plot_credit_amount <- ggplot(raw_data_credit_amount, aes(x=credit_amount)) +
  scale_x_continuous(breaks = seq(0,16000,2500)) +
  geom_histogram(bins = 150 ,fill = "skyblue", color = "black") +
  labs(title = "Distribution of Credit Amount") + 
  theme_minimal()
plot_credit_amount

#### Boxplot credit amount by credit risk class ####
plot_credit_amount_risk_class <- ggplot(raw_data_credit_amount, aes(x = credit_risk_class, y=credit_amount)) +
  geom_boxplot(fill = 'skyblue')+
  labs(title = "Credit Amount by Credit Risk Class", x = "Credit Risk Class", y = "Credit Amount") +
  theme_minimal()
plot_credit_amount_risk_class 

#### Boxplot credit amount below 10k by credit risk class ####
plot_credit_amount_risk_class <- ggplot(raw_data_credit_amount_below_10k, aes(x = credit_risk_class, y=credit_amount)) +
  geom_boxplot(fill = 'skyblue')+
  labs(title = "Credit Amount by Credit Risk Class", x = "Credit Risk Class", y = "Credit Amount") +
  theme_minimal()
plot_credit_amount_risk_class 

# ---- Hypothesis Testing for credit_amount ----

#### one sample prop testing ####
prop_test_below_10k <- prop.test(sum(raw_data_credit_amount_below_10k$credit_risk_class == "good"), 
                                 nrow(raw_data_credit_amount_below_10k),
                                 p = 0.5,
                                 alternative = "greater")
prop_test_below_10k


#### two sample prop testing (t-test) ####
test_result_credit_amount <- t.test(credit_amount ~ credit_risk_class, data = raw_data_credit_amount)
test_result_credit_amount


#### Log regression with odd ratio and percentage ####
# Factorise credit risk class
raw_data_credit_amount$credit_risk_class <- factor(raw_data_credit_amount$credit_risk_class, levels = c("bad", "good"))

# Relevel the category by credit less than 10k
raw_data_credit_amount$cato_by_credit_less_than_10k <- relevel(raw_data_credit_amount$cato_by_credit_less_than_10k, ref = "10000+")

# Perform Log regression
glm_model_credit <- glm( credit_risk_class ~ cato_by_credit_less_than_10k, family = binomial, data = raw_data_credit_amount)
summary(glm_model_credit)

# Calculate odd ratio and probability
ratio_credit_amount <- exp(coef(glm_model_credit)["cato_by_credit_less_than_10k0-10000"])
ratio_credit_amount

probability_credit_less_than_10k <- ratio_credit_amount * 100 / (1 + ratio_credit_amount)
probability_credit_less_than_10k





#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#

## John Har Wey Jon TP068348

# Objective 1: To determine how age impacts the credit risk classification of a customer.

# Summarize the dataset
summary(pfda_df$age)
table(pfda_df$credit_risk_class)

# Analyse the credit risk classification differ between younger, middle_age, and older customers.
sort_age_pfda_df <- pfda_df[order(pfda_df$age, pfda_df$credit_risk_class), ]

# This code is to categorize the age to 5 intervals 
sort_age_pfda_df$age_group <- cut(sort_age_pfda_df$age, 
                                  breaks = c(18,34,54,75), 
                                  right = TRUE,
                                  labels = c("younger", "middle_age", "older"),
                                  include.lowest = TRUE)

sort_age_pfda_df

# Summarize data to get counts and percentages
bar_chart_summary <- sort_age_pfda_df %>%
  group_by(age_group, credit_risk_class) %>%
  summarise(count = n()) %>%
  mutate(total = sum(count),
         percentage = (count / total) * 100) 

# Bar Chart
bar_plot <- ggplot(bar_chart_summary, aes(x = age_group, y = count, fill = credit_risk_class, 
                                          text = paste("Age Group:", age_group, 
                                                       "<br>Credit Class:", credit_risk_class, 
                                                       "<br>Percentage:", round(percentage, 1), "%"))) +
  geom_bar(stat = "identity", position = "fill") + 
  labs(title = "Distribution of Credit Class by Age Group", 
       x = "Age Group", 
       y = "Percentage of Customers", 
       fill = "Credit Class") +
  scale_y_continuous(labels = scales::percent) + 
  theme_minimal()

bar_plot

# This extra feature is to help me easily analyse the bar chart accurately by hovering over the bar chart and it will list the percentage, number of counts, credit class, and the age group.
interactive_plot <- ggplotly(bar_plot, tooltip = "text")

interactive_plot

# Form Hypothesis

# Null Hypothesis:
## The age group of 55 - 75 does not have at least a probability of 60% of getting a good credit risk classification.

# Alternative Hypothesis: 
## The age group of 55 - 75 does have at least a probability of 60% of getting a good credit risk classification.


# Hypothesis Testing

# Logistic Regression Model
logistic_model <- glm(credit_risk_class ~ age_group, data = sort_age_pfda_df, family = binomial)

# Summary of the model
summary(logistic_model)





#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#

#                                                                  Group Hypothesis

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#

# Combined (Complex) Hypothesis:

# Null Hypothesis: Customers who are single males, aged between 55 and 75, own real estate, and apply for a loan of less than 10000 
# with a duration of less than or equal to 24 months do not have a significantly higher probability of being classified as a good credit 
# risk compared to customers who do not meet these criteria.

# Alternative Hypothesis: Customers who are single males, aged between 55 and 75, own real estate, and apply for a loan of less than 10000 
# with a duration of less than or equal to 24 months have higher probability of being classified as good credit risk compared to customers 
# who do not meet these criteria. 


# Accepting Criteria & Rejecting Criteria
## If p-value is less than or equal to 0.05, reject null hypothesis.
## If p-value is greater than 0.05, then fail to reject null hypothesis.



# Data Preparation

# Duplicate the dataset
pfda_df_Group <- pfda_df
View(pfda_df_Group)

# Categorize the loan_duration_months into 0-24 and 24+ months loan duration categories
pfda_df_Group$loan_duration_category <- cut(pfda_df_Group$loan_duration_months, breaks = c(0, 24, Inf), labels = c("0-24", "24+"))
summary(pfda_df_Group$loan_duration_category)
levels(pfda_df_Group$loan_duration_category)


# Categorize the credit_amount into 0-10000 and 10000+ credit amount categories
pfda_df_Group$credit_amount_category <- cut(pfda_df_Group$credit_amount, breaks = c(0, 10000, Inf), labels = c("0-10000", "10000+"))
summary(pfda_df_Group$credit_amount_category)
levels(pfda_df_Group$credit_amount_category)


# Categorize age into 3 categories: younger (18-34), middle_age (35-54), and older (55-75)
pfda_df_Group$age_group <- cut(pfda_df_Group$age, 
                                  breaks = c(18,34,54,75), 
                                  right = TRUE,
                                  labels = c("younger", "middle_age", "older"),
                                  include.lowest = TRUE)
summary(pfda_df_Group$age_group)
levels(pfda_df_Group$age_group)


# Hypothesis Testing

# Checking the levels of each variable
levels(pfda_df_Group$loan_duration_category)
levels(pfda_df_Group$personal_status)
levels(pfda_df_Group$property_magnitude)
levels(pfda_df_Group$credit_amount_category)
levels(pfda_df_Group$age_group)
levels(pfda_df_Group$credit_risk_class)


# Logistic Regression
logistic_model_Group <- glm(
  credit_risk_class ~ loan_duration_category * personal_status * property_magnitude * credit_amount_category * age_group,
  family = binomial(link = "logit"),
  data = pfda_df_Group
)
summary(logistic_model_Group)

# Save the logistic regression results into a .txt file
sink("logistic_model_Group.txt")
print(summary(logistic_model_Group))
sink()

# personal_statusmale single:property_magnitudereal estate:age_groupolder 2.988e+15  2.567e+07  116415959
# personal_statusmale single:property_magnitudereal estate:age_groupolder <2e-16 ***

