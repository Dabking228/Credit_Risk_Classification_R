# Load the libraries and packages
library(dplyr)
library(readr)
library(ggplot2)
library(DataExplorer)
library(missForest)
library(VIM)
library(caret)
library(caTools)
library(randomForest)
library(vcd)

# Set the working directory
## Enter your own path
setwd("C:/APU/Degree/Semester 1/Programming for Data Analysis/Assignment/PFDA Assignment")
getwd()


# Import the dataset
## Enter your own file path
filePath = "C:/APU/Degree/Semester 1/Programming for Data Analysis/Assignment/PFDA Assignment/(cleaned) credit_risk_classification.csv"

## Read the CSV file into a dataframe
pfda_df = read.csv(filePath)

# Convert the data type of all categorical columns from character to factor
pfda_df <- pfda_df %>% 
  mutate(across(where(is.character), as.factor))
str(pfda_df)
sapply(pfda_df, class)

# Duplicate the dataset for individual analysis
pfda_df_Daryl <- pfda_df
View(pfda_df_Daryl)

# Data Analysis
#---------------------------------------------------------------------Daryl Sim Wei Shern TP068964------------------------------------------------------------------#

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









# # Objective 2: To determine whether the credit history impacts the credit risk classification of a customer.
# # Independent Variable: credit_history_status (previously known as credit_history)
# 
# 
# # Calculate the number of rows for each credit_history_status
# credit_history_count <- pfda_df_Daryl %>% 
#   group_by(credit_history_status) %>% 
#   count()
# View(credit_history_count)
# 
# # Visualize distribution of loan_duration_category 
# credit_history_status_distribution <-
#   ggplot(credit_history_count, aes(x = credit_history_status, y = n, fill = credit_history_status)) +
#   geom_bar(stat = "identity", position = position_dodge(width = 0.9), fill = "skyblue") +
#   geom_text(aes(label = n),
#             position = position_dodge(width = 0.9),
#             vjust = -0.5,
#             size = 3.5) +
#   labs(title = "Distribution of Credit History Status", x = "Credit History Status", y = "Count") +
#   theme_minimal()
# print(credit_history_status_distribution)
# 
# # Save the plot
# ggsave("Distribution of Credit History Status.png", credit_history_status_distribution, width = 12, height = 8, dpi = 300, bg="white")
# 
# 
# # Calculate the number and percentage of good and bad credit_risk_class in each credit_history_status
# proportion_credit_history_status <- pfda_df_Daryl %>% 
#   group_by(credit_history_status, credit_risk_class) %>% 
#   summarise(count = n(), .groups = 'drop') %>% 
#   group_by(credit_history_status) %>% 
#   mutate(percent = count / sum(count) * 100)
# proportion_credit_history_status
# 
# # Visualize distribution of loan_duration_category by credit_risk_class
# credit_history_status_bar <- 
#   ggplot(proportion_credit_history_status, aes(x = credit_history_status, y = count, fill = credit_risk_class)) +
#   geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
#   geom_text(aes(label = count),
#             position = position_dodge(width = 0.9),
#             vjust = -0.5,
#             size = 3.5) +
#   labs(title = "Distribution of Credit History Status by Credit Risk Class", x = "Credit History Status", y = "Count") +
#   theme_minimal()
# print(credit_history_status_bar)
# 
# # Save the plot
# ggsave("Distribution of Loan Duration Categories.png", loan_duration_category_bar, width = 12, height = 8, dpi = 300, bg="white")
# 
# # Visualize the percentage of good and bad credit_risk_class in each loan_duration_category
# proportion_credit_history_status_bar <- 
#   ggplot(proportion_credit_history_status, aes(x = credit_history_status, y = percent, fill = credit_risk_class)) + 
#   geom_bar(stat = "identity", position = position_dodge(width = 0.9)) + 
#   geom_text(aes(label = sprintf("%.1f%%", percent)),
#             position = position_dodge(width = 0.9),
#             vjust = -0.5,
#             size = 3.5) +
#   labs(title = "Percentage Distribution of Credit Risk Class by Credit History Status", 
#        x = "Credit History Status", 
#        y = "Percentage (%)") + 
#   scale_y_continuous(labels = scales::percent_format(scale = 1)) + 
#   theme_minimal() + 
#   theme(legend.position = "top")
# print(proportion_credit_history_status_bar)


























































