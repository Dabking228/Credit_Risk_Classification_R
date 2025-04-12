# Load the libraries and packages
library(dplyr)
library(readr)
library(ggplot2)
library(DataExplorer)
library(missForest)

# Set the working directory
## Enter your own path
setwd("SET YOUR OWN PATH") #SET PATH
getwd()


# Import the dataset
## Enter your own file path
filePath = "SET YOUR OWN PATH/5. credit_risk_classification.csv" #SET PATH

## Read the CSV file into a dataframe
pfda_df = read_csv(filePath)
pfda_df

# Data Exploration
dim(pfda_df)
colnames(pfda_df)
spec(pfda_df)
sapply(pfda_df, class)
str(pfda_df)
summary(pfda_df)
View(pfda_df)


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



# Converting factors back to characters for categorical columns
pfda_df <- pfda_df %>% 
  mutate(across(where(is.factor), as.character))
str(pfda_df)
sapply(pfda_df, class)

# HERE, YOU CAN START YOUR OWN DATA ANALYSIS ALREADY!

# OPTIONAL! CAN COMMENT THIS PART IF YOU WANT
# Export the data frame containing the cleaned data to a new CSV file
# It will export the csv file to your working directory
current_directory = getwd()
export_path = file.path(current_directory, "(cleaned) credit_risk_classification.csv")
write.csv(pfda_df, export_path, row.names = FALSE)