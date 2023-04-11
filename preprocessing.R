# loading necessary packages 
library(magrittr)
library(dplyr)
library(tidyverse)
library(tableone)
library(caret) #splitting data

df<-read_csv('data_final.csv')

sum(df$diabetes..0.0 == 0, na.rm = T)
sum(df$diabetes..0.0 == 1, na.rm = T)

# Check for missing values in the 'column_name' column
sum(is.na(df$diabetes..0.0))

# Subset the data frame to keep only the rows without missing values in the diabetes..0.0 column
df <- df[complete.cases(df$diabetes..0.0), ]


# Check for non-numeric columns and convert to numeric
non_numeric_cols <- sapply(df, function(x) !is.numeric(x))
df[, non_numeric_cols] <- lapply(df[, non_numeric_cols], as.numeric)


# splitting training and test data
# ----- 75/25 splitting (caret package)
# Create a random stratified split of the data, keeping NAs as a separate category
set.seed(123) # for reproducibility
split_index <- createDataPartition(ifelse(is.na(df$diabetes..0.0), "NA", "Non-NA"), p = 0.75, list = FALSE)

# Split the data into training and test sets based on the split index
train_data <- df[split_index, ]
test_data <- df[-split_index, ]

write.csv(train_data, "train_df.csv")
write.csv(test_data, "test_df.csv")







# Summary Statistics ------------------------------------------------------
case_df <- subset(df, df$diabetes..0.0 == 1, na.rm = T)
summary(case_df)

control_df <- subset(df, df$diabetes..0.0 == 0, na.rm = T)
summary(control_df)

str(case_df)



