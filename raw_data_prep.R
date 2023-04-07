# load the ukb_extracted data 
library(dplyr)

data <- readRDS("ukb_extracted.rds")

colnames_data <- colnames(data)

colindex_filtered <- grep(".*\\.0.0$", colnames_data, fixed=FALSE)
#str(colindex_filtered)

data_filtered <- data[,colindex_filtered]

data_filtered <- data.frame(data_filtered)

dim(data_filtered)

### ICD codes for type 2 diabetes 

sum(data_filtered$ICD9_diag.0.0 == '2500')
# 13

sum(data_filtered$ICD9_diag.0.0 == '2504')
# 3

sum(data_filtered$ICD9_diag.0.0 == '2509')


sum(data_filtered$ICD10_diag.0.0 == 'E15')


sum(data_filtered$diabetes..0.0 == 0)


sum(data_filtered$diabetes..0.0 == 1, na.rm = T )

sum(data_filtered$diabetes..0.0 == 0, na.rm = T )

# Delete the "ICD9" and ICD10" from the data_filtered dataframe 
data_filtered <- subset(data_filtered, select = -ICD10_diag.0.0)
data_filtered <- subset(data_filtered, select = -ICD9_diag.0.0)


# calculate age from birth year 
library(lubridate)

data_filtered <- data_filtered %>%
  mutate(age = year(Sys.Date())-birth_year.0.0)
print(data_filtered)


# check the sum of missing values in each column of the data_filtered
missing_count <-colSums(is.na(data_filtered))

# print the missing count for each column 
print(missing_count)

# I want to delete other unnecessary columns in the dataset.
data_filtered <- subset(data_filtered, select = -birth_year.0.0)

# Count the number of non-missing values in "age_diabetes_diag.0.0" that are smaller than "Age_recr.0.0"
count <- sum(!is.na(data_filtered$age_diabetes_diag.0.0) &
               !is.na(data_filtered$Age_recr.0.0) &
               data_filtered$age_diabetes_diag.0.0 < data_filtered$Age_recr.0.0)

print(count)
# 23521 

count<-nrow(data_filtered)
print(count)
### 502401 


#######
# Subset the data to exclude rows where "age_diabetes_diag.0.0" is smaller than "Age_recr.0.0"
data_filtered_excluded <- data_filtered[!(data_filtered$age_diabetes_diag.0.0 < data_filtered$Age_recr.0.0 & !is.na(data_filtered$age_diabetes_diag.0.0) & !is.na(data_filtered$Age_recr.0.0)), ]

# Count the number of rows in the updated dataframe
count <- nrow(data_filtered_excluded)

# Print the count
print(count)


#####
# total diabetes case = 26395


### now total case: 26395 - 23521 = 2874 , data_filtered_excluded

# Count the number of missing values in "medications.0.0" column
missing_count <- sum(is.na(data_filtered_excluded$medications.0.0))

# Print the missing count
print(missing_count)
# 216640 

#exclude all the poeple that use medications 
data_no_meds <- subset (data_filtered_excluded, is.na(medications.0.0))

count <- nrow(data_no_meds)

print(count)

# all the cases 
sum(data_no_meds$diabetes..0.0 ==1,na.rm = T) 
### 1105 

# data now is called data_no_meds, exclude the unneccesary columns again
print(data_no_meds)

data_no_meds <- subset(data_no_meds, select = -medications.0.0)



# check the sum of missing values in each column of the data_no_meds
missing_count <-colSums(is.na(data_no_meds))

# print the missing count for each column 
print(missing_count)


# I don't actually needto deal with all the food_intake remove those columns as well. 

data_final <- data_no_meds %>%
  select(-sweets_intake.0.0, -banana_intake.0.0, -berry_intake.0.0, -cherry_intake.0.0, -pineapple_intake..0.0, 
         -education_score_england.0.0, -education_score_scotland.0.0, -education_score_wales.0.0)


# check sum of negative sumbers in each column 
neg_sums <- colSums(data_final < 0)

# print the result
print(neg_sums)
### they are greater than or equal to 0. 


missing_count <-colSums(is.na(data_final))

# print the missing count for each column 
print(missing_count)

####-----------------------------------------------

# I want to check how many NAs are in my medications.0.0 column when "diabetes..0.0" = 1
# Subset the data to include only rows where "diabetes..0.0" column equals 1
data_filtered_diabetes <- subset(data_filtered_excluded, diabetes..0.0 == 1)

# Count the number of NAs in the "medications.0.0" column of the subsetted dataframe
na_count <- sum(is.na(data_filtered_diabetes$medications.0.0))

# Print the number of NAs
print(na_count)


