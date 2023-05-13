#' Author: Daniel Evenschor
#' Date: April 7,2023
#' Purpose: A2 - Predicting hospital readmissions

# set my working directory
setwd("C:/Users/danie/OneDrive - Hult Students/Hult_Visualizing-Analyzing-Data-with-R/personalFiles")

# Libraries
library(ggplot2) 
library(dplyr)
library(MLmetrics)
library(tidyr)
library(RColorBrewer)
library(moments) # for kurtosis and Skewness
library(ROSE) # oversampling
library(caret) # k-fold crossval and modeling
library(tibble)



# Read all of the files
HospitalTest <- read.csv("https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A2_Hospital_Readmission/caseData/diabetesHospitalInfoTest.csv")
HospitalTrain <- read.csv("https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A2_Hospital_Readmission/caseData/diabetesHospitalInfoTrain.csv")
MedsTest <- read.csv("https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A2_Hospital_Readmission/caseData/diabetesMedsTest.csv")
MedsTrain <- read.csv("https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A2_Hospital_Readmission/caseData/diabetesMedsTrain.csv")
PatientTest <- read.csv("https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A2_Hospital_Readmission/caseData/diabetesPatientTest.csv")
PatientTrain <- read.csv("https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A2_Hospital_Readmission/caseData/diabetesPatientTrain.csv")

# Basic EDA before joining to get information about each individual data set
# Structure gives insights on the datatypes and general content of each column
# Just want to have a look at which columns come from which data set a reference
str(HospitalTest)
str(HospitalTrain)
str(MedsTest)
str(MedsTrain)
str(PatientTest)
str(PatientTrain)
# Takeaway: Empty values (N/a) might be represented by "?" for these datasets

OriginalTest <- HospitalTest %>%
  left_join(MedsTest, by = "tmpID") %>%
  left_join(PatientTest, by = "tmpID") 

OriginalTrain <- HospitalTrain %>%
  left_join(MedsTrain, by = "tmpID") %>%
  left_join(PatientTrain, by = "tmpID")

# I want to combine both Train and Test so i can look at only one dataframe
# For that i need to create a new column that allows me to identify which row belongs to which df
# Create column "type" and set it to train or test respectively so i can identify them later
OriginalTrain$type <- "train"
OriginalTest$type <- "test"
# Now i can combine them to work with the data way easier 
DF <- rbind(OriginalTrain, OriginalTest)
############################################################################################################

names(DF) # Get all column names
str(DF) # Looking at datatypes

colSums(is.na(DF))# No nulls are shown because they are represented by "?" or "" so i will replace
# Replace "?" and "" and "Not Available" and "Not Mapped" with NA
DF[DF == "?"] <- NA
DF[DF == ""] <- NA
DF[DF == "Not Available"] <- NA
colSums(is.na(DF)) # Now i can properly see Nulls 
sum(colSums(is.na(DF)))
#Look at all the column values
table(DF$admission_type_id, useNA = "ifany")
table(DF$discharge_disposition_id, useNA = "ifany")
table(DF$admission_source_id, useNA = "ifany")
table(DF$time_in_hospital, useNA = "ifany")
table(DF$medical_specialty, useNA = "ifany")
table(DF$num_lab_procedures, useNA = "ifany")
table(DF$num_procedures, useNA = "ifany")
table(DF$num_medications, useNA = "ifany")
table(DF$number_outpatient, useNA = "ifany")
table(DF$number_emergency, useNA = "ifany")
table(DF$number_inpatient, useNA = "ifany")
table(DF$number_diagnoses, useNA = "ifany")
table(DF$diag_1_desc, useNA = "ifany")
table(DF$diag_2_desc, useNA = "ifany")
table(DF$diag_3_desc, useNA = "ifany")
table(DF$max_glu_serum, useNA = "ifany")
table(DF$A1Cresult, useNA = "ifany")
table(DF$metformin, useNA = "ifany")
table(DF$repaglinide, useNA = "ifany")
table(DF$nateglinide, useNA = "ifany")
table(DF$chlorpropamide, useNA = "ifany")
table(DF$glimepiride, useNA = "ifany")
table(DF$acetohexamide, useNA = "ifany")
table(DF$glipizide, useNA = "ifany")
table(DF$glyburide, useNA = "ifany")
table(DF$tolbutamide, useNA = "ifany")
table(DF$pioglitazone, useNA = "ifany")
table(DF$rosiglitazone, useNA = "ifany")
table(DF$acarbose, useNA = "ifany")
table(DF$miglitol, useNA = "ifany")
table(DF$troglitazone, useNA = "ifany")
table(DF$tolazamide, useNA = "ifany")
table(DF$examide, useNA = "ifany")
table(DF$citoglipton, useNA = "ifany")
table(DF$insulin, useNA = "ifany")
table(DF$change, useNA = "ifany")
table(DF$diabetesMed, useNA = "ifany")
table(DF$race, useNA = "ifany")
table(DF$gender, useNA = "ifany")
table(DF$age, useNA = "ifany")
table(DF$wgt, useNA = "ifany")
table(DF$payer_code, useNA = "ifany")
table(DF$readmitted_y, useNA = "ifany")


############################################################################################################
# COLUMN DESCRIPTION AFTER EXTENSIVE EXPLORATION:

# tmpID: Unique Identifier

# admission_type_id: 6 distinct values: Elective, Emergency, Newborn, Not available, Not Mapped and Urgent

# discharge_disposition_id: String values indicating type of discharge:discharged to home, expired etc. 

# admission_source_id: String values indicating source of admission:  physician referral, Clinic referral, Emergency Room etc.

# time_in_hospital: Integer value from 1-14, probably the duration of hospital stay in DAYS

# medical_specialty: String values, Probably the field that the patient was admitted by or to be treated in  

# num_lab_procedures: Integer from 1-120 - represents the count of how many lab procedures were done 

# num_procedures: Integer from 0-6 - represents the count of how many procedures (other than lab procedures)

# num_medications: Integer from 1-81 - represents the count of how many medication the patient was put under or is under

# number_outpatient: Integer from 0-36 outpatient is probably visitors according to google so amount of visitors

# number_emergency: Integer from 0-42 Probably the amount of emergency situations during the hospital stay of that patient

# number_inpatient: Integer from 0-10 Probably the amount of previous admissions ? 

# number_diagnoses: Integer from 1-9 Representing the number of diagnoses during their stay

# diag_1_desc: Long string that indicate the diagnosis of the patient 
# diag_2_desc: Long string that indicate the second diagnosis of the patient completely different to 1st 
# diag_3_desc: Long string that indicate the third diagnosis of the patient completely different to 1st & 2nd

# max_glu_serum: 4 unique values: >200, >300, None or Norm  - Probably test results 

# A1Cresult: 4 unique values: >7, >8, None or Norm - Some type of test reuslts

# metformin: 4 unique values Down, No, Steady, Up 
# repaglinide: 4 unique values Down, No, Steady, Up 
# nateglinide: 4 unique values Down, No, Steady, Up 
# glimepiride: 4 unique values Down, No, Steady, Up 
# glipizide: 4 unique values Down, No, Steady, Up 
# glyburide: 4 unique values Down, No, Steady, Up 
# pioglitazone: 4 unique values Down, No, Steady, Up 
# rosiglitazone: 4 unique values Down, No, Steady, Up 
# miglitol: 4 unique values Down, No, Steady, Up 
# insulin:  4 unique values Down, No, Steady, Up 

# chlorpropamide: 3 unique values No, Steady, Up 
# acarbose: 3 unique values No, Steady, Up 

# tolbutamide: 2 unique values No, Steady
# tolazamide:  2 unique values No, Steady

# acetohexamide: 1 unique value: No 
# troglitazone: 1 unique value: No 
# examide: 1 unique value: No 
# citoglipton: 1 unique value: No 

# change: 2 unique string values "CH" or "No" probably represent Change (something changed) or No 

# diabetesMed: Yes or No probably whether or not the patient has diabetes medication 

# race: String values: AfricanAmerican, Asian, Caucasian, Hispanic or Other 

# gender: Male or Female 

# age: Age from 0 to 100 

# wgt: probably weight of patient from 56-378 i hope this is not in kilos (America LOL) 

# payer_code: Different abbreviations, probably insurance codes of who paid the bill 

# type: Identifier column for myself 

# readmitted_y: TRUE or FALSE if readmitted within 30 days 

###########################################################################################################
# Take a look at the nulls to decide which columns to drop after understanding more about the data
colSums(is.na(DF)) 

DF <- DF %>%
  select(-payer_code, # Too many null values impossible to impute correctly
         -medical_specialty, # too many null values 
         -acetohexamide,# Only one value -> no insight
         -troglitazone, # Only one value -> no insight
         -examide, # Only one value -> no insight
         -citoglipton, # Only one value -> no insight
         -diag_1_desc, # Too many unique strings, cant group by anything in common
         -diag_2_desc,# Too many unique strings, cant group by anything in common 
         -diag_3_desc # Too many unique strings, cant group by anything in common also missing values
         )

colSums(is.na(DF)) 

# Still missing values in Race 
# fill missing values with "Other"
DF <- DF %>%
  mutate(race = ifelse(is.na(race), "Other", race))

# Replace nulls in disposition with "Not Mapped" 
# There is no other way that i can see to fill in those nulls 
DF <- DF %>%
  mutate(discharge_disposition_id = ifelse(is.na(discharge_disposition_id), "Not Mapped", discharge_disposition_id))

# Since we are predicting readmissions it doesn't make sense keeping rows of dead patients
# Remove "Expired" rows as well as the rowns containing "Hospice" since we assume that they will expire soon
# Remove rows using grepl
DF <- DF %>%
  filter(!grepl("Expired", discharge_disposition_id) & !grepl("Hospice", discharge_disposition_id))


colSums(is.na(DF)) 
# Only two columns left with missing values 
# admission_type_id and admission_source_id
# create a seperate df to look at these columns 
filtered_DF <- DF[, c("admission_type_id", "admission_source_id")]
# It seems like there might be a relationship between the two columns admission type and admission source
# In order to visualize this i will make a mosaic plot based on a contingency table 
# But first I will simplify the values
# Urgent and Emergency is the same for me 
DF <- DF %>%
  mutate(admission_type_id = ifelse(admission_type_id == "Urgent", "Emergency", admission_type_id))
# There is only one record of Newborn, i decided to remove that row 
DF <- DF %>%
  filter(!grepl("Newborn", admission_type_id))
# There is only one row with "Court/Law Enforcement" -> Remove
DF <- DF %>%
  filter(!grepl("Court/Law Enforcement", admission_source_id))
# combine all of the "transfer" values into one 
DF <- DF %>%
  mutate(admission_source_id = ifelse(grepl("Transfer", admission_source_id),
                                      "Transfer", admission_source_id))
# combine all "referrals" into one 
DF <- DF %>%
  mutate(admission_source_id = ifelse(grepl("Referral", admission_source_id),
                                      "Referral", admission_source_id))

# Now the values for both columns are cleaned up we can continue the plan to create a mosaic plot 
# First I have to create a contingency table
contingency_table <- table(DF$admission_type_id, DF$admission_source_id)
# Now I need to calculate the number of cells in the contingency table
num_cells <- length(contingency_table)
# My color palette with as many colors as there are "cells"
colors <- brewer.pal(num_cells, "Blues")

# Now i can create the mosaic plot with unique colors for each cell
mosaicplot(contingency_table, main = "Relationship between Admission Type and Admission Source",
           xlab = "Admission Type ID", ylab = "Admission Source ID",
           col = colors, las = 2)

# Will fill in missing values based on the following Insights
# Insights: 
# Emergency Room admission source usually -> Emergency for type ID and vice versa
# Referell is usually -> Elective Type ID and vice versa 


colSums(is.na(DF)) 
# 1291 na for admission type and 916 for admission source
DF <- DF %>% 
  mutate(admission_type_id = ifelse((admission_source_id == "Emergency Room") & is.na(admission_type_id),
                                    "Emergency", admission_type_id),
         admission_source_id = ifelse((admission_type_id == "Emergency") & is.na(admission_source_id),
                                      "Emergency Room", admission_source_id))
DF <- DF %>%
  mutate(admission_source_id = ifelse((admission_type_id == "Elective") & is.na(admission_source_id),
                               "Referral", admission_source_id),
         admission_type_id = ifelse((admission_source_id == "Referral") & is.na(admission_type_id),
                                    "Elective", admission_type_id))


colSums(is.na(DF)) 
# 797 admission type and 782 admission source 
        
# If both admission_type_id and admission_source_id are NA replace with "Not Mapped"
DF <- DF %>%
  mutate(both_na = is.na(admission_type_id) & is.na(admission_source_id)) %>%
  mutate(admission_type_id = ifelse(both_na, replace_na(admission_type_id, "Not Mapped"), admission_type_id),
         admission_source_id = ifelse(both_na, replace_na(admission_source_id, "Not Mapped"), admission_source_id)) %>%
  select(-both_na)

colSums(is.na(DF)) 
# 15 admission type and 0 admission source left 

# Filter the DF dataframe to show only the rows where admission_type_id is still NA
filtered_DF <- DF %>%
  filter(is.na(admission_type_id))
# All of those rows are "Transfer" for Source ID-> according to mosaic plot most of those are emergencies
# Fill in the last 15 values based on that 
DF <- DF %>%
  mutate(admission_type_id = ifelse((admission_source_id == "Transfer") & is.na(admission_type_id)
                                    ,"Emergency", admission_type_id))
         
colSums(is.na(DF)) 
# No more missing values! 

############################################################################################################

# Its time for some Feature engineering and simplification

table(DF$discharge_disposition_id)
# Combine all of the values that contain "transferred to home" to "Transferred to Home with Assistance"
# Combine all of the values that contain "transferred" to "Transferred" 
# Left AMA should mean Against Medical Advice so that seems relevant even with just 45 mentions
DF <- DF %>%
  mutate(discharge_disposition_id = ifelse(grepl("transferred to home", discharge_disposition_id, ignore.case = TRUE),
                                           "Transferred to Home with Assistance",
                                           ifelse(grepl("transferred", discharge_disposition_id, ignore.case = TRUE),
                                                  "Transferred",
                                                  discharge_disposition_id)))
# Admitted as an inpatient to this hospital doesnt make sense for me -> Drop the rows (2)
DF <- DF %>%
  filter(!grepl("Admitted as an inpatient to this hospital", discharge_disposition_id))
table(DF$discharge_disposition_id) # way better

table(DF$number_outpatient) # Since I defined this column as the count of visitors 
# I made the decision to turn this into a binary column 
# rename to had_visitor and keep all 0, turn all values >=1 into 1
DF <- DF %>%
  mutate(had_visitor = ifelse(number_outpatient >= 1, 1, 0))
# Same for number_emergency, had_emergency 1 or 0 for whether or not that patient had an emergency during his stay
DF <- DF %>%
  mutate(had_emergency = ifelse(number_emergency >= 1, 1, 0))
# Same for number_inpatient, previous_patient 1 or 0 if that patient has already been admitted once before
DF <- DF %>%
  mutate(previous_patient = ifelse(number_inpatient >= 1, 1, 0))
# Change the gender column from Female / Male to 1 / 0 and rename to is_male 
DF <- DF %>%
  mutate(is_male = ifelse(gender == "Male", 1, 0))

# Removing the old columns 
DF <- DF %>%
  select(-number_outpatient, # new column: had_visitor
         -number_emergency, # new column: had_emergency
         -number_inpatient,# new column: previous_patient
         -gender # new column: is_male
         )

table(DF$change)# Turning this column into 1 and 0 
# 1 = "CH" stands for changed = True 
# 0 = "No" stands for changed = False
DF <- DF %>%
  mutate(change = ifelse(change == "Ch", 1, 0))

table(DF$diabetesMed)# Same for this column 
# 1 = "Yes"
# 0 = "No" 
DF <- DF %>%
  mutate(diabetesMed = ifelse(diabetesMed == "Yes", 1, 0))

table(DF$readmitted_y)# Same for this column
# 1 = TRUE
# 0 = FALSE
DF <- DF %>%
  mutate(readmitted_y = ifelse(readmitted_y == "TRUE", 1, 0))



table(DF$max_glu_serum)
table(DF$A1Cresult)
# Both of these columns seem to have similar values
# This is most likely the test results for glucose test and a1c for bloodsugar levels 
# I decided to simplify these columns into 1/0 tested or not tested 
# Rename max_glo_serum to tested_glucose -> 1 = "Norm", ">200", ">300" ; 0 = "None"
# Rename A1Cresult to tested_A1C -> = 1= "Norm", ">7", ">8" ; 0 = "None"
DF <- DF %>%
  mutate(max_glu_serum = ifelse(max_glu_serum != "None", 1, 0),
         A1Cresult = ifelse(A1Cresult != "None", 1, 0)) %>%
  rename(tested_glucose = max_glu_serum,
         tested_A1C = A1Cresult)



# For the medication columns 
medcols <- c("metformin", "repaglinide", "nateglinide", "chlorpropamide", "glimepiride",
             "glipizide", "glyburide", "tolbutamide", "pioglitazone", "rosiglitazone",
             "acarbose", "miglitol", "tolazamide")#, "insulin")
# It turns out after working with the dataset for a while that the following is true:
# The change column is 1 if one of these columns are Up/Down/Steady and 0 if all of these columns are No
# The only exception is insulin, if insulin remained "steady" and all others are "No" then change is also 0 

# Therefore i decided to simplify the medcols representing medication columns into 1 and 0 
# The values inside probably indicate whether or not the medication was adjusted 
# Thats the only thing that makes sense since the relationship with change exists

# 1= "Up", "Down", "Steady
# 0 = "No" 
# Replace as described
DF[medcols] <- lapply(DF[medcols], function(x) ifelse(x %in% c("Up", "Down", "Steady"), 1, 0))
# Exception is insulin where 
# 1 = "Up", "Down" 
# 0 = "No", "Steady"
DF$insulin <- ifelse(DF$insulin %in% c("Up", "Down"), 1, 0)

str(DF)# Looking at datatypes
# I need to convert some datatypes

# Convert binary columns to logical
binary_columns <- c("tested_glucose", "tested_A1C", "metformin", "repaglinide", "nateglinide", "chlorpropamide",
                    "glimepiride", "glipizide", "glyburide", "tolbutamide", "pioglitazone", "rosiglitazone",
                    "acarbose", "miglitol", "tolazamide", "insulin", "change", "diabetesMed", "readmitted_y",
                    "had_visitor", "had_emergency", "previous_patient", "is_male")

DF[binary_columns] <- lapply(DF[binary_columns], as.logical)

# Convert categorical columns to factors
categorical_columns <- c("admission_type_id", "discharge_disposition_id", "admission_source_id", "race", "type")

DF[categorical_columns] <- lapply(DF[categorical_columns], as.factor)

# Check the updated data types
str(DF)

###########################################################################################################

# Lets get some insights before I start modelling
# Also get an understanding of the Y variable behaviour 
# Understand which columns I want to log-transform 

table(DF$readmitted_y)

ggplot(DF, aes(x = readmitted_y, fill = readmitted_y)) +
  geom_bar() +
  scale_fill_manual(values = c("darkgreen", "darkred")) +
  labs(x = "Readmission", y = "Count", fill = "Readmission") +
  ggtitle("Distribution of Readmission") +
  theme_minimal()

# Insight: The prediction variable is slightly imbalanced, might have to balance it out later to avoid overfitting for one side

# Lets look at the relationship between some of the most interesting columns and the prediction variable
# Starting with Admission Type 
DF_summarized <- DF %>%
  group_by(admission_type_id, readmitted_y) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count))

ggplot(DF_summarized, aes(x = admission_type_id, y = count, fill = as.factor(readmitted_y))) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = c("darkgreen", "darkred"), labels = c("No", "Yes")) +
  labs(x = "Admission Type", y = "Count", fill = "Readmission") +
  ggtitle("Relationship between Readmission and Admission Type") +
  geom_text(aes(label = paste0(round(percentage * 100, 1), "%")), position = position_stack(vjust = 0.5)) +
  theme_minimal()
# Insight: approximately 40% for all of the admissiontypes -> no clear trend


# Same graph for discharge disp
DF_summarized_discharge <- DF %>%
  group_by(discharge_disposition_id, readmitted_y) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count))

ggplot(DF_summarized_discharge, aes(x = discharge_disposition_id, y = count, fill = as.factor(readmitted_y))) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = c("darkgreen", "darkred"), labels = c("No", "Yes")) +
  labs(x = "Discharge Disposition", y = "Count", fill = "Readmission") +
  ggtitle("Relationship between Readmission and Discharge Disposition") +
  geom_text(aes(label = paste0(round(percentage * 100, 1), "%")), position = position_stack(vjust = 0.5)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Insight: 
# It seems that patients who are being discharged to home have a ~ 10% higher probability to be readmitted
# Patients that have not been Mapped are less likely to be readmitted (10% less)
# Patients that left AMA (Against Medical Advice) have approximately 50/50 % to be readmitted or not 


# Same for admission source
DF_summarized_admission_source <- DF %>%
  group_by(admission_source_id, readmitted_y) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count))

ggplot(DF_summarized_admission_source, aes(x = admission_source_id, y = count, fill = as.factor(readmitted_y))) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = c("darkgreen", "darkred"), labels = c("No", "Yes")) +
  labs(x = "Admission Source", y = "Count", fill = "Readmission") +
  ggtitle("Relationship between Readmission and Admission Source") +
  geom_text(aes(label = paste0(round(percentage * 100, 1), "%")), position = position_stack(vjust = 0.5)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Insights: 
# Patients that have been admitted to the hospital by referral have a lower readmission rate than ones coming due to an emergency
# Patients that have been transferred to the hospital from another medical facility have he lowest readmission rate

# Same for Race
DF_summarized_race <- DF %>%
  group_by(race, readmitted_y) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count))

ggplot(DF_summarized_race, aes(x = race, y = count, fill = as.factor(readmitted_y))) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = c("darkgreen", "darkred"), labels = c("No", "Yes")) +
  labs(x = "Race", y = "Count", fill = "Readmission") +
  ggtitle("Relationship between Readmission and Race") +
  geom_text(aes(label = paste0(round(percentage * 100, 1), "%")), position = position_stack(vjust = 0.5)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Insights: 
# Hard to read Asian: 65% NO and 35% YES
# Caucasians seem to have the highest readmission rate 
# Other races than AAmerican, Asian, Hispanic and caucasian have the lowest 


# Time in Hospital seems like a good indicator to look at 
# Calculate percentage of readmission for each time in the hospital
readmission_perc <- DF %>%
  group_by(time_in_hospital, readmitted_y) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  filter(readmitted_y == 1)

# Create line plot
ggplot(readmission_perc, aes(x = time_in_hospital, y = percentage)) +
  geom_line(color = "darkred") +
  labs(x = "Time in Hospital (days)", y = "Readmission Percentage") +
  ggtitle("Relationship between Time in Hospital and Readmission") +
  theme_minimal()
# Insight: 
# Time in hospital could be a significant indicator for whether or not a patient will be readmitted
# The highest % is if the patient was discharged after 10 days
# The range iwth the highest readmissions is between 5 and 13 days 


# Looking at age in the same way
# Calculate the percentage of readmissions for each age bin
readmission_age_perc <- DF %>%
  mutate(age_bin = cut(age, seq(0, 100, by = 10), include.lowest = TRUE, right = FALSE)) %>%
  group_by(age_bin, readmitted_y) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  filter(readmitted_y == 1)

# Create the line plot 
ggplot(readmission_age_perc, aes(x = age_bin, y = percentage)) +
  geom_line(color = "darkred", group = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "Age Bin", y = "Readmission Percentage") +
  ggtitle("Relationship between Age Bin and Readmission") +
  theme_minimal()
# Insight: 
# It makes sense that the older the patient the higher the rate of readmission 
# Between 20 and 30 the rate stays almost the same 


# Same for Weight 
# Calculate the percentage of readmissions for each weight bin
readmission_wgt_perc <- DF %>%
  mutate(wgt_bin = cut(wgt, seq(0, 400, by = 40), include.lowest = TRUE, right = FALSE)) %>%
  group_by(wgt_bin, readmitted_y) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  filter(readmitted_y == 1)

# Create the line plot with a trendline
ggplot(readmission_wgt_perc, aes(x = wgt_bin, y = percentage)) +
  geom_line(color = "darkred", group = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "Weight Bin", y = "Readmission Percentage") +
  ggtitle("Relationship between Weight Bin and Readmission") +
  theme_minimal()
# Insight: 
# People that have "Healthy weights - between 120-240 have a lower rate of readmission than overweight or underweight 


########################################################################################################

# Check the integer columns for kurtosis and skewness 
# Select the integer columns
integer_columns <- DF %>% select_if(is.integer)

# Calculate the kurtosis and skewness
kurtosis_skewness <- data.frame(
  column = colnames(integer_columns),
  kurtosis = sapply(integer_columns, kurtosis),
  skewness = sapply(integer_columns, skewness)
)

kurtosis_skewness 
# Looking at the skewness of these columns 
# I consider a skewness over -1 or 1 to be in need for a logtransform 
# The columns that fall into this threshhold are: 
# time_in_hospital, num_procedures and num_medications
# Now i apply logtransform and repeath the step above to see the change 
# Apply log transformation to the selected columns
DFP <- DF

DFP$time_in_hospital <- log1p(DFP$time_in_hospital)
DFP$num_procedures <- log1p(DFP$num_procedures)
DFP$num_medications <- log1p(DFP$num_medications)

# Calculate kurtosis and skewness for the transformed columns
kurtosis_skewness_transformed <- data.frame(
  column = c("time_in_hospital", "num_procedures", "num_medications"),
  kurtosis = c(kurtosis(DFP$time_in_hospital), kurtosis(DFP$num_procedures), kurtosis(DFP$num_medications)),
  skewness = c(skewness(DFP$time_in_hospital), skewness(DFP$num_procedures), skewness(DFP$num_medications))
)

kurtosis_skewness_transformed
# Logtransform achieved my objective to get them all below a skewness of -1 or 1 

# I made a mistake earlier by converting columns to the logical datatype 
# I didnt think of modelling and that i might run into issues with some models that cant use logical datatype
# I fix this here 
# Identify logical columns and convert them to numeric
logical_cols <- sapply(DFP, is.logical)
DFP[, logical_cols] <- sapply(DFP[, logical_cols], as.numeric)

DFP$readmitted_y <- factor(DFP$readmitted_y, levels = c(0, 1))

# Now it is time to one-hot encode my factor columns 
DF_one_hot <- model.matrix(~ . -1, data = DFP[, c("admission_type_id", "discharge_disposition_id",
                                                 "admission_source_id", "race")])

# Convert the one-hot encoded matrix to a dataframe
DF_one_hot <- as.data.frame(DF_one_hot)

# Combine the one-hot encoded columns with the original dataframe (without the original factor columns)
DFO <- cbind(DFP[, !(names(DF) %in% c("admission_type_id", "discharge_disposition_id",
                                    "admission_source_id", "race"))], DF_one_hot)
names(DFO)# There are spaces in some of the column names 
# Removing them: 
# Replacing spaces in the column names with underscores 
names(DFO) <- gsub(" ", "_", names(DFO))
names(DFO) # perfect


# Now that everything is one-Hot encoded and usable i can continue 
###########################################################################################################

# Now im done with the preprocessing so i can split the DF back to its original test train split 
# I use the column "type" that i declared earlier
train <- DFO[DFO$type == "train", ]
test <- DFO[DFO$type == "test", ]
# Now i can remove the column "type" from both datasets
train$type <- NULL
test$type <- NULL

# Check the balance for the prediction variable in train dataset

#table(train$readmitted_y) # It is not balanced
# Balance dataset using ROSE 
# Set the seed for reproducibility
#set.seed(42)

# Apply ROSE to balance the train dataset
# I decided to comment this out because after trial and error i was getting better results without balance

#rose <- ROSE(readmitted_y ~., data = train)$data

# Check the balance for the prediction variable in the balanced dataset
#table(rose$readmitted_y)
# Now we have a balanced dataset 


# Partition the data 
# Set seed for reproducability
set.seed(89)
# Split the full train into two parts 60/40
# Now i will have train wich is what i train my model with 
# validation which I check my performance with 
# and test for the final implementation of the model
idxPrep        <- sample(1:nrow(train),.6*nrow(train))
train    <- train[idxPrep,] # this is what i will train my models with to avoid overfitting
validation <- train[-idxPrep,] # this data will be used for validation


# Making sure everything is a factor with same levels 
# Rename class levels of the response variable in train test validation
train$readmitted_y <- factor(train$readmitted_y, levels = c(1, 0), labels = c("Class_1", "Class_0"))
test$readmitted_y <- factor(test$readmitted_y, levels = c(1, 0), labels = c("Class_1", "Class_0"))
validation$readmitted_y <- factor(validation$readmitted_y, levels = c(1, 0), labels = c("Class_1", "Class_0"))


# NOW I START MODELLING
############################################################################################################
tune_grid <- expand.grid(.alpha = 0,
                         .lambda = 0.112)
# tune_grid <- expand.grid(.alpha = 0:1,
#                          .lambda = seq(0.001, 1, length.out = 10))

# set the parameter for k-Fold Cross Validation
trCntl <- trainControl(method = "CV", number = 5, summaryFunction = twoClassSummary, classProbs = TRUE)

# Train a logistic regression model
Logreg <- train(readmitted_y ~ .,  
                data = train, 
                trControl = trCntl,
                method = "glmnet",
                tuneGrid = tune_grid,
                family = "binomial")

# Look at the best parameter tuning values and then implement them 
# print(Logreg$bestTune) # this was printed with the tune_grid that has ranges 
# > print(Logreg$bestTune)
# alpha lambda
#   0  0.112

# Now I predict on the validation data
Logregpredict <- predict(Logreg, validation) 
# Look at confusion matrix
confusionMatrix(Logregpredict, validation$readmitted_y)
# Accuracy: 0.6308
# Kappa: 0.1894
# 874 TN // 233 TP 

# Predict on test data 
Logregpredict <- predict(Logreg, test)
confusionMatrix(Logregpredict, test$readmitted_y)
# Accuracy: 0.6547
# Kappa: 0.2166
# 1277 TN // 314 TP 

# Now I adjust the cutoff, getting slightly improved results 
LogProb <- predict(Logreg, test, type = "prob")
nCutoff <- .39   #adjusting for highest AUC - Graph below 
nPred <- ifelse(LogProb[, "Class_1"] >= nCutoff, "Class_1", "Class_0")
confusionMatrix(factor(nPred, levels = c("Class_1", "Class_0")), test$readmitted_y)
# Accuracy: 0.6539 --slightly lower accuracy
# Kappa: 0.2223  ---- better Kappa
# 1250 TN // 339 TP -- more  TPS  

# Lets try some other models
###########################################################################################################
# Randomforest using RANGER 

# Set up the traincontrol 
fitControl <- trainControl(method = "cv",
                           number = 5, #5fold crossval
                           verboseIter = TRUE,
                           returnData = FALSE,
                           classProbs = TRUE,
                           returnResamp = "final", 
                           summaryFunction = twoClassSummary,
                           allowParallel = TRUE)
# Hyperparameter Grid
# Adjusted to final fit so i can rerun faster
tune_grid <- expand.grid(
  .mtry = 5, # Sequence from 2 to 5 with a step of 1
  .splitrule = "gini", # gini or extratrees
  .min.node.size = 4 # sequence from 1 to 5 with a step of 1
) 
# Fitting mtry = 5, splitrule = gini, min.node.size = 4 
# Changed to this to increase speed

Rangerforest <- train(readmitted_y ~ 
                        #tmpID +                       # Here i try to list all columns
                        time_in_hospital +            # I was trying to play around with commenting out
                        num_lab_procedures +          # in order to train with less columns
                        num_procedures +              # Since that didnt improve my model 
                        num_medications +             # Decided to include all except tmpID
                        number_diagnoses +
                        tested_glucose +
                        tested_A1C +
                        metformin +
                        repaglinide +
                        nateglinide +
                        chlorpropamide +
                        glimepiride +
                        glipizide +
                        glyburide +
                        tolbutamide +
                        pioglitazone +
                        rosiglitazone +
                        acarbose +
                        miglitol +
                        tolazamide +
                        insulin +
                        change +
                        diabetesMed +
                        age +
                        wgt +
                        had_visitor +
                        had_emergency +
                        previous_patient +
                        is_male +
                        admission_type_idElective +
                        admission_type_idEmergency +
                        admission_type_idNot_Mapped +
                        discharge_disposition_idLeft_AMA +
                        discharge_disposition_idNot_Mapped +
                        discharge_disposition_idTransferred +
                        discharge_disposition_idTransferred_to_Home_with_Assistance +
                        admission_source_idNot_Mapped +
                        admission_source_idReferral +
                        admission_source_idTransfer +
                        raceAsian +
                        raceCaucasian +
                        raceHispanic +
                        raceOther,
                      data = train,
                      method = 'ranger',
                      num.trees = 250,   # adjusted according to the graph below 
                      tuneGrid = tune_grid,
                      trControl = fitControl)

Rangerforest$finalModel
# 22% OOB 

Rangerforestpred <- predict(Rangerforest, test)
confusionMatrix(Rangerforestpred, test$readmitted_y)
# Accuracy: 0.6481
# Kappa: 0.2206
# 1204 TN // 371 TP
# Note that i get slightly different result each time running the same 
# Which is to be expected 

# Now I adjust the cutoff, getting slightly improved results 
RangerProb <- predict(Rangerforest, test, type = "prob")
nCutoff <- .41 # I am optimizing AUC, graph below
Rangerpred <- ifelse(RangerProb[, "Class_1"] >= nCutoff, "Class_1", "Class_0")
confusionMatrix(factor(Rangerpred, levels = c("Class_1", "Class_0")), test$readmitted_y)
# Accuracy: 0.6272 lower accuracy
# Kappa: 0.2542 - Way better
# Less TN but almost double TP ! 


# Below is the code for tuning 

# checking what the best amount of num.trees
Rangerforest <- train(as.factor(readmitted_y) ~ ., data = train,
             method = 'ranger',
             tuneGrid = tune_grid,
             trControl = fitControl)
Rangerforest$finalModel$num.trees
Rangerforest$finalModel$prediction.error

# Run the forloop to get the data for the graph
numTreesVec <- vector()
oobError  <- vector()
nTreeSearch <- seq(from = 100, to = 500, by=50)
for(i in 1:length(nTreeSearch)){
  print(i)
  Rangerforest <- train(as.factor(readmitted_y) ~ ., data = train,
               method = 'ranger',
               num.trees = nTreeSearch[i],
               tuneGrid = tune_grid,
               trControl = fitControl)
  numTreesVec[i] <- Rangerforest$finalModel$num.trees
  oobError[i] <- Rangerforest$finalModel$prediction.error
}
# Plot the results and look at the OOB error rate 
results <- data.frame(ntrees =numTreesVec,
                      oobError = oobError)
ggplot(results, aes(x=ntrees,y=oobError)) + geom_line(alpha =0.25, color = 'red') +
  geom_smooth(method = "loess")
# 250 trees looks like the optimal choice
# Getting slightly different graph here each time as well 
# 250 was the optimal choice for most instances 
# The change in graph happens because i adjust the number of trees above to reduce runtime

############################################################################################################
# Decision Tree model using rpart
library(rpart)

# set up the traincontrol
fitControl <- trainControl(method = "cv",
                           number = 5,
                           verboseIter = TRUE,
                           returnData = FALSE,
                           classProbs = TRUE,
                           returnResamp = "final",
                           summaryFunction = twoClassSummary,
                           allowParallel = TRUE)

# hyperparams
tune_grid <- expand.grid(.cp = c(0.0001, 0.001, 0.005, 0.01, 0.05, 0.07, 0.1, 0.25))

# train using caret 
Dt <- train(readmitted_y ~.,
                   data = train,
                   method = "rpart",
                   trControl = fitControl,
                   tuneGrid = tune_grid,
                   control = rpart.control(minsplit = 50, minbucket = 100)) # adjusted manually

print(Dt$finalModel)

Dt 

dtp <- predict(Dt, test)
confusionMatrix(dtp, test$readmitted_y)
# Accuracy: 0.6523
# kappa: 0.2223
# 1235 TN // 350 TP 

dtprob <- predict(Dt, test, type = "prob") # 45
nCutoff <- 0.45# For decision tree the probability doesnt seem to have a lot of impact atleast in this case 
# Up or down until +- 10% doesnt trigger significant change in the AUC
dtpred <- ifelse(dtprob[, "Class_1"] >= nCutoff, "Class_1", "Class_0")
confusionMatrix(factor(dtpred, levels = c("Class_1", "Class_0")), test$readmitted_y)

############################################################################################################
# Now i fit an XGB model using caret

# Hyperparameters
tune_grid <- expand.grid(
  .nrounds = 100,
  .eta = 0.1,
  .max_depth = 4,
  .gamma = 0,
  .colsample_bytree = 0.6,
  .min_child_weight = 1,
  .subsample = 0.75
)

# tune_grid <- expand.grid(
#   .nrounds = c(100, 150),
#   .eta = c(0.1, 0.3),
#   .max_depth = c(4, 6),
#   .gamma = c(0, 0.1),
#   .colsample_bytree = c(0.6, 0.8),
#   .min_child_weight = c(1, 3),
#   .subsample = c(0.5, 0.75)
# )

# set the parameter for k-Fold Cross Validation
trCntl <- trainControl(method = "CV",number = 5, summaryFunction = twoClassSummary, classProbs = TRUE)

# Train 
xgb <- train(
  readmitted_y ~ .,
  data = train,
  trControl = trCntl,
  method = "xgbTree",
  tuneGrid = tune_grid,
  verbosity = 0 # avoiding error messages
)

# Best tuning parameters    
#print(xgb$bestTune)
# nrounds max_depth eta gamma colsample_bytree min_child_weight subsample
#  100         4   0.1    0           0.6                1      0.75

# Predict
xgbp <- predict(xgb, test)

# Evaluate
confusionMatrix(xgbp, test$readmitted_y)
# Accuracy: 0.6502
# Kappa: 0.2315
# 1184 TN // 396 TP


# Now I adjust the cutoff, getting slightly improved results, since im optimizing ROC i am ok to sacrifice some accuracy
xgbprob <- predict(xgb, test, type = "prob")
nCutoff <- .47  # optimized AUC
xgbpred <- ifelse(xgbprob[, "Class_1"] >= nCutoff, "Class_1", "Class_0")
confusionMatrix(factor(xgbpred, levels = c("Class_1", "Class_0")), test$readmitted_y)
# Accuracy: 0.6436 slightly worse
# Kappa: 0.2529 better
# 1043 TN // 521 TP - Getting better at predicting True positives, which is the most important for this model 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

model_list <- list(LR = Logreg, RF = Rangerforest, DT = Dt, XGB = xgb)
res <- resamples(model_list)
summary(res)

roc.curve(test$readmitted_y, nPred, plotit = T, col = "blue")
roc.curve(test$readmitted_y, Rangerpred, plotit = T, add.roc = T, col = "green")
roc.curve(test$readmitted_y, dtpred, plotit = T, add.roc = T, col = "red")
roc.curve(test$readmitted_y, xgbpred, plotit = T, add.roc = T, col = "orange")

legend(.8, .4, legend = c("LG", "RF", "DT", "XGB"),
       col = c("blue", "green", "red", "orange"),
       lty = c(1,2,3,4), ncol = 1, cex = 0.8) 


# after repeatedly (5x) training all of the 4 models and evaluating their AUC 
# Results: 
# LG 0.631 all
# RF 0.621, 0.627, 0.616, 0.620, 0.625 Average: 0.6218
# DT 0.609 all
# XGB 0.613, 0.605, 0.613, 0.611, 0.614 Average: 0.6112

# The  highest AUC is consistently achieved by the Logistic Regression model
# Therefore I will use this model to get my insights

#######################################################################################################################
probabilities <- predict(Logreg, test, type = 'prob') # get the probabilities
head(probabilities)
yes_probabilities <- probabilities[, "Class_1"] # show only class_1 = TRUE for readmitted_y

results_df <- data.frame(tmpID = test$tmpID,  # create a new dataframe with the information needed 
                         actual_outcome = test$readmitted_y,
                         predicted_probability = yes_probabilities)
head(results_df)

top_100_results <- results_df %>%  # filter the dataframe to desc by probability and get only the top 100 
  arrange(desc(predicted_probability)) %>%
  head(100)

head(top_100_results)

#get the file as csv for submission
write.csv(top_100_results, "top_100.csv", row.names = FALSE)

top_100 <- top_100_results %>% # now i leftjoin to get back all of the data 
  left_join(DF, by = "tmpID") # getting columns from the cleaned dataframe before Onehotencoding and logtransform

head(top_100)

# Check na's 
colSums(is.na(top_100)) # none

# Now i am ready to visualize the information to get further insights into the types of patients
#########################################################################################################################
for (column_name in colnames(top_100)) {
  cat("Column:", column_name, "\n")
  print(table(top_100[[column_name]]))
  cat("\n")
}
# Column: actual_outcome 
# 
# Class_1 Class_0 
# 75%      25% 
names(top_100)

str(top_100)

# Series of Graphs to compare the graphs from full dataset 
# Insights will be described in the presentation

# Looking at the readmission distribution (actual outcome)
table(top_100$readmitted_y)

ggplot(top_100, aes(x = readmitted_y, fill = readmitted_y)) +
  geom_bar() +
  scale_fill_manual(values = c("darkgreen", "darkred")) +
  labs(x = "Readmission", y = "Count", fill = "Readmission") +
  ggtitle("Distribution of Readmission") +
  theme_minimal()


#  Admission Type 
DF_summarized <- top_100 %>%
  group_by(admission_type_id, readmitted_y) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count))

ggplot(DF_summarized, aes(x = admission_type_id, y = count, fill = as.factor(readmitted_y))) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = c("darkgreen", "darkred"), labels = c("No", "Yes")) +
  labs(x = "Admission Type", y = "Count", fill = "Readmission") +
  ggtitle("Relationship between Readmission and Admission Type") +
  geom_text(aes(label = paste0(round(percentage * 100, 1), "%")), position = position_stack(vjust = 0.5)) +
  theme_minimal()


# discharge disp
DF_summarized_discharge <- top_100 %>%
  group_by(discharge_disposition_id, readmitted_y) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count))

ggplot(DF_summarized_discharge, aes(x = discharge_disposition_id, y = count, fill = as.factor(readmitted_y))) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = c("darkgreen", "darkred"), labels = c("No", "Yes")) +
  labs(x = "Discharge Disposition", y = "Count", fill = "Readmission") +
  ggtitle("Relationship between Readmission and Discharge Disposition") +
  geom_text(aes(label = paste0(round(percentage * 100, 1), "%")), position = position_stack(vjust = 0.5)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#  admission source
DF_summarized_admission_source <- top_100 %>%
  group_by(admission_source_id, readmitted_y) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count))

ggplot(DF_summarized_admission_source, aes(x = admission_source_id, y = count, fill = as.factor(readmitted_y))) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = c("darkgreen", "darkred"), labels = c("No", "Yes")) +
  labs(x = "Admission Source", y = "Count", fill = "Readmission") +
  ggtitle("Relationship between Readmission and Admission Source") +
  geom_text(aes(label = paste0(round(percentage * 100, 1), "%")), position = position_stack(vjust = 0.5)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Same for Race
DF_summarized_race <- top_100 %>%
  group_by(race, readmitted_y) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count))

ggplot(DF_summarized_race, aes(x = race, y = count, fill = as.factor(readmitted_y))) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = c("darkgreen", "darkred"), labels = c("No", "Yes")) +
  labs(x = "Race", y = "Count", fill = "Readmission") +
  ggtitle("Relationship between Readmission and Race") +
  geom_text(aes(label = paste0(round(percentage * 100, 1), "%")), position = position_stack(vjust = 0.5)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Time in Hospital 
readmission_perc <- top_100 %>%
  group_by(time_in_hospital, readmitted_y) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  filter(readmitted_y == 1)

# Create line plot
ggplot(readmission_perc, aes(x = time_in_hospital, y = percentage)) +
  geom_line(color = "darkred") +
  labs(x = "Time in Hospital (days)", y = "Readmission Percentage") +
  ggtitle("Relationship between Time in Hospital and Readmission") +
  theme_minimal()


# Age

readmission_age_perc <- top_100 %>%
  mutate(age_bin = cut(age, seq(0, 100, by = 10), include.lowest = TRUE, right = FALSE)) %>%
  group_by(age_bin, readmitted_y) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  filter(readmitted_y == 1)

# Create the line plot 
ggplot(readmission_age_perc, aes(x = age_bin, y = percentage)) +
  geom_line(color = "darkred", group = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "Age Bin", y = "Readmission Percentage") +
  ggtitle("Relationship between Age Bin and Readmission") +
  theme_minimal()



#  Weight 

readmission_wgt_perc <- top_100 %>%
  mutate(wgt_bin = cut(wgt, seq(0, 400, by = 40), include.lowest = TRUE, right = FALSE)) %>%
  group_by(wgt_bin, readmitted_y) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  filter(readmitted_y == 1)

# Create the line plot with a trendline
ggplot(readmission_wgt_perc, aes(x = wgt_bin, y = percentage)) +
  geom_line(color = "darkred", group = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "Weight Bin", y = "Readmission Percentage") +
  ggtitle("Relationship between Weight Bin and Readmission") +
  theme_minimal()


# Now i look at the histograms to put information into perspective

# Columns to exclude from the histograms
exclude_columns <- c("type", "tmpID", "readmitted_y")

# Loop through the columns of top_100
for (column in colnames(top_100)) {
  
  # Skip excluded columns
  if (column %in% exclude_columns) {
    next
  }
  
  # Check if the column is numeric or character
  if (is.numeric(top_100[[column]]) || is.integer(top_100[[column]])) {
    # Create a histogram for numeric variables
    p <- ggplot(top_100, aes(.data[[column]])) +
      geom_histogram(binwidth = 1, color = "black", fill = "darkorange", alpha = 0.7) +
      labs(title = paste("Histogram of", column), x = column, y = "Count") +
      theme_minimal()
  } else {
    # Create a bar plot for character variables
    p <- ggplot(top_100, aes(.data[[column]])) +
      geom_bar(color = "black", fill = "darkorange", alpha = 0.7) +
      labs(title = paste("Bar plot of", column), x = column, y = "Count") +
      theme_minimal()
  }
  
  # Print the plots
  print(p)
}

# Now i have all of the graphs needed for my presentation


# Appendix ----------------------------------------------------------------------------------------------------------
# Second version of XGB 

# Load required libraries
library(xgboost)

# Converting the datasets to a matrix format
train_matrix <- model.matrix(readmitted_y ~ ., data = train)
test_matrix <- model.matrix(readmitted_y ~ ., data = test)

# Converting the y predict to binary format (0 or 1)
train_labels <- as.numeric(train$readmitted_y) - 1
test_labels <- as.numeric(test$readmitted_y) - 1

# Convert everything to xgb.DMatrix format
dtrain <- xgb.DMatrix(data = train_matrix, label = train_labels)
dtest <- xgb.DMatrix(data = test_matrix, label = test_labels)

# Set up the parameters for the XGBoost model
params <- list(
  objective = "binary:logistic",
  eval_metric = "error",
  max_depth = 4, 
  eta = 0.1,
  gamma = 0,
  colsample_bytree = 0.6,
  min_child_weight = 1,
  subsample = 0.75,
  alpha = 0.2, 
  lambda = 2 
)

# Train the XGBoost model
xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 10,
  early_stopping_rounds = 15,
  watchlist = list(train = dtrain, test = dtest)
)

# Predict on the test dataset
test_preds <- predict(xgb_model, dtest)
test_preds <- ifelse(test_preds > 0.5, 1, 0)

# Evaluate the predictions
confusionMatrix(factor(test_preds), factor(test_labels))
# Accuracy: 0.6502
# Kappa: 0.2182
# 1231 TN // 349 TP 

############################################################################################################
# This is a third version of XGB that i was working on

# Prepare the data
train_matrix <- model.matrix(readmitted_y ~ ., data = train)
test_matrix <- model.matrix(readmitted_y ~ ., data = test)
train_labels <- as.factor(as.numeric(train$readmitted_y) - 1)
test_labels <- as.factor(as.numeric(test$readmitted_y) - 1)
levels(train_labels) <- make.names(levels(factor(train_labels)))
levels(test_labels) <- make.names(levels(factor(test_labels)))

# Set up the control for training
xgb_trcontrol <- trainControl(method = "cv",
                              number = 5,
                              verboseIter = TRUE,
                              returnData = FALSE,
                              returnResamp = "all",
                              classProbs = TRUE,
                              summaryFunction = twoClassSummary,
                              allowParallel = TRUE)

# Define hyperparameters to be tuned in a grid
# tune_grid <- expand.grid(nrounds = c(100,150),
#                          max_depth = c(2, 3),
#                          eta = c(0.1, 0.2),
#                          gamma = c(0.1,0.2),
#                          colsample_bytree = c(0.7, 0.8),
#                          min_child_weight = c(3,4),
#                          subsample = c(0.25, 0.5))

tune_grid <- expand.grid(nrounds = 150,
                         max_depth = 2,
                         eta = 0.1,
                         gamma = 0.2,
                         colsample_bytree = 0.4,
                         min_child_weight = 3,
                         subsample = 0.5)

# Train the XGBoost model with grid search
xgb_model <- train(x = train_matrix,
                   y = train_labels,
                   method = "xgbTree",
                   trControl = xgb_trcontrol,
                   tuneGrid = tune_grid,
                   metric = "ROC")

# Print best hyperparameters
print(xgb_model$bestTune)
# nrounds max_depth eta gamma colsample_bytree min_child_weight subsample
#     100         2 0.1   0.1              0.8                3       0.5

# Predict on the test dataset
test_preds <- predict(xgb_model, test_matrix)

# Evaluate the predictions
confusionMatrix(test_preds, test_labels)
# Accuracy: 0.6461
# Kappa: 0.2182
# 1194 TN // 376 TP


########################################################################################################################
# NEW MODEL: NAIVE BAYES 

#hyperparameter grid
tune_grid <- expand.grid(.laplace = seq(0, 1, length.out = 10),
                          .usekernel = c(TRUE, FALSE),
                          .adjust = seq(0, 1, length.out = 5))

# set the parameter for k-Fold Cross Validation
trCntl <- trainControl(method = "CV",number = 5, summaryFunction = twoClassSummary, classProbs = TRUE)

# Train a Naive Bayes model using the naive_bayes 
Nb <- train(readmitted_y ~ .,
            data = train,
            trControl = trCntl,
            method = "naive_bayes",
            tuneGrid = tune_grid)

print(Nb$bestTune)
# laplace usekernel adjust
#   0     FALSE      0

# Predict
Nbp <- predict(Nb, test)
# Evaluate
confusionMatrix(Nbp, test$readmitted_y) # This model is pretty overfitted, I couldnt fix it unfortunately
# Accuracy: 0.6041
# Kappa: 0.0295
# 1415 TN // 53 TP 
