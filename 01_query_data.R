#load libraries
library(DBI)
library(RPostgreSQL)
library(tidyverse)
library(dbplyr)
library(stringr)
library(readr)

#connect to the database
conn = dbConnect(PostgreSQL(), dbname = "mimic",
                 host = "healthdatascience.lshtm.ac.uk",
                 port = 5432,
                 user = "student",
                 password = rstudioapi::askForPassword("Please enter your password"))

#view table names
DBI::dbListTables(conn)


#Question 1
#gather all possible heart failure diagnoses  
#identify icd_9 codes of heart failure
says_heart_failure <- tbl(conn, "d_icd_diagnoses") %>%
  filter(str_detect(long_title, "heart failure"))
heart_failure_icd9 <- says_heart_failure %>%
  filter(!str_detect(short_title, 'w/o hf'))

diagnoses_icd <- tbl(conn, 'diagnoses_icd')

#gather specific patients with heart failure
heart_failure_patients <- diagnoses_icd %>%
  inner_join(heart_failure_icd9, by = "icd9_code") %>%
  distinct(subject_id)


#get all other diagnoses of patients who have heart failure
hf_diagnoses <- diagnoses_icd %>%
  inner_join(heart_failure_patients, by = "subject_id")

#gather information on diabetes status
#get icd_9 codes for secondary diabetes
secondary_diabetes_codes <- tbl(conn, "d_icd_diagnoses") %>%
  filter(str_detect(long_title, "Secondary diabetes")) %>%
  select('icd9_code')

#find heart failure patients who have secondary diabetes
hf_secondary_diabetes <- hf_diagnoses %>%
  inner_join(secondary_diabetes_codes, by = "icd9_code") %>%
  distinct(subject_id)

hf_secondary <- hf_secondary_diabetes %>%
  mutate(has_secondary_diabetes = TRUE)

#make table of all heart failure patients and their diabetes status
hf_patients_sd_code <- heart_failure_patients %>%
  left_join(hf_secondary, by = "subject_id") %>%
  mutate(has_secondary_diabetes = if_else(is.na(has_secondary_diabetes), FALSE, TRUE))


#gather demographic information and admission type information on patients
tbl(conn, "admissions") %>% colnames()

demographics1 <- tbl(conn, 'admissions') %>%
  select(subject_id,
         admission_type,
         language,
         religion,
         marital_status,
         ethnicity)

demographics2 <- tbl(conn, 'demographic') %>%
  select(subject_id,
         age_group,
         insurance,
         gender,
         ethnicity_grouped)

demographics_full <- full_join(demographics1, demographics2, by = 'subject_id')

#make a table of the heart failure patients (and their diabetes status) with their demographics and admission type
hf_summary <- hf_patients_sd_code %>%
  left_join(demographics_full, by = 'subject_id') %>%
  distinct(subject_id, .keep_all = TRUE)

hf_summary_final <- hf_summary %>% collect()

#compare information of heart failures to non heart failures
non_hf_patients <- diagnoses_icd %>%
  anti_join(heart_failure_patients, by = "subject_id")


#find non-heart failure patients who have secondary diabetes
nonhf_secondary_diabetes <- non_hf_patients %>%
  inner_join(secondary_diabetes_codes, by = "icd9_code") %>%
  distinct(subject_id)

nonhf_secondary <- nonhf_secondary_diabetes %>%
  mutate(has_secondary_diabetes = TRUE)

#make table of all non-heart failure patients and their diabetes status
nonhf_patients_sd_code <- non_hf_patients %>%
  left_join(nonhf_secondary, by = "subject_id") %>%
  mutate(has_secondary_diabetes = if_else(is.na(has_secondary_diabetes), FALSE, TRUE))


#combine table of diabetes information with their demographics and admission types (non-heart failures)
nonhf_summary <- nonhf_patients_sd_code %>%
  left_join(demographics_full, by = 'subject_id') %>%
  distinct(subject_id, .keep_all = TRUE)

nonhf_summary_final <- nonhf_summary %>% collect()

#combine summary tables of heart failures and non heart failures

combined_summary <- hf_summary_final %>%
  mutate(hf_status = "Heart Failure") %>%
  bind_rows(
    nonhf_summary_final %>% mutate(hf_status = "Non-Heart Failure")
  )


#Question 2
#get admit and discharge times, demographics of all heart failure admissions
admissions <- tbl(conn, "admissions")
admissions %>% colnames()


summary1 <- diagnoses_icd %>%
  inner_join(heart_failure_icd9, by = "icd9_code") %>%
  inner_join(admissions, by = 'hadm_id') %>%
  select('subject_id.x', 'hadm_id', 'admittime', 'dischtime', 'deathtime') %>%
  left_join(demographics2, join_by('subject_id.x' == 'subject_id'))


#get icu data on heart failure admissions,demographics,  join with summary1
icu_info <- tbl(conn, "icustays") %>% select('hadm_id', 'intime', 'outtime')

hfs_icu <- summary1 %>% left_join(icu_info, by = 'hadm_id')

#join heart failure data on admissions, demographics, and icu with diabetes
q2_1 <- hfs_icu %>%
  left_join(hf_patients_sd_code, join_by('subject_id.x' == 'subject_id')) %>%
  distinct(hadm_id, .keep_all = TRUE) %>% collect()

#create new variables that are needed for analysis, change units for time variables to days
question2 <- q2_1 %>%
  mutate(time_in_icu = (outtime - intime),
         time_in_hospital = (dischtime - admittime),
         age_strat = case_when(
           age_group %in% c("0-9", "10-19") ~ "0–17",
           age_group %in% c("20-29", "30-39", "40-49", "50-59") ~ "18–59",
           age_group %in% c("60-69", "70-79", "80-89", "90+") ~ "60+",
           TRUE ~ NA_character_
         ),
         died = !is.na(deathtime))

units(question2$time_in_icu) <- 'days'
units(question2$time_in_hospital) <- 'days'

#Question 3
prescriptions <- tbl(conn, 'prescriptions')
prescriptions %>% colnames()

#create dataframe with necessary information
hf_prescription <- diagnoses_icd %>%
  inner_join(heart_failure_icd9, by = "icd9_code") %>%
  inner_join(admissions, by = 'hadm_id') %>%
  left_join(prescriptions, by = 'hadm_id') %>%
  select('hadm_id', 'drug', 'icustay_id', 'dose_val_rx', 'dose_unit_rx')

#collect top 5 drugs
top_5_drugs <- hf_prescription %>%
  filter(!is.na(drug)) %>%
  count(drug, sort = TRUE) %>%
  head(5)


#summarize top5 drugs
question3 <- hf_prescription %>%
  inner_join(top_5_drugs, by = 'drug') %>%
  distinct() %>% collect()

#Convert final datasets into CSVs in a new folder to be used in quarto
install.packages("here")  
library(here)
dir.create(here("HDMAssessment"), showWarnings = FALSE)
write.csv(hf_summary_final,
          here("HDMAssessment", "hf_summary_final.csv"),
          row.names = FALSE)

write.csv(nonhf_summary_final,
          here("HDMAssessment", "nonhf_summary_final.csv"),
          row.names = FALSE)

write.csv(question2,
          here("HDMAssessment", "question2.csv"),
          row.names = FALSE)

write.csv(question3,
          here("HDMAssessment", "question3.csv"),
          row.names = FALSE)