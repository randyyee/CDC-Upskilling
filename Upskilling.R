#### Recency Model (Main)
#### CREATION DATE: 3/6/2019
#### NOTE: For CDC Upskilling Project
#### MODELS: Random Forest, SVM, Max Entropy


setwd("~/Randy Codebase/R/Upskilling")

#/////////////////////////////////////////////////////////////////////////
# ---------------------------- Load Libraries ----------------------------
#/////////////////////////////////////////////////////////////////////////
library(tidyverse)
library(openxlsx)
library(readstata13)
library(MXM)
library(bnlearn)
library(bnviewer)



#/////////////////////////////////////////////////////////////////////////
# --------------------------- Get Recency Data ---------------------------
#/////////////////////////////////////////////////////////////////////////
df_raw <- read.dta13("mdrs_final_analytic_dataset_30sep2018_de-identified.dta")
#openxlsx::write.xlsx(df_raw, "MWI_recency_raw.xlsx")


#/////////////////////////////////////////////////////////////////////////
# -------------------------- Clean Recency Data --------------------------
#/////////////////////////////////////////////////////////////////////////
colnames(df_raw)
cor(df_raw)

# ==== 1) Drop empty columns ====
#rna to NA, dk

new_df <- purrr::discard(df_raw, ~all(is.na(.)))
openxlsx::write.xlsx(new_df, "MWI_recency_raw.xlsx")

# ==== 2) Drop Unneeded Columns ====  
df_clean <- new_df %>%
  filter(merge == "no data missing" & merge_1 == "no data missing") %>%
  select(-c(
    "ID",
    "interviewer",
    "interviewdate",
    "comments",
    "adddate",
    "addby",
    "update",
    "updateby",
    "_merge",
    "_merge1",
    "DateResultReturnedatClinic",
    "why_no_ror",
    "SpecimenCollectionDate",
    "SpecimenPickedUpDateatANC",
    "ANCSiteCode",
    "date_spec_col",
    "date_spec_tra",
    "date_spec_lab",
    "date_spec_test",
    "date_res_dis",
    "ANCSiteCode",
    "EncryptedANCPatientID",
    "Age3",
    "LabFacilityCode",
    "RejectionCodeAtCentralLab",
    "ReceiptDateatLab",
    "LabTechNameID",
    "DateofTestCompletion",
    "ResultDispatchedDatetoClinic",
    "AddedDate",
    "Addedby",
    "LastUpdatedDate",
    "LastUpdatedby",
    "DateReturnedtoParticipant",

  ))

# ==== 3) Complete Participant Age ====  
## age, part_age
df_clean_p_age <- df_clean %>% 
  mutate(clean_age = ifelse(!is.na(age), age, participantage)) %>%
  filter(!is.na(clean_age))


# ==== 4) Complete Partner Age ==== 
## part_age, ptnragedif
df_clean_part_age <- df_clean_p_age %>% 
  mutate(clean_part_age = clean_age - part_age)


# ==== 5) Complete Occupation Other ==== 
df_clean_occ <- df_clean_part_age %>% 
  mutate(occupation = ifelse(occupation == "other" & !is.na(occupothr), occupothr, occupation))


# ==== 6) Clean Yrs Sex ==== 
df_clean_yrssex <- df_clean_occ %>%
  mutate(yrs_sex = clean_age - agefirstsex)

#/////////////////////////////////////////////////////////////////////////
# ------------------------- Explore Recency Data -------------------------
#/////////////////////////////////////////////////////////////////////////

