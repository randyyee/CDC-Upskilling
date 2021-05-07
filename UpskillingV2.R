#### Recency Model (Main)
#### AUTHOR: Randy Yee
#### CREATION DATE: 3/6/2019
#### NOTE: For CDC Upskilling Project

do{
  
  
  rm(list = ls())
  cat("\014")
  gc()
  
  setwd("~/Randy Codebase/R/Upskilling")
  
  #/////////////////////////////////////////////////////////////////////////
  # ---------------------------- Load Libraries ----------------------------
  #/////////////////////////////////////////////////////////////////////////
  library(tidyverse)
  library(openxlsx)
  library(readstata13)
  
  
  
  #/////////////////////////////////////////////////////////////////////////
  # --------------------------- Get Recency Data ---------------------------
  #/////////////////////////////////////////////////////////////////////////
  df_raw <- read.dta13("mdrs_final_analytic_dataset_30sep2018_de-identified.dta")
  #openxlsx::write.xlsx(df_raw, "MWI_recency_raw.xlsx")
  
  
  #/////////////////////////////////////////////////////////////////////////
  # -------------------------- Clean Recency Data --------------------------
  #/////////////////////////////////////////////////////////////////////////
  #colnames(df_raw)
  #cor(df_raw)
  
  # ==== 1) Drop empty columns ====
  df_raw <- purrr::discard(df_raw, ~all(is.na(.)))
  #openxlsx::write.xlsx(new_df, "MWI_recency_raw.xlsx")
  
  # ==== 2) Keep Select Columns ====  
  df_clean <- df_raw %>%
    #filter(merge == "no data missing" & merge_1 == "no data missing") %>%
    select(c(
      
      "ANCsite",
      "ancdistrict",
      
      
      #Q0 What is your age?
      "age", # 88 - Don't know; 99 - Refused
      "participantage",
      #TODO: QC age
      
      
      #Q1 What kind of work/occupation do you do most of the time?
      "occupation", # 88 - Don't know; 99 - Refused
      "occupothr", # Other
      #TODO: Collapse occupation
      
      #Q2 Have you ever attended school?
      "education",
      #Q2A If YES to Q2, what was the highest level of education that you completed or are attending now?
      "educationlvl",
      #Q2B Are you attending school now?
      "isparticipantattendingschoolnow",
      #TODO: QC education; Fill in educationlvl (nevered); Fill in isparticipantattendingschoolnow (nevered)
      
      
      #Q3 Have you ever been married or lived with a partner in a union as if married?
      "evermarried",
      #Q3A If YES to Q3, how old were you when you first got married or lived with a partner in a union?
      "agemarried",
      #Q3B If YES to Q3, have you ever been widowed? That is, did a spouse ever pass away while you were still married or living with them?   
      "everwidow",
      #Q3C If YES to Q3, what is your marital status now?
      "maritalstatus",
      #TODO: QC evermarried; Fill in agemarried (nevermarried); Fill in everwidow (nevermarried); Fill in marital status (nevermarried)
      
      
      #Q4 [BEFORE TODAY] Have you ever been tested for HIV?
      "everHIVtest",
      #Q4A If YES to Q4, when was the most recent time you were tested for HIV?   
      "timelastHIVtest",
      #Q4B If YES to Q4, what was the result of that HIV test?
      "resultlastHIVtest",	
      #Q4C If YES to Q4, where were you tested for HIV - in the community or at a facility?
      "placeoflastHIVtest",	# 1 - Facility; 2 - Community; 3 - Don't Know
      #TODO: QC everHIVtest; Fill in timelastHIVtest (nevertest); Fill in resultHIVtest (nevertest); Fill in placeoflastHIVtest (nevertest)
      
      
      #Q5 How old were you when you had sexual intercourse for the very first time?
      "agefirstsex",
      #Q6 The first time you had sex, was it because you wanted to or because you were forced to? 
      "forcedfirstsex",	
      
      
      #Q7 How many different people have you had sexual intercourse with in your entire life? 
      "sexptnrlife",	
      #Q8 How many different people have you had sex with in the last 6 months? 
      "sexptnr6m",
      #TODO: Recode 0/Blank to 1 for sexptnrlife
      
      #Q9 What is the HIV status of your spouse/main sexual partner's (person with whom you have sexual intercourse most frequently)?  
      "ptnrHIVstat",	# 1 - POS; 2 - NEG; 3 - No Main Sexual Partner; 88 - Don't Know; 99 - Refused
      
      #Q10 How old was your spouse/main sexual partner on his last birthday?
      "part_age",
      #Q11 Is the age of your spouse/main sexual partner older, younger, or the same age as you?
      "ptnragedif",	
      #TODO: Collapse ptnagedif
      
      #Q12 Is your spouse/main sexual partner circumcised?
      "circumcised",	
      #Q12A If YES to Q12, what type of circumcision?
      "circumtype",	
      #TODO: QC circumsied; Fill in circumtype (circumnever)
      
      
      #Q13 Have you ever received money/gifts in exchange for sex?  
      "gifts4sex",	
      #Q13A If YES to Q13, when did you last receive money/gifts in exchange for sex?
      "gifts4sexlast",
      #TODO: QC gifts4sex; Fill in gifts4sexlast (nevergifts4sex)
      
      
      #Q14 How many times have you been pregnant? (including this pregnancy)
      "pregnancies",	
      #Q14A When you got pregnant this time, did you want to get pregnant?
      "haspatientwantedtogetpregnant",	
      #Q14B If NO to Q14A, did you want to have a baby later on or did you not want any(more) children?
      "haspatientwantedtohaveababylater",
      #TODO: Fill in haspatientwantedtohaveababylater (didnotwantpreg)
      
      #Q15 Before becoming pregnant this time, in the past year how frequently did you use a condom when having sexual intercourse?   
      "condomyr",
      #Q16 Since becoming pregnant this time, how frequently have you used a condom when having sexual intercourse?
      "condompreg",
      #TODO: QC condomyr & condompreg
      
      
      #Q17 In the past year, did you have symptoms such as abnormal genital discharge, sores in your genital area, pain during urination, or pain in your lower abdomen?   
      "STIsym",	
      #Q17A If YES to Q17, were you treated for these symptoms?
      "STItreat",	
      #Q18 Have you ever been diagnosed or treated for syphilis?  
      "syphilis",	
      #Q19 Have you ever received a vaccine (HPV vaccine) to prevent cervical cancer, which is caused by a common virus that can be passed through sexual contact?  
      "HPVvaccine",	
      #TODO: Fill in STItreat (neversti); Fill in syphillis (neversti)
      
      
      #Q20 During the past 6 months, on how many days did you have at least one drink containing alcohol?
      "alcdays6m",
      #Q21 How often do you have 4 or more drinks with alcohol on one occasion (e.g., within 2 hours)?
      "alcbingefreq",	# 1 - Never; 2 - Monthly; 3 - Weekly; 4 - Daily or Most Days; 88 - Don't Know; 99 - Refused
      
      
      #Q22 Have you ever tried recreational drugs?
      "everdrugs",
      #Q22A If YES  to Q22, in the last 6 months, have you taken any recreational drugs? 
      "drugs6m",
      #YES to previous
      "drugtype",	
      #TODO: Collapse drugs6m & drugtype; QC everdrugs; Fill in drugs6m (neverdrug); Fill in drugtype (neverdrug)
      
      
      #Q23 Have you ever been emotionally or physically abused by your partner or your loved one?
      "everabuse",	
      #Q23A If YES to Q23, who abused you? (Circle all that apply)
      "everabuseby",	# 1 -Husband; 2 - Ex-husband; 3 - Boyfriend; 4 - Stranger; Other; 88; 99
      #Q23B If YES to Q23, how many times were you abused? 
      "everabusefreq", # Times; 88; 99	
      
      #Q24 Within the last year, have you ever been hit, slapped, kicked, or otherwise physically hurt by someone?
      "abuselastyr",
      #Q24A If YES to Q24, who hurt you? (Circle all that apply)
      "abuselastyrby", # See everabuseby
      # Other
      "abuselastyrbyothr",	
      #Q24B If YES to Q24, how many times were you hurt? 
      "abuselastyrfreq", # Times; 88; 99	
      
      #Q25 Since you've been pregnant have you been slapped, kicked or otherwise physically hurt by someone?
      "abusepreg",
      #Q25A If YES to Q25, who hurt you? (Circle all that apply)
      "abusepregby", # See everabuseby
      # Other
      "abusepregbyothr",	
      #Q25B If YES to Q25, how many times were you hurt? 
      "abusepregfreq",	
      
      #Q26 Within the last year, has anyone forced you to have sexual activities?
      "forcedsexyr",	
      #Q26A If YES to Q26, who forced you? (Circle all that apply)
      "forcedsexyrby",	
      #Q26B If YES to Q26, how many times were you forced? 
      "forcedsexyrfreq",	
      
      
      #Q27 Are you afraid of any of the following people?
      "afraid",	# See everabuseby
      "patientafraidofanyone",
      # Other
      "patientafraidofother",	
      #TODO: Collapse all
      
      
      #Q28 Have you ever participated in a club for adolescents? If YES, was it a Go Girls! club or a different club?
      "haspatienteverparticipatedinaclu",	
      #Q28A If YES (Go Girls! or different club) to Q28, when did you participate in the club?
      "timeofpatientparticipateinclub",	
      #Q29 Have you ever participated in a mothers' group? 
      "haspatienteverparticipatedinmoth",
      
      
      "trimester",
      as.character("date_res_dis"),
      #"weight",
      "lag_rita_res"
      
    )) %>%
    na_if("") %>%
    filter(!is.na(lag_rita_res)) %>%
    filter(!is.na(ANCsite) & !is.na(ancdistrict))
  
  # ==== 3) Clean Participant Age ====  
  ## age, part_age
  df_clean_p_age <- df_clean %>% 
    mutate(age = ifelse(!is.na(age), age, participantage)) %>%
    filter(!is.na(age)) %>%
    select(-participantage)
  
  # ==== 4) Clean Occupation Other ==== 
  df_clean_occ <- df_clean_p_age %>% 
    mutate(occupation = ifelse(occupation == "other" & !is.na(occupothr), 
                               occupothr, 
                               ifelse(occupation=="response not available",
                                      "refused",
                                      occupation))) %>%
    select(-occupothr)
  
  # ==== 5) Clean Education ==== 
  df_clean_edu <- df_clean_occ %>%
    mutate(education = case_when(education == "response not available" ~"refused",
                                 education == "no" & educationlvl == "primary/vocational" ~ "yes",
                                 education == "no" & educationlvl == "secondary" ~ "yes",
                                 education == "no" & educationlvl == "university/college" ~ "yes",
                                 education == "no" & isparticipantattendingschoolnow == "yes" ~ "yes",
                                 TRUE ~ education)) %>%
    mutate(educationlvl = ifelse(education == "no",
                                 "never_edu",
                                 ifelse(educationlvl == "response not available",
                                        "refused",
                                        educationlvl))) %>%
    mutate(isparticipantattendingschoolnow = case_when(education == "no" ~ "never_edu",
                                                       is.na(isparticipantattendingschoolnow) | isparticipantattendingschoolnow == "rna" ~ "refused",
                                                       TRUE ~isparticipantattendingschoolnow))
  
  # ==== 6) Clean Married ==== 
  df_clean_married <- df_clean_edu %>% 
    mutate_at(vars(agemarried), as.character) %>%
    mutate(evermarried = case_when(is.na(evermarried) ~"refused",
                                   evermarried == "rna" ~"refused",
                                   evermarried == "dk" ~"don't know",
                                   !is.na(agemarried) ~"yes",
                                   everwidow == "yes" ~"yes",
                                   everwidow == "no" ~"no",
                                   maritalstatus == "married/cohabiting" ~"yes",
                                   maritalstatus == "divorced" ~"yes",
                                   maritalstatus == "separated" ~"yes",
                                   maritalstatus == "widowed" ~"yes",
                                   maritalstatus == "never married/cohabiting" ~ "no",
                                   TRUE ~evermarried
                                   
                                   
                                   
    )) %>%
    #mutate(agemarried = ifelse(is.na(agemarried), "0", agemarried)) %>%
    mutate(agemarried = case_when(evermarried == "yes" & is.na(agemarried) ~"yes, refused",
                                  evermarried == "no" & is.na(agemarried) ~"never_married",
                                  evermarried == "response not available" | evermarried == "refused" | evermarried == "don't know" ~"refused",
                                  #evermarried == "don't know" ~"don't know",
                                  agemarried %in% c("10", "11", "12", "13", "14", "15") ~"10-15",
                                  agemarried %in% c("16", "17", "18", "19", "20") ~"16-20",
                                  agemarried %in% c("21", "22", "23", "24", "25") ~"21-25",
                                  TRUE ~agemarried)) %>%
    mutate(everwidow = case_when(is.na(everwidow) & !is.na(evermarried) ~"refused",
                                 evermarried == "no" ~"never_married",
                                 maritalstatus == "widowed" ~"yes",
                                 evermarried == "refused" ~"refused",
                                 evermarried == "don't know" ~"don't know",
                                 TRUE ~everwidow)) %>%
    mutate(everwidow = ifelse(everwidow=="rna", "refused",everwidow)) %>%
    mutate(maritalstatus = case_when(maritalstatus == "refused" ~"refused",
                                     evermarried == "refused" ~"refused",
                                     evermarried == "don't know" ~"don't know",
                                     evermarried == "no" ~"never married/cohabiting",
                                     everwidow == "yes" & is.na(maritalstatus) ~"widowed",
                                     is.na(maritalstatus) | maritalstatus == "rna" ~ "refused",
                                     TRUE ~maritalstatus
    ))
  
  # ==== 7) Clean HIVTest ====
  df_clean_HIV <- df_clean_married %>%
    mutate(everHIVtest = ifelse(!is.na(timelastHIVtest) | !is.na(resultlastHIVtest) | !is.na(placeoflastHIVtest),
                                "yes", 
                                ifelse(is.na(everHIVtest) & is.na(timelastHIVtest) & is.na(resultlastHIVtest) & is.na(placeoflastHIVtest), 
                                       "refused",
                                       everHIVtest))) %>%
    mutate(timelastHIVtest = ifelse(everHIVtest=="no",
                                    "never_hivtest",
                                    ifelse(is.na(timelastHIVtest) | timelastHIVtest == "response not available",
                                           "refused",
                                           timelastHIVtest)))%>%
    mutate(resultlastHIVtest = ifelse(everHIVtest=="no",
                                      "never_hivtest",
                                      ifelse(resultlastHIVtest == "rna" | resultlastHIVtest == "indeterminate" | is.na(resultlastHIVtest),
                                             "refused",
                                             ifelse(resultlastHIVtest == "dk" | resultlastHIVtest == "didnotreceive",
                                                    "don't know",
                                                    resultlastHIVtest)))) %>%
    mutate(placeoflastHIVtest = ifelse(is.na(placeoflastHIVtest) | placeoflastHIVtest=="rna",
                                       "refused",
                                       placeoflastHIVtest)) %>%
    mutate(everHIVtest = ifelse(everHIVtest=="rna", "refused",everHIVtest))
  
  # ==== 8) Clean First Sex ====
  df_clean_firstsex <- df_clean_HIV %>%
    # mutate(agefirstsex = ifelse(is.na(agefirstsex) | agefirstsex == 99 | agefirstsex == 777,
    #                             999,
    #                             ifelse(agefirstsex==88,
    #                                    888,
    #                                    agefirstsex))) %>%
    mutate(agefirstsex = ifelse(is.na(agefirstsex) | 
                                  agefirstsex == 99 | 
                                  agefirstsex == 777 | 
                                  agefirstsex == 999 | 
                                  agefirstsex == 88 | 
                                  agefirstsex == 888,
                                NA, agefirstsex)) %>%
    mutate(forcedfirstsex = ifelse(is.na(forcedfirstsex) | forcedfirstsex == "response not available",
                                   "refused",
                                    forcedfirstsex)) # %>%
    # mutate_at(vars(agemarried), as.character) %>%
    # mutate(agefirstsex = case_when(agefirstsex == "999" ~"refused",
    #                                agefirstsex == "888" ~"don't know",
    #                                agefirstsex == "777" ~"refused",
    #                                TRUE ~agefirstsex
    # ))
  
  # ==== 9) Clean Sex Partner ====
  df_clean_sexpart <- df_clean_firstsex %>%
    mutate(sexptnrlife = ifelse(is.na(sexptnrlife) | 
                                  sexptnrlife == 777 | 
                                  sexptnrlife == 888 | 
                                  sexptnrlife == 999, sexptnr6m,
                                sexptnrlife
    )) %>%
    mutate(sexptnrlife = ifelse(sexptnrlife == 777 | is.na(sexptnrlife), 
                                999, 
                                sexptnrlife)) %>%
    mutate_at(vars(sexptnrlife), as.character) %>%
    mutate(sexptnrlife = case_when(sexptnrlife == "999" | is.na(sexptnrlife) ~"refused",
                                   sexptnrlife == "888" ~"don't know",
                                   sexptnrlife == "0" ~"refused",
                                   sexptnrlife == "3 " ~"3",
                                   sexptnrlife %in% c("6", "7", "8", "9", "10", "20") ~"more than 5",
                                   TRUE ~sexptnrlife))%>%
    mutate_at(vars(sexptnr6m), as.character) %>%
    mutate(sexptnr6m = case_when(sexptnr6m == "999" | is.na(sexptnr6m) ~ "refused",
                                 sexptnr6m == "888" ~"don't know",
                                 sexptnr6m == "777" ~"refused",
                                 TRUE ~sexptnr6m))
  
  # ==== 7) Clean Partner Age ==== 
  ## part_age & ptnragedif to ptnragedif categories
  df_clean_part_age <- df_clean_sexpart %>% 
    mutate(clean_part_age = part_age - age) %>%
    mutate(clean_ptnagedif = case_when(clean_part_age == 0 ~ "same age",
                                       clean_part_age >= 1 & clean_part_age < 5 ~ "<5yrs older",
                                       clean_part_age >= 5 & clean_part_age <= 10 ~ "5-10yrs older",
                                       clean_part_age > 10 ~ ">10yrs older",
                                       clean_part_age <= -1 & clean_part_age > -5 ~ "<5yrs younger",
                                       clean_part_age <= -5 & clean_part_age >= -10 ~ "5-10yrs younger",
                                       clean_part_age < -10 ~ ">10yrs younger"))%>%
    mutate(ptnragedif = ifelse(is.na(clean_ptnagedif), 
                               ptnragedif, 
                               clean_ptnagedif)) %>%
    mutate(ptnragedif = ifelse(is.na(ptnragedif) | ptnragedif == "", 
                               "response not available", 
                               ptnragedif)) %>%
    select(-c("clean_ptnagedif",
              "clean_part_age")) %>%
    mutate(ptnrHIVstat = case_when(ptnrHIVstat == "dk" ~ "don't know",
                                   ptnrHIVstat == "r" | ptnrHIVstat == "rna" | is.na(ptnrHIVstat) ~"refused",
                                   TRUE ~ptnrHIVstat
                                   
    ))%>%
    select(-part_age)
  
  # ==== 6) Clean Circumcision ==== 
  df_clean_circum <- df_clean_part_age %>%
    mutate(circumcised = ifelse(circumtype == "traditional" | circumtype=="medical",
                                "yes",
                                circumcised)) %>%
    mutate(circumcised = case_when(circumcised == "dk" ~"don't know",
                                   circumcised == "r" | circumcised == "rna" | is.na(circumcised) ~"refused",
                                   TRUE ~circumcised),
           circumtype = case_when(circumcised == "no" ~ "never_circum",
                                  circumtype == "dk" ~ "don't know",
                                  circumtype == "r" | circumtype == "rna" | is.na(circumtype) ~"refused",
                                  TRUE ~circumtype))
  
  
  # ==== 7) Clean Gifts4sex ==== 
  df_clean_gift <- df_clean_circum %>%
    mutate(gifts4sex = case_when(gifts4sex == "dk" ~ "don't know",
                                 gifts4sex == "r" | gifts4sex == "rna" | is.na(gifts4sex) ~"refused",
                                 TRUE ~gifts4sex),
           gifts4sexlast = ifelse(gifts4sexlast == "response not available" | is.na(gifts4sexlast),
                                  "refused",
                                  gifts4sexlast))
  
  # ==== 8) Clean Pregnancies ==== 
  df_clean_preg <- df_clean_gift %>%
    mutate(pregnancies = case_when(pregnancies == ",1" | pregnancies == "1 " ~"1",
                                   pregnancies == ",2" ~"2",
                                   is.na(pregnancies) | pregnancies == "999" | pregnancies == "888" ~"refused",
                                   #pregnancies == "888" ~"don't know",
                                   TRUE ~pregnancies),
           haspatientwantedtogetpregnant = case_when(haspatientwantedtogetpregnant == "dk" ~ "don't know",
                                                     haspatientwantedtogetpregnant == "r" | haspatientwantedtogetpregnant == "rna" | is.na(haspatientwantedtogetpregnant) ~"refused",
                                                     TRUE ~haspatientwantedtogetpregnant),
           haspatientwantedtohaveababylater = case_when(haspatientwantedtohaveababylater == "dk" ~ "don't know",
                                                        haspatientwantedtohaveababylater == "r" | haspatientwantedtohaveababylater == "rna" | is.na(haspatientwantedtohaveababylater) ~"refused",
                                                        TRUE ~haspatientwantedtohaveababylater)
    )
  
  
  # ==== 9) Clean Condom ==== 
  df_clean_cond <- df_clean_preg %>%
    mutate(condomyr = case_when(condomyr == "dk" ~ "don't know",
                                condomyr == "r" | condomyr == "rna" | is.na(condomyr) ~"refused",
                                TRUE ~condomyr),
           condompreg = case_when(condompreg == "dk" ~ "don't know",
                                  condompreg == "r" | condompreg == "rna" | is.na(condompreg) ~"refused",
                                  TRUE ~condompreg)
    )
  
  
  # ==== 10) Clean STI ====
  df_clean_sti <- df_clean_cond %>%
    mutate(STIsym = case_when(STIsym == "dk" ~ "don't know",
                              STIsym == "r" | STIsym == "rna" | is.na(STIsym) ~"refused",
                              TRUE ~STIsym),
           STItreat = case_when(STItreat == "dk" ~ "don't know",
                                STItreat == "r" | STItreat == "rna" | is.na(STItreat) ~"refused",
                                TRUE ~STItreat),
           syphilis = case_when(syphilis == "dk" ~ "don't know",
                                syphilis == "r" | syphilis == "rna" | is.na(syphilis) ~"refused",
                                TRUE ~syphilis),
           HPVvaccine = case_when(HPVvaccine == "dk" ~ "don't know",
                                  HPVvaccine == "r" | HPVvaccine == "rna" | is.na(HPVvaccine) ~"refused",
                                  TRUE ~HPVvaccine)
           
    )
  
  # ==== 11) Clean Alcohol ====
  df_clean_alc <- df_clean_sti %>%
    mutate(alcdays6m = case_when(alcdays6m == "O" | alcdays6m == "0"  ~ "None",
                                 alcdays6m %in% c("1", "2", "3", "4", "5", "7") ~"Less than week",
                                 alcdays6m %in% c("16", "20") ~"2-3 weeks",
                                 # alcdays6m == "30" ~"1 month",
                                 # alcdays6m == "60" ~"2 months",
                                 # alcdays6m == "90" ~"3 months",
                                 alcdays6m %in% c("30", "60", "90") ~"1-3 months",
                                 alcdays6m == "160" | alcdays6m == "180" ~"5-6 month",
                                 alcdays6m == "777" | alcdays6m == "999" ~ "refused",
                                 alcdays6m == "88" | alcdays6m == "888" ~ "don't know",
                                 is.na(alcdays6m) ~ "refused",
                                 TRUE ~alcdays6m),
           alcbingefreq = case_when(alcbingefreq == "dk" ~ "don't know",
                                    alcbingefreq == "r" | alcbingefreq == "rna" | is.na(alcbingefreq) ~"refused",
                                    TRUE ~alcbingefreq)
    )
  
  
  # ==== 12) Clean Drugs ====
  df_clean_drugs <- df_clean_alc %>%
    mutate(everdrugs = case_when(everdrugs == "dk" ~ "don't know",
                                 everdrugs == "r" | everdrugs == "rna" | is.na(everdrugs) ~"refused",
                                 TRUE ~everdrugs),
           drugs6m = case_when(everdrugs == "no" ~ "never_drugs",
                               drugs6m == "dk" ~ "don't know",
                               drugs6m == "r" | drugs6m == "rna" | is.na(drugs6m) ~"refused",
                               TRUE ~drugs6m),
           drugtype = case_when(everdrugs == "no" ~ "never_drugs",
                                everdrugs == "refused" ~ "refused",
                                everdrugs == "yes" & drugs6m == "yes" & drugtype == "not applicable" ~ "yes, refused",
                                everdrugs == "yes" & drugs6m == "yes" & drugtype == "No response" ~ "yes, refused",
                                drugs6m == "no" ~ "not applicable",
                                drugs6m == "refused" ~ "refused",
                                drugtype == "dk" ~ "don't know",
                                drugtype == "r" | drugtype == "rna" | is.na(drugtype) | drugtype == "No response"  ~"refused",
                                drugtype == "MARIJUANA" | drugtype == "CANABIS" | drugtype == "Canabis" ~ "marijuana",
                                TRUE ~drugtype)
    )
  
  # ==== 14) Clean Abuse ====
  df_clean_abuse <- df_clean_drugs %>%
    mutate(everabuse = case_when(!is.na(everabuseby) & everabuseby != "rna" ~"yes",
                                 everabuse == "rna" | is.na(everabuse) ~"refused",
                                 TRUE ~everabuse),
           everabuseby = case_when(everabuse == "yes" & is.na(everabuseby) ~"yes, refused",
                                   everabuse == "no" ~"never_abuse",
                                   everabuse == "refused" & is.na(everabuseby) ~"refused",
                                   everabuseby == "rna" ~ "refused",
                                   TRUE ~everabuseby),
           everabusefreq = case_when(everabuse == "no" ~"never_abuse",
                                     everabusefreq == "777" & everabuse == "refused" ~"refused",
                                     everabusefreq == "999" &  everabuse == "refused" ~"refused",
                                     everabusefreq == "777" &  everabuse == "yes" ~"yes, refused",
                                     everabusefreq == "999" &  everabuse == "refused" ~"yes, refused",
                                     everabuse == "yes" & is.na(everabusefreq) ~"yes, refused",
                                     is.na(everabusefreq) ~"refused",
                                     TRUE ~everabusefreq),
           abuselastyrby = ifelse(abuselastyrby == "other", 
                                  abuselastyrbyothr, 
                                  abuselastyrby),
           abuselastyr = ifelse(abuselastyr == "rna" | is.na(abuselastyr),
                                "refused",
                                abuselastyr)) %>%
    mutate(everabusefreq = case_when(everabusefreq == "888" ~"don't know",
                                     everabusefreq == "999" ~"refused",
                                     everabusefreq %in% c("1", "2", "3", "4", "5", "7") ~"Less than 10",
                                     TRUE ~everabusefreq
                                     )) %>%
    mutate_at(vars(abusepregfreq), as.character) %>%
    
    mutate(abuselastyrby = case_when(abuselastyr == "no" ~ "not_abuselastyr",
                                     abuselastyrby == "r" | abuselastyrby == "rna" | abuselastyr == "refused" ~ "refused",
                                     abuselastyr == "yes" & is.na(abuselastyrby) ~ "yes, refused",
                                     TRUE ~abuselastyrby),
           abuselastyrfreq = case_when(abuselastyr == "no" ~ "not_abuselastyr",
                                       abuselastyr == "rna" ~ "refused",
                                       abuselastyrfreq == "777" ~"refused",
                                       abuselastyr == "yes" & is.na(abuselastyrfreq) ~"yes, refused",
                                       abuselastyr == "refused" ~"refused",
                                       abuselastyrfreq == "Many " | abuselastyrfreq == "Many" ~"many",
                                       TRUE ~abuselastyrfreq),
           abusepreg = ifelse(is.na(abusepreg),
                              "refused",
                              abusepreg),
           abusepregby = ifelse(abusepregby == "r" | abusepregby == "rna" | is.na(abusepregby),
                                "refused",
                                abusepregby)) %>%
    
    mutate(abusepregby = ifelse(abusepreg == "no",
                                "not_abusepreg",
                                abusepregby),
           abusepregfreq = case_when(abusepreg == "no" ~"not_abusepreg",
                                     abusepreg == "refused" ~"refused",
                                     abusepreg == "1" | abusepreg == "6" ~"yes",
                                     abusepreg == "yes" & is.na(abusepregfreq) ~"yes, refused",
                                     abusepreg == "yes" & abusepregfreq == "999" ~"yes, refused",
                                     abusepreg == "yes" & abusepregfreq == "0" ~"yes, refused", #Error correction
                                     TRUE ~abusepregfreq))%>%
    
    select(-c("abuselastyrbyothr", "abusepregbyothr"))
  
  
  # ==== 15) Clean Forced Sex ====
  df_clean_forced <- df_clean_abuse %>%
    mutate_at(vars(forcedsexyrfreq), as.character) %>%
    mutate(forcedsexyr = case_when(forcedsexyrby == "husband" ~"yes",
                                   forcedsexyrby == "exhusband" ~"yes",
                                   forcedsexyrby == "boyfriend" ~"yes",
                                   forcedsexyrby == "stranger" ~"yes",
                                   is.na(forcedsexyr) ~ "refused",
                                   TRUE ~forcedsexyr),
           forcedsexyrby = case_when(forcedsexyr == "no" ~"not_forcedsexyr",
                                     forcedsexyr == "yes" & is.na(forcedsexyrby) ~"yes, refused",
                                     forcedsexyr == "yes" & forcedsexyrby == "r" ~"yes, refused",
                                     forcedsexyr == "refused" ~"refused",
                                     forcedsexyrby == "dk" ~"don't know",
                                     TRUE ~forcedsexyrby
           ),
           forcedsexyrfreq = case_when(forcedsexyr == "no" ~"not_forcedsexyr",
                                       forcedsexyrfreq == "888" ~"don't know",
                                       #forcedsexyrfreq == "999" ~"refused",
                                       forcedsexyr == "yes" & is.na(forcedsexyrfreq) ~"yes, refused",
                                       forcedsexyr == "refused" ~"refused",
                                       TRUE ~forcedsexyrfreq)
    )
  
  # ==== 16) Clean Afraid ====
  df_clean_afraid <- df_clean_forced %>%
    mutate(patientafraidofanyone = ifelse(patientafraidofanyone=="other",
                                          patientafraidofother,
                                          patientafraidofanyone)) %>%
    select(-patientafraidofother) %>%
    mutate(patientafraidofanyone = case_when(patientafraidofanyone == "rna" | patientafraidofanyone == "r" | patientafraidofanyone == "ena" ~"refused",
                                             patientafraidofanyone == "dk" ~"don't know",
                                             patientafraidofanyone == "No" | patientafraidofanyone == "None" | patientafraidofanyone == "afraid of no one" | patientafraidofanyone == "NONE" | patientafraidofanyone == "none" ~"not_afraid",
                                             patientafraidofanyone == "Did not specify" | patientafraidofanyone == "other but has not specified"| patientafraidofanyone == " But Not specified" ~"yes, refused",
                                             patientafraidofanyone == "5" | patientafraidofanyone == "6" | patientafraidofanyone == "DIfferent kinds of people" ~"many",
                                             TRUE ~patientafraidofanyone
    )) %>%
    mutate(afraid = case_when(afraid == "rna" | afraid == "r" ~"refused",
                              afraid == "dk" ~"don't know",
                              is.na(afraid) & is.na(patientafraidofanyone) ~"refused",
                              patientafraidofanyone == "yes, refused" | patientafraidofanyone == "mother" | patientafraidofanyone == "many" ~"yes",
                              is.na(afraid)& patientafraidofanyone == "refused" ~"refused",
                              is.na(afraid)& patientafraidofanyone == "husband" ~"yes",
                              is.na(afraid)& patientafraidofanyone == "exhusband" ~"yes",
                              is.na(afraid)& patientafraidofanyone == "stranger" ~"yes",
                              is.na(afraid)& patientafraidofanyone == "boyfriend" ~"yes",
                              is.na(afraid)& patientafraidofanyone == "not_afraid" ~"no",
                              is.na(afraid)& patientafraidofanyone == "don't know" ~"don't know",
                              TRUE ~afraid),
           patientafraidofanyone = case_when(afraid == "yes" & is.na(patientafraidofanyone) ~"yes, refused",
                                             afraid == "no" ~"not_afraid",
                                             afraid == "refused" ~"refused",
                                             afraid == "don't know" ~"don't know",
                                             TRUE ~patientafraidofanyone
                                             
           ))
  
  # ==== 17) Clean Clubs ====
  df_clean_club <- df_clean_afraid %>%
    mutate(haspatienteverparticipatedinaclu = ifelse(is.na(haspatienteverparticipatedinaclu) | haspatienteverparticipatedinaclu == "response not available",
                                                     "refused",
                                                     haspatienteverparticipatedinaclu
    )) %>%
    mutate(timeofpatientparticipateinclub = case_when(haspatienteverparticipatedinaclu=="no" ~"never_girlclub",
                                                      haspatienteverparticipatedinaclu=="yes a go girls club" & is.na(timeofpatientparticipateinclub) ~"yes, refused",
                                                      haspatienteverparticipatedinaclu=="yes a different club" & is.na(timeofpatientparticipateinclub) ~"yes, refused",
                                                      haspatienteverparticipatedinaclu=="refused" & is.na(timeofpatientparticipateinclub) ~"refused",
                                                      haspatienteverparticipatedinaclu=="don't know" & is.na(timeofpatientparticipateinclub) ~"don't know",
                                                      TRUE ~timeofpatientparticipateinclub
    )) %>%
    mutate(haspatienteverparticipatedinmoth = case_when(haspatienteverparticipatedinmoth == "dk" ~"don't know",
                                                        haspatienteverparticipatedinmoth == "rna" | is.na(haspatienteverparticipatedinmoth) ~"refused",
                                                        TRUE ~haspatienteverparticipatedinmoth))
  
  # ==== 18) Final DF ====
  final_df <- df_clean_club %>%
    mutate(lag_rita_res = ifelse(lag_rita_res==1,
                                 "recent",
                                 "not recent"
    ))
  
  
  rm(list= ls()[! (ls() %in% c('final_df','df_raw'))])


#/////////////////////////////////////////////////////////////////////////
# ------------------------------ Imputation ------------------------------
#/////////////////////////////////////////////////////////////////////////

# ==== 1) Prepare DF ====
which(is.na(final_df))
#is.na(final_df)

final_df <- final_df %>%
  mutate_at(vars(agefirstsex), as.numeric) %>%
  mutate(agefirstsex = ifelse(is.na(agefirstsex),
                              round(mean(as.numeric(final_df$agefirstsex), na.rm = T)),
                              agefirstsex))  %>%
  select(-ANCsite) %>%
  mutate_at(vars(-c(age, agefirstsex)),
            as.factor)

which(is.na(final_df))
} # do  

# final_df[] <- lapply(final_df, factor)
# final_df[,"age"] <- as.numeric(final_df[,"age"]) 
# final_df[,"agefirstsex"] <- as.numeric(final_df[,"agefirstsex"]) 
summary(final_df)

#openxlsx::write.xlsx(final_df, "MWI_RecencyClean.xlsx")
#str(final_df)


#/////////////////////////////////////////////////////////////////////////
# ------------------------- Explore Recency Data -------------------------
#/////////////////////////////////////////////////////////////////////////

library(MXM)
library(bnlearn)
library(bnviewer)
library("FactoMineR")
library("factoextra")
library(tidymodels)
library(ggplot2)



for (i in 1:58){
  print(colnames(final_df[i]))
  print(table(final_df[,i], final_df$lag_rita_res))
  print(chisq.test(final_df[,i], final_df$lag_rita_res))
  print("~~~~~~~~~~~~~~~~~~~~~~~")
}

#plot(table(final_df$occupation, final_df$lag_rita_res))

# ==== 2) Bayesian Network ====
final_dffac <- data.frame(lapply(final_df, factor))
final_dffac <- final_dffac %>% 
  select(c(ancdistrict, 
           occupation, 
           educationlvl, 
           evermarried,
           everHIVtest,
           agefirstsex,
           forcedfirstsex,
           sexptnrlife,
           ptnrHIVstat,
           ptnragedif,
           circumtype,
           gifts4sex,
           pregnancies,
           condompreg,
           STIsym,
           alcbingefreq,
           everdrugs,
           everabuseby,
           afraid,
           haspatienteverparticipatedinaclu,
           lag_rita_res
           ))
bn.gs <- gs(final_dffac)
bn.gs
bn2 <- iamb(final_dffac)
bn3 <- fast.iamb(final_dffac)
bn4 <- inter.iamb(final_dffac)

compare(bn.gs, bn2)
compare(bn.gs, bn3)
compare(bn.gs, bn4)

bn.hc <- hc(final_dffac, score = "aic")
bn.hc <- hc(final_dffac)
bn.hc

compare(bn.hc, bn.gs)


plot(bn.hc)

library(Rgraphviz)
library(visNetwork)

graphviz.plot(bn.gs)
graphviz.plot(bn.hc)
viewer(bn.hc,
       bayesianNetwork.width = "100%",
       bayesianNetwork.height = "80vh",
       bayesianNetwork.layout = "layout_in_circle",
       bayesianNetwork.title="Discrete Bayesian Network",
       bayesianNetwork.subtitle = "AGYW Malawi Recency",
       bayesianNetwork.footer = "Fig. 1 - Layout in circle",
       
       node.colors = list(background = "red",
                          border = "black",
                          highlight = list(background = "black",
                                           border = "red"))
)

viewer(bn.hc,
       bayesianNetwork.width = "100%",
       bayesianNetwork.height = "80vh",
       bayesianNetwork.layout = "layout_on_grid",
       bayesianNetwork.title="Discrete Bayesian Network",
       bayesianNetwork.subtitle = "AGYW Malawi Recency",
       bayesianNetwork.footer = "Fig. 1 - Layout on grid",
       
       node.colors = list(background = "#f4bafd",
                          border = "#2b7ce9",
                          highlight = list(background = "#97c2fc",
                                           border = "#2b7ce9"))
       
)

#plot(bn.hc)
modelstring(bn.hc)


# ==== 3) RandomForest ====
library(tidypredict)
library(randomForest)
library(party)
set.seed(415)

data_split <- initial_split(final_df, strata = 'lag_rita_res')

train <- training(data_split)
test <- testing(data_split)

nrow(test)/nrow(train)


# Random forest model ----
model1 <- randomForest(lag_rita_res ~ ., data = train, importance=T, proximity=T)
model1

#predict classes on test data
pred <- predict(model1, newdata = test[,-58], type = "response")

table(test$lag_rita_res, pred)


fit <- randomForest(lag_rita_res ~ ., data = final_df,
                    importance=TRUE, 
                    ntree=8000)
fit
varImpPlot(fit)
model <- randomForest(lag_rita_res ~ occupation + 
                        haspatienteverparticipatedinaclu + sexptnrlife + ptnragedif + alcbingefreq,
                      data = final_df)
model
fit2 <- cforest(lag_rita_res ~ .,
                data = final_df,
                controls=cforest_unbiased(ntree=2000, mtry=3))
fit2
#summary(final_df)

