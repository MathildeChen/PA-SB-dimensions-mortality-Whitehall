# Script: 0-2_data_CoLaus.R
# Author: M. Chen, Inserm
# Date: 2022

# Prepare data from CoLaus study

#---------
# > Set local language as english to be able to read dates properly
Sys.setlocale("LC_ALL","English")

#---------
# packages
library(haven)
library(tidyr)
library(dplyr)
library(purrr)
library(lubridate)
library(broom)
library(testthat)

#---------
# data 
# Accelerometer variables from GGIR (2.4.1 version)
# > Person-level summary
data_acc <- read.csv("H:/data from pedro/Stats/GGIR results/part5_personsummary_WW_L40M100V400_T5A5.csv")

# /!\ some participants with repeated GGIR outputs
data_acc %>% filter(ID %in% c("5556", "5905", "6213")) %>% select(ID, filename)

# /!\ some participants with problematic ID 
data_acc[which(nchar(data_acc$ID)>4),"ID"]

# /!\ some participants that shoud be removed ("the 7000" group: dement people that were included in the study)
data_acc <- data_acc %>% 
  # > remove 7000 group
  filter(!(ID %in% c("7006","7056","7108","7131","7137","7165","7173","7178","7180","7181"))) %>%
  # > remove repetition  
  filter(filename != "5556__031476_2016-06-06 11-43-31.bin.RData") %>%
  filter(filename != "5905__031507_Colaus.bin.RData") %>%
  filter(filename != "6213__020051_2017.bin.RData") %>% 
  # > rename problematic ID and transform it as number
  mutate(ID = as.numeric(as.character(substr(ID, 1, 4))))

# > Day-level summary
data_acc_day <- read.csv("H:/data from pedro/Stats/GGIR results/part5_daysummary_WW_L40M100V400_T5A5.csv")  

data_acc_day <- data_acc_day  %>% # > remove 7000 group
  filter(!(ID %in% c("7006","7056","7108","7131","7137","7165","7173","7178","7180","7181"))) %>%
  # > remove repetition  
  filter(filename != "5556__031476_2016-06-06 11-43-31.bin.RData") %>%
  filter(filename != "5905__031507_Colaus.bin.RData") %>%
  filter(filename != "6213__020051_2017.bin.RData") %>% 
  # > rename problematic ID and transform it as number
  mutate(ID = as.numeric(as.character(substr(ID, 1, 4))))

# > check number of days of data per participant
# >> total number of days
test <- data_acc_day %>%
  rename("stno" = "ID") %>% 
  group_by(stno) %>% 
  summarise(n_days_total = n()) ; table(test$n_days_total, exclude = NULL)
# > some participants with less than 4 days of data (n=50)

# >> number of WE and WD days
test2 <- data_acc_day %>%
  rename("stno" = "ID") %>%
  group_by(stno, daytype) %>% 
  summarise(n_days = n()); table(test2$daytype, test2$n_days, exclude=NULL)
# > some participants with less than 2 days of WE less than 2 days of WD

# >> identify participants with not enough data 
test3 <- data_acc_day %>%
  rename("stno" = "ID") %>%
  group_by(stno, daytype) %>% 
  summarise(n_days = n()) %>% 
  group_by(stno) %>% 
  mutate(n_days_total = sum(n_days)) %>% 
  # > participants with not enough data (less than 2 days of WE less than 2 days of WD)
  mutate(exclude_0 = if_else(n_days_total < 4 | n_days < 2, 1, 0),
         exclude_1 = if_else(sum(exclude_0) != 0 | stno %in% c("3994", "4035", "4764"), 1, 0))

# > participants with (1) less than 4 days of data, or (2) less than 2 days of WD, or (3) less than 2 days of WE
stno_exclude_1 <- unique(test3$stno[which(test3$exclude_1==1)]); length(stno_exclude_1) # 106 participants

# > participants with enough data
stno_keep_1 <- unique(test3$stno[which(test3$exclude_1==0)]); length(stno_keep_1) # 2919 participants

# Covariates, outcomes and health events
data_cov <- read.csv("H:/data from pedro/Sabia_data_5.csv", sep=";")

#---------
# Data for participants with accelerometer data 

# > Person summary variables
data_acc <- data_acc %>%
  rename("stno" = "ID") %>%
  # > Select participants with enough data 
  filter(stno %in% stno_keep_1) %>%
  # > Derive total duration in MVPA (total duration MOD + total duration VIG)
  mutate(dur_day_total_MVPA_min_wei = dur_day_total_MOD_min_wei + dur_day_total_VIG_min_wei,
         dur_day_total_MVPA_min_WD  = dur_day_total_MOD_min_WD  + dur_day_total_VIG_min_WD,
         dur_day_total_MVPA_min_WE  = dur_day_total_MOD_min_WE  + dur_day_total_VIG_min_WE) %>%
  # > Derive time spent in <10 MVPA bouts (total duration of MVPA - time in >10 min MVPA bouts)
  mutate(dur_day_MVPA_unbt_min_wei = dur_day_total_MVPA_min_wei - dur_day_MVPA_bts_10_min_wei,
         dur_day_MVPA_unbt_min_WD = dur_day_total_MVPA_min_WD - dur_day_MVPA_bts_10_min_WD,
         dur_day_MVPA_unbt_min_WE = dur_day_total_MVPA_min_WE - dur_day_MVPA_bts_10_min_WE) %>% 
  # > Derive number of unbouted fragments of MVPA (unbouted MOD + VIG)
  mutate(Nblocks_day_MVPA_unbt_wei = Nblocks_day_total_MOD_wei + Nblocks_day_total_VIG_wei,
         Nblocks_day_MVPA_unbt_WD  = Nblocks_day_MOD_unbt_WD   + Nblocks_day_VIG_unbt_WD,
         Nblocks_day_MVPA_unbt_WE  = Nblocks_day_MOD_unbt_WE   + Nblocks_day_VIG_unbt_WE) %>% 
  # > Correct M5TIME
  mutate(M5TIME_num_wei = if_else(M5TIME_num_wei>24, M5TIME_num_wei-12, M5TIME_num_wei),               
         M5TIME_num_WD = if_else(M5TIME_num_WD>24, M5TIME_num_WD-12, M5TIME_num_WD),                            
         M5TIME_num_WE = if_else(M5TIME_num_WE>24, M5TIME_num_WE-12, M5TIME_num_WE)) %>% 
  # > Select appropriate variables for all days (_wei, weighted estimates), weekd days (_WD), weekend days (_WE)
  dplyr::select(stno,
                # > Waking time
                dur_day_min_wei, dur_day_min_WD, dur_day_min_WE,
                # > Mean acceleration
                ACC_day_mg_wei,               ACC_day_mg_WD,               ACC_day_mg_WE,
                # > Time in SB, LIPA and MVPA
                dur_day_total_IN_min_wei,     dur_day_total_IN_min_WD,     dur_day_total_IN_min_WE, 
                dur_day_total_LIG_min_wei,    dur_day_total_LIG_min_WD,    dur_day_total_LIG_min_WE, 
                dur_day_total_MVPA_min_wei,   dur_day_total_MVPA_min_WD,   dur_day_total_MVPA_min_WE,
                # > Total number of bouts in SB, LIPA and MVPA
                FRAG_Nfrag_IN_day_wei,        FRAG_Nfrag_IN_day_WD,        FRAG_Nfrag_IN_day_WE,
                FRAG_Nfrag_LIPA_day_wei,      FRAG_Nfrag_LIPA_day_WD,      FRAG_Nfrag_LIPA_day_WE,
                FRAG_Nfrag_MVPA_day_wei,      FRAG_Nfrag_MVPA_day_WD,      FRAG_Nfrag_MVPA_day_WE,
                # > Number of fragments of different lengths in SB, LIPA and MVPA
                Nblocks_day_IN_unbt_wei,      Nblocks_day_IN_unbt_WD,      Nblocks_day_IN_unbt_WE,
                Nblocks_day_IN_bts_10_30_wei, Nblocks_day_IN_bts_10_30_WD, Nblocks_day_IN_bts_10_30_WE,
                Nblocks_day_IN_bts_30_wei,    Nblocks_day_IN_bts_30_WD,    Nblocks_day_IN_bts_30_WE, 
                Nblocks_day_LIG_unbt_wei,     Nblocks_day_LIG_unbt_WD,     Nblocks_day_LIG_unbt_WE,
                Nblocks_day_LIG_bts_10_wei,   Nblocks_day_LIG_bts_10_WD,   Nblocks_day_LIG_bts_10_WE,
                Nblocks_day_MVPA_unbt_wei,    Nblocks_day_MVPA_unbt_WD,    Nblocks_day_MVPA_unbt_WE,
                Nblocks_day_MVPA_bts_10_wei,  Nblocks_day_MVPA_bts_10_WD,  Nblocks_day_MVPA_bts_10_WE,
                # > Mean duration of bouts in SB, LIPA and MVPA
                FRAG_mean_dur_IN_day_wei,     FRAG_mean_dur_IN_day_WD,     FRAG_mean_dur_IN_day_WE, 
                FRAG_mean_dur_LIPA_day_wei,   FRAG_mean_dur_LIPA_day_WD,   FRAG_mean_dur_LIPA_day_WE,
                FRAG_mean_dur_MVPA_day_wei,   FRAG_mean_dur_MVPA_day_WD,   FRAG_mean_dur_MVPA_day_WE,
                # > Time spent in <10 min bouts of SB, LIPA and MVPA 
                dur_day_IN_unbt_min_wei,     dur_day_IN_unbt_min_WD,       dur_day_IN_unbt_min_WE,
                dur_day_LIG_unbt_min_wei,    dur_day_LIG_unbt_min_WD,      dur_day_LIG_unbt_min_WE, 
                dur_day_MVPA_unbt_min_wei,   dur_day_MVPA_unbt_min_WD,     dur_day_MVPA_unbt_min_WE,
                # > Time spent in >10 min bouts of SB, LIPA and MVPA
                dur_day_IN_bts_10_30_min_wei,dur_day_IN_bts_10_30_min_WD,  dur_day_IN_bts_10_30_min_WE,
                dur_day_IN_bts_30_min_wei,   dur_day_IN_bts_30_min_WD,     dur_day_IN_bts_30_min_WE,
                dur_day_LIG_bts_10_min_wei,  dur_day_LIG_bts_10_min_WD,    dur_day_LIG_bts_10_min_WE,
                dur_day_MVPA_bts_10_min_wei, dur_day_MVPA_bts_10_min_WD,   dur_day_MVPA_bts_10_min_WE,
                # > Timing of the most 5 active hours
                M5TIME_num_wei,               M5TIME_num_WD,               M5TIME_num_WE,
                # > Activity gradient
                ig_gradient_wei,              ig_gradient_WD,              ig_gradient_WE, 
                ig_intercept_wei,             ig_intercept_WD,             ig_intercept_WE) ; dim(data_acc) # 2919   85

# > Number of days with > 30 min of MVPA
data_acc_day_nb_days <- data_acc_day %>%
  rename("stno" = "ID") %>%
  # > keep participants with enough data
  filter(stno %in% stno_keep_1) %>%
  # > compute total MVPA duration
  mutate(dur_day_total_MVPA_min = dur_day_total_MOD_min + dur_day_total_VIG_min) %>% 
  # > identify days with > 30 min of MVPA
  select(stno, window_number, daytype, dur_day_total_MVPA_min) %>% 
  mutate(no_meet_MVPA_guidelines = if_else(dur_day_total_MVPA_min < 30, 1, 0)) %>% 
  # > compute the number of week days and weekend days
  group_by(stno, daytype) %>% 
  summarise(n_days = n(),
            n_days_no_meeting_guidelines = sum(no_meet_MVPA_guidelines == 1)) %>% 
  mutate(n_days_meeting_guidelines = n_days - n_days_no_meeting_guidelines) %>%
  # > compute the total number of days 
  group_by(stno) %>% 
  mutate(n_days_total = sum(n_days),
         n_week_total = if_else(n_days_total < 8, 1, 2)) %>% 
  # > compute rounded number of days meeting guideline per WE and WD, depending on the total number of days
  group_by(stno, daytype) %>%  
  mutate(n_round = if_else(n_days_total < 8  & daytype == "WD", round(as.numeric(as.character(n_days_meeting_guidelines))*5/as.numeric(as.character(n_days))),  # if one week of data, week days
                   if_else(n_days_total < 8  & daytype == "WE", as.numeric(as.character(n_days_meeting_guidelines)),                                            # if one week of data, weekend days
                   if_else(n_days_total >= 8 & daytype == "WD", round(as.numeric(as.character(n_days_meeting_guidelines))*10/as.numeric(as.character(n_days))), # if 2 weeks of data, week days
                   round(as.numeric(as.character(n_days_meeting_guidelines))*4/as.numeric(as.character(n_days))))))) %>%                                        # if 2 weeks of data, weekend days
  # > compute number of days with > 30min of MVPA over the full wearing period
  ungroup() %>%
  group_by(stno) %>% 
  summarise(n_week_total = unique(n_week_total),
            n_days_meeting_guidelines_wei_full = sum(n_round)) %>% 
  # > report at same scale as WII data (1 week)
  mutate(n_days_meeting_guidelines_wei = round(n_days_meeting_guidelines_wei_full/n_week_total))

# > add the variable with number of days to the data 
data_acc <- data_acc %>% 
  left_join(data_acc_day_nb_days %>% select(stno, n_days_meeting_guidelines_wei), by = "stno")

#---------
# Covariates data cleaning
# Imputation of missing variables collected during 2nd FU with data from the first FU

# > F1 follow-up data
dataf1 <- data_cov %>% 
  select(pt, F1mrtsts2, 
         F1conso_hebdo, F1sbsmk, F1stop, F1Fruits, F1Fruits_OK, F1Vegetables, F1Vegetables_OK, 
         F1BMI, F1HTA, F1DIAB, F1DYSLIP, F1Quest3, F1Quest4, F1depressed) %>%
  gather(key = "variable", value = "f1value", -pt) %>% 
  mutate(variable = substring(variable, 3))

# > F2 follow-up data
dataf2 <- data_cov %>% 
  select(pt, F2mrtsts2, 
         F2conso_hebdo, F2sbsmk, F2stop, F2Fruits, F2Fruits_OK, F2Vegetables, F2Vegetables_OK,
         F2BMI, F2HTA, F2DIAB, F2DYSLIP, F2Quest3, F2Quest4, F2depressed)  %>%
  gather(key = "variable", value = "f2value", -pt) %>% 
  mutate(variable = substring(variable, 3))

# > F3 follow-up data
dataf3 <- data_cov %>% 
  select(pt, F3mrtsts2, 
         F3conso_hebdo, F3sbsmk, F3stop, F3Fruits, F3Fruits_OK, F3Vegetables, F3Vegetables_OK,
         F3BMI, F3HTA, F3DIAB, F3DYSLIP, F3Quest3, F3Quest4, F3depressed)  %>%
  gather(key = "variable", value = "f3value", -pt) %>% 
  mutate(variable = substring(variable, 3))

# > Merge 
data_temp <- left_join(dataf2, dataf1, by = c("pt", "variable")) %>% 
  left_join(., dataf3, by = c("pt", "variable"))

# > Imputation
data_imp <- data_temp %>% 
  mutate(f2value_i = if_else(is.na(f2value)==T, f1value, f2value)) %>% 
  mutate(f2value_i = if_else(is.na(f2value_i)==T, f3value, f2value_i)) %>% 
  select(-f2value, -f1value, -f3value) %>%
  # Data formating: Get one column for each imputed variable 
  pivot_wider_spec(
    # > specification for name of the columns
    tibble(
      .name = c(paste0("F2", unique(data_temp$variable), "_i")),
      .value = c(rep("f2value_i", 15)),
      variable = unique(data_temp$variable))
  ) 

# > Merge with initial dataset
data_cov_imputed <- left_join(data_cov, data_imp, by="pt") %>%
  rename("stno" = "pt") %>%
  filter(F2age >= 60) %>% 
  # > select participants with valid accelerometer data
  filter(stno %in% stno_keep_1)
dim(data_cov_imputed) # 1599    143

# > Create new variables based on imputed variables 
t <- data_cov_imputed %>% 
  # > date of screening at f1 and at f2
  mutate(F1datexam_d = dmy(F1datexam),
         F2datexam_d = dmy(F2datexam),
         F1datexam_d = if_else(is.na(F1datexam_d)==T, F2datexam_d, F1datexam_d), # some participants interviewed at F2 were not interviewed at F1
         F3datexam_d = dmy(F3datexam)) %>% 
  # > delay between F1 and F2,
  mutate(delayF1F2 = as.numeric(interval(F1datexam_d, F2datexam_d), "years"))  %>% 
  # > reverse sex to be consistent with WII (0=male, 1=female)
  mutate(F2_sex_reverse = recode(F2sex, "0"="1", "1"="0")) %>%
  # > some participants have missing ethnicity
  mutate(ethori_self_i = if_else(ethori_self =="" & stno > 9000, "X", if_else(ethori_self=="" & stno < 9000, "W", ethori_self))) %>%
  # > 3 no-educated participants have no information regarding education
  # > and reverse education gradient to make it consistent with WII (low value = lower education level)
  mutate(edtyp4_i = if_else(is.na(edtyp4) == TRUE, 4, as.numeric(as.character(edtyp4))),
         edtyp4_i_reverse = recode(edtyp4_i, "1"="4", "2"="3", "3"="2", "4"="1")) %>%
  # > Heart failure: self-reported data from baseline, F1 and F2
  mutate(chf_i = if_else(is.na(chf)==T, 0, as.numeric(as.character(chf))),
         F1chf_i = if_else(is.na(F1chf)==T, 0, as.numeric(as.character(F1chf))),
         F2chf_i = if_else(is.na(F2chf)==T | F2chf==9, 0, as.numeric(as.character(F2chf))),
         HF_prev_sr = if_else(chf_i==1 | F1chf_i==1 | F2chf_i==1, 1, 0)) %>%
  # > Difficulty in ADL and IADL, dicotomize variables
  mutate(F2Quest3_2 = if_else(F2Quest3_i !=1, 1, 0),
         F2Quest4_2 = if_else(F2Quest4_i !=1, 1, 0)) %>% 
  # > socio-demographic factors
  mutate(fage_s       = as.numeric(as.character(F2age)),                 # age at follow up 2
         sex          = as.factor(F2_sex_reverse),                       # sex (female=1)
         ethnicity    = as.factor(if_else(ethori_self_i=="W", 0, 1)),    # self-reported ethnicity (non-white=1)
         edu_conti    = as.numeric(as.character(edtyp4_i_reverse))+0.5,  # education level (lowest edu = lowest values)
         fstatusx_i_2 = as.factor(F2mrtsts2_i)) %>%                      # marital status (living alone=1)
  # > behavioral factors (except smoking, treated separately)
  mutate(no_smokers = if_else(F2sbsmk_i == 0 & F1sbsmk == 0, 1, 0),                                                      # never smokers
         ex_smokers = if_else(F2sbsmk_i == 1 & F1sbsmk == 1, 1, 0),                                                      # long-term ex-smokers
         current_smokers = if_else(F2sbsmk_i == 2 | F1sbsmk == 2 | F2sbsmk_i == 1 & F1sbsmk == 2 & delayF1F2 < 5,1, 0),  # current smoker or recent ex smokers 
         recentex_cur_smoker = if_else(no_smokers==1, 0, if_else(ex_smokers == 1, 1, 2)),                                
         funitwk0_i_3 = if_else(F2conso_hebdo_i==0, 0,                                                                   # 0 unit of alcohol
                                if_else(F2conso_hebdo_i > 15, 2,                                                                 # >14 units of alcohol
                                        1)),                                                                                             # 1-14 units of alcohol
         ffruitvg_i_3 = if_else(F2Fruits_i == 0 & F2Vegetables_i == 0, 0,                                                # < daily fg intake
                                if_else(F2Fruits_OK_i == 1 | F2Vegetables_OK_i == 1 | F2Fruits_i == 1 & F2Vegetables_i == 1, 2,  # more than daily fg intake (more than daily fruits, or more than daily veg, more at least daily fruits and daily veg)
                                        1))) %>%                                                                                         # daily fg intake
  # > health-related factors
  mutate(fbmi_i_3           = if_else(F2BMI_i <25, 0,   # normal (BMI<25)
                                      if_else(F2BMI_i >= 30, 2, # obese (BMI>30)
                                              1)),                      # overweight (25<=BMI<=30)
         hypertension       = as.factor(F2HTA_i),       # having hypertension
         ldl_4_1_drgs       = as.factor(F2DYSLIP_i),    # hyperlidemia or lowering drugs
         diabetes_prevalent = as.factor(F2DIAB_i),      # prevalent diabetes
         fadl_tot_2         = as.factor(F2Quest3_2),   
         ifadl_tot_2        = as.factor(F2Quest4_2)) %>% 
  # > compute multi morbidity index (continuous or dicotomous)
  group_by(stno) %>% 
  mutate(fmm_index_acm   = sum(c(AVC_prev, CHD_prev, F2depressed_i, HF_prev_sr), na.rm=T),
         fmm_index_acm_2 = if_else(AVC_prev == 1| CHD_prev == 1 | F1depressed == 1| HF_prev_sr == 1, 1, 0)) %>% 
  # create binary variables for factors with more than 2 levels
  mutate(funitwk0_i_3_1        = if_else(funitwk0_i_3 == 0, 1, 0),                   # no alcohol intake
         funitwk0_i_3_2        = if_else(funitwk0_i_3 == 2, 1, 0),                   # high alcohol intake
         recentex_cur_smoker_1 = if_else(recentex_cur_smoker == 1, 1, 0),            # long-term ex smokers
         recentex_cur_smoker_2 = if_else(recentex_cur_smoker == 2, 1, 0),            # recent ex or current smokers
         fbmi_i_3_1            = if_else(fbmi_i_3 == 1, 1, 0),                       # overweight
         fbmi_i_3_2            = if_else(fbmi_i_3 == 2, 1, 0),                       # obese
         ffruitvg_i_3_1        = if_else(ffruitvg_i_3 == 0, 1, 0),                   # <daily fg intake
         ffruitvg_i_3_2        = if_else(ffruitvg_i_3 == 1, 1, 0))                   # =daily fg intake

# Merge with accelerometer data and remove missing data 
data_cov_no_na <- t %>% 
  select(stno, 
         fage_s, sex, ethnicity, edu_conti, fstatusx_i_2, 
         F2sbsmk_i, funitwk0_i_3, #ffruitvg_i_3,
         fbmi_i_3, hypertension, ldl_4_1_drgs, diabetes_prevalent, fadl_tot_2, ifadl_tot_2, fmm_index_acm) %>% 
  left_join(data_acc, by="stno") %>% 
  drop_na(); dim(data_cov_no_na) # 2767  101

# > Outcome 
t_outcome <- t %>% 
  select(stno, F2datexam_d, any_death, datany_death, F3datexam_d, AVC_inc, AVC_date, CHD_inc, CHD_date) %>% 
  # > compute date of event (if death==1, date of event == date of death, else date of event == date of last visit)
  mutate(any_death = as.numeric(as.character(any_death)),
         datany_death_d = dmy(datany_death),
         AVC_date_d = dmy(AVC_date), 
         CHD_date_d = dmy(CHD_date),
         date_first_event = if_else(any_death == 0, max(c(F3datexam_d, AVC_date_d, CHD_date_d), na.rm=T), F3datexam_d),
         datevent_d = if_else(any_death == 1, datany_death_d, date_first_event)) %>% 
  # > compute the follow-up time between F2 and datevent_d
  mutate(pf21ac = as.numeric(interval(F2datexam_d, datevent_d), "years"),
         ef21ac = any_death)

# > Merge data
data_cov_outcome <- data_cov_no_na %>% 
  left_join(t_outcome, by="stno"); dim(data_cov_outcome)

dat0_colaus <- data_cov_outcome %>% 
  filter(pf21ac>0) 

# > Compute standardized variables (age and accelerometery variables )
mean_age <- mean(dat0_colaus$fage_s)

# > age, standardized on the mean and by 5 y increment 
dat0_colaus <- dat0_colaus %>% 
  mutate(z_fage_s_5 = (fage_s-mean_age)/5)

# > accelerometery variables standardized based on mean and sd in WII
dataWII <- read.csv("E:/PC_FIXE/Analysis/04_MLpaper/02_PLS/PLS_mathilde_2/0_data/data_WII.csv") %>% 
  select(starts_with(names(data_acc)))

# > compute mean and SD 
dataWII_mean_sd <- dataWII %>% 
  gather(key = "variable", value = "value", -stno) %>% 
  group_by(variable) %>% 
  summarise(mean_wii = mean(value),
            sd_wii = sd(value))

# > standardize based on WII data 
z_dat <- list()
for(i in names(data_acc)[-1])
{
  
  z_var_name <- paste0("z_", i)
  zvar <- as.numeric(as.character(scale(dat0_colaus[,paste0(i)], center = dataWII_mean_sd$mean_wii[which(dataWII_mean_sd$variable==i)], scale = dataWII_mean_sd$sd_wii[which(dataWII_mean_sd$variable==i)])))
  
  z_dat[[paste0(z_var_name)]] <- data.frame(stno = dat0_colaus$stno, z_var = zvar)
  
}

z_dat_acc <- plyr::ldply(z_dat, .id="var") %>% 
  spread(key="var", value = "z_var")

dat0_colaus <- left_join(dat0_colaus, z_dat_acc, by="stno"); dim(dat0_colaus) # 1956  202


# > Save dataset
write.csv(x = dat0_colaus, file = "E:/PC_FIXE/Analysis/04_MLpaper/02_PLS/PLS_mathilde_2/0_data/data_colaus.csv", row.names = F)











