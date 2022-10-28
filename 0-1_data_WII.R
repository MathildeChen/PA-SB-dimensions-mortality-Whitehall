# Script: 0-1_data_WII.R
# Author: M. Chen, Inserm
# Date: 2022

# Prepare data from Whitehall II accelerometer sub-study

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

#---------
# Data
# > Set of participants with valid accelerometer data
sample_stno <- read_dta("E:/PC_FIXE/Data/04_DAILY_ACT_SUM/2021-05-03/data_03052021.dta") %>% select(stno)
dim(sample_stno) 

# > Mortality status & covariates
data_mort_cov <- read.csv("E:/PC_FIXE/Analysis/04_MLpaper/00_DATA/data_mortality.csv") 

dim(data_mort_cov) 
names(data_mort_cov)
# --> outcomes: Mortality variables: 
# "stat0221_c": mortality status labels (ALIVE/DEAD)
# "e_21ac":     mortality status integer (0/1) -from phase 1 to death or to follow-up (28th Feb 2021)
# "ef21ac":     mortality status integer (0/1) -from phase 11 to death or to follow-up (28th Feb 2021)
# "pf21ac":     number years between entry into the study until death or end of follow-up
# "py0":        age (years) at entry into the study
# "py":         age (years) at death or end of follow-up 
# "fmm_index_acm_2", "acm_0319", and "end_acm_0319": data from earliest follow-up (2019)

# > ADL & iADL
#ADL = "ADL_dressing" ("fadl1"), "ADL_walking" ("fadl2"), "ADL_bathing" ("fadl3"), "ADL_eating" ("fadl4"), "ADL_bed" ("fadl5"), "ADL_toilet" ("fadl6")
#IADL = "ADL_cooking" ("fadl8"),"ADL_shopping" ("fadl9"), "ADL_telephon" ("fadl10"),"ADL_medication ("fadl11")", "ADL_housework" ("fadl12"), "ADL_managing" ("fadl13")

# Data from screening and questionnaires
# > phase 12
tab_s12 <- read_dta("E:\\PC_FIXE\\Data\\05_WHITEHALL\\s12quest.dta") %>% 
  filter(stno %in% unique(sample_stno$stno))

# > phase 11 
tab_s11 <- read_dta("E:\\PC_FIXE\\Data\\05_WHITEHALL\\s11quest.dta") %>%
  left_join(read.csv("E:\\PC_FIXE\\Data\\05_WHITEHALL\\S11screen_txt.csv"),
            by = "stno") %>% 
  filter(stno %in% unique(sample_stno$stno))

# > phase 9 (for imputation) 
tab_s9 <- read_dta("E:\\PC_FIXE\\Data\\05_WHITEHALL\\s9quest.dta") %>% 
  left_join(read_dta("E:\\PC_FIXE\\Data\\05_WHITEHALL\\s9screen.dta"),
            by = "stno") %>% 
  filter(stno %in% unique(sample_stno$stno))

# > phase 8
tab_s8 <- read_dta("E:\\PC_FIXE\\Data\\05_WHITEHALL\\s8quest.dta") %>% 
  filter(stno %in% unique(sample_stno$stno))

# Covariables from phase 12
datas12a <- tab_s12 %>%
  select(stno, starts_with("dadl"), -dadl7, -dadl14, -dadl15, -dadl16, -starts_with("dadlhlp")) %>%
  gather(key = "variable", value = "dvalue", -stno) %>% 
  mutate(variable = substring(variable, 2))

# Covariables from phase 11
datas11a <- tab_s11 %>% 
  select(stno, starts_with("fadl"), -fadl7, -fadl14, -fadl15, -fadl16) %>%
  gather(key = "variable", value = "fvalue", -stno) %>% 
  mutate(variable = substring(variable, 2))

# Covariables from phase 9
datas9a <- tab_s9 %>%
  select(stno, starts_with("jadl"), -jadl7) %>%
  gather(key = "variable", value = "jvalue", -stno) %>% 
  mutate(variable = substring(variable, 2))

# Covariables from phase 8
datas8a <- tab_s8 %>%
  select(stno, starts_with("kadl"), -kadl7) %>%
  gather(key = "variable", value = "kvalue", -stno) %>% 
  mutate(variable = substring(variable, 2))

data_tempa <- datas11a %>% 
  left_join(datas9a, by = c("stno", "variable")) %>% 
  left_join(datas12a, by = c("stno", "variable")) %>% 
  left_join(datas8a, by = c("stno", "variable"))

data_impa <- data_tempa %>% 
  # Imputation
  mutate(fvalue_i = if_else(is.na(fvalue) == F, fvalue, jvalue)) %>% 
  mutate(fvalue_i = if_else(is.na(fvalue_i) == F, fvalue_i, dvalue)) %>% 
  mutate(fvalue_i = if_else(is.na(fvalue_i) == F, fvalue_i, kvalue)) %>%
  select(-fvalue, -jvalue, -dvalue, -kvalue) %>%
  # Data formating: Get one column for each imputed variable 
  pivot_wider_spec(
    # > specification for name of the columns
    tibble(
      .name = c(paste0("f", unique(data_tempa$variable), "_i")),
      .value = c(rep("fvalue_i", 12)),
      variable = unique(data_tempa$variable))
  ) 

# Remaining 2 missing data
data_impa %>% 
  mutate_at(.vars = vars(matches("fadl")) , .funs = ~ { if_else(.x == 1, 1, 0)} ) %>% 
  filter_all(any_vars(is.na(.)==T)) 

#    stno fadl1_i fadl2_i fadl3_i fadl4_i fadl5_i fadl6_i fadl8_i fadl9_i fadl10_i fadl11_i fadl12_i fadl13_i
#1  10422       0       0       0       0       0       0       0       0        0       NA        0        0
#2 190767       0       0       0       0       0       0       0       0        0       NA        0        0

# -> change them into 0, as all are 0 
data_impa <- data_impa %>% 
  mutate(fadl11_i = if_else(is.na(fadl11_i)==T, 0, fadl11_i))

# Compute ADL and iADL scores
data_impa <- data_impa %>% 
  mutate_at(.vars = vars(matches("fadl")) , .funs = ~ { if_else(.x == 1, 1, 0)} ) %>% 
  group_by(stno) %>%
  # Number of ADL and iADL
  mutate(fadl_tot  = fadl1_i + fadl2_i + fadl3_i + fadl4_i + fadl5_i + fadl6_i,
         ifadl_tot = fadl8_i + fadl9_i + fadl10_i + fadl11_i + fadl12_i + fadl13_i) %>% 
  # Having at least 1 ADL or 1 iADL
  mutate(fadl_tot_2 = if_else(fadl_tot > 0, 1, 0),
         ifadl_tot_2 = if_else(ifadl_tot > 0, 1, 0))

# > Accelerometer predictors from GGIR (2.4.1 version)
# Person level summary
data_acc <-  read.csv("E:/PC_FIXE/Data/04_DAILY_ACT_SUM/2021-07-12/part5_personsummary_WW_L40M100V400_T5A5.csv") 

# Data for participants with *valid* accelerometer data 
# and selected set of covariates
data_acc <- data_acc %>%
  rename("stno" = "ID") %>%
  filter(stno %in% unique(sample_stno$stno)) %>%
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
  # > Derive mean duration of MOD and VIG bouts
  mutate(FRAG_mean_dur_MOD_day_wei = if_else(Nblocks_day_total_MOD_wei == 0, 0, dur_day_total_MOD_min_wei / Nblocks_day_total_MOD_wei),
         FRAG_mean_dur_VIG_day_wei = if_else(Nblocks_day_total_VIG_wei == 0, 0, dur_day_total_VIG_min_wei / Nblocks_day_total_VIG_wei)) %>% 
  # > Rename 
  rename("FRAG_Nfrag_MOD_day_wei" = "Nblocks_day_total_MOD_wei", 
         "FRAG_Nfrag_VIG_day_wei" = "Nblocks_day_total_VIG_wei") %>%
  # > Select appropriate variables for all days (_wei, weighted estimates), weekd days (_WD), weekend days (_WE)
  dplyr::select(stno,
                # > Waking time and sleep duration
                dur_day_min_wei, dur_day_min_WD, dur_day_min_WE,
                dur_spt_min_wei,
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
                ig_intercept_wei,             ig_intercept_WD,             ig_intercept_WE,
                # > Moderate and Vigorous 
                dur_day_total_MOD_min_wei, dur_day_total_VIG_min_wei,
                FRAG_Nfrag_MOD_day_wei, FRAG_Nfrag_VIG_day_wei,
                FRAG_mean_dur_MOD_day_wei, FRAG_mean_dur_VIG_day_wei)

# Day-level summary
data_acc_day <- read.csv("E:/PC_FIXE/Data/04_DAILY_ACT_SUM/2021-07-12/part5_daysummary_WW_L40M100V400_T5A5.csv") 

# Create new variable: number of days with >=30 min MVPA
data_acc_day_nb_days <- data_acc_day %>%
  rename("stno" = "ID") %>%
  filter(stno %in% unique(sample_stno$stno)) %>%
  mutate(dur_day_total_MVPA_min = dur_day_total_MOD_min + dur_day_total_VIG_min) %>% 
  select(stno, window_number, daytype, dur_day_total_MVPA_min) %>% 
  mutate(no_meet_MVPA_guidelines = if_else(dur_day_total_MVPA_min < 30, 1, 0)) %>% 
  group_by(stno, daytype) %>% 
  summarise(n_days = n(),
            n_days_no_meeting_guidelines = sum(no_meet_MVPA_guidelines == 1)) %>% 
  mutate(n_days_meeting_guidelines = n_days - n_days_no_meeting_guidelines) %>%
  ungroup() %>%
  mutate(n_round = if_else(daytype == "WD", 
                           round(as.numeric(as.character(n_days_meeting_guidelines))*5/as.numeric(as.character(n_days))), 
                           as.numeric(as.character(n_days_meeting_guidelines)))) %>% 
  group_by(stno) %>% 
  summarise(n_days_meeting_guidelines_wei = sum(n_round))

# Add the variable with number of days to the data 
data_acc <- data_acc %>% 
  left_join(data_acc_day_nb_days, by = "stno")

#---------
# Build dataset
# > Select participants with valid data
data <- data_mort_cov %>%
  # recode fstatusx_i_2 variable to have fstatusx_i_2=1 corresponding to not married/cohabitating
  mutate(fstatusx_i_2 = if_else(fstatusx_i_2==0, 1, 0)) %>%
  # declare categorical variables as factors
  mutate(sex                 = as.factor(sex),
         ethnicity           = as.factor(ethnicity),  
         fstatusx_i_2        = as.factor(fstatusx_i_2),
         flgrlump_i          = as.factor(flgrlump_i),
         recentex_cur_smoker = as.factor(recentex_cur_smoker),
         funitwk0_i_3        = as.factor(funitwk0_i_3),
         ffruitvg_i_3        = as.factor(ffruitvg_i_3),
         fbmi_i_3            = as.factor(fbmi_i_3),
         hypertension        = as.factor(hypertension),
         ldl_4_1_drgs        = as.factor(ldl_4_1_drgs),
         diabetes_prevalent  = as.factor(diabetes_prevalent)) %>% 
  # create binary variables for factors with more than 2 levels
  mutate(funitwk0_i_3_1        = if_else(funitwk0_i_3 == 0, 1, 0),                   # no alcohol intake
         funitwk0_i_3_2        = if_else(funitwk0_i_3 == 2, 1, 0),                   # high alcohol intake
         recentex_cur_smoker_1 = if_else(recentex_cur_smoker == 1, 1, 0),            # long-term ex smokers
         recentex_cur_smoker_2 = if_else(recentex_cur_smoker == 2, 1, 0),            # recent ex or current smokers
         fbmi_i_3_1            = if_else(fbmi_i_3 == 1, 1, 0),                       # overweight
         fbmi_i_3_2            = if_else(fbmi_i_3 == 2, 1, 0),                       # obese
         ffruitvg_i_3_1        = if_else(ffruitvg_i_3 == 0, 1, 0),                   # <daily fg intake
         ffruitvg_i_3_2        = if_else(ffruitvg_i_3 == 1, 1, 0),                   # =daily fg intake
         flgrlump_i_1          = if_else(flgrlump_i == "2: prof/exec", 1, 0),        # prof/exec
         flgrlump_i_2          = if_else(flgrlump_i == "3: clerical/support", 1, 0), # clerical/support
  ) %>%
  # convert dates into date format
  mutate(dateob_c_d     = parse_date_time2(as.character(dateob_c), "dmy", cutoff_2000 = 0),
         fdatscrn_i_d   = dmy(fdatscrn_i),
         end_cvd2_d     = dmy(end_cvd2),
         end_acm_0319_d = dmy(end_acm_0319)) %>% 
  # strata
  mutate(dateob_c_d_str = year(floor_date(dateob_c_d, years(5)))) %>% 
  # age standardization (to present association per 5 years increase)
  mutate(z_fage_s_5 = (fage_s - mean(fage_s))/5) %>% 
  # join to accelerometer features
  left_join(data_acc, by = "stno") %>%    
  # accelerometer variable standardization
  mutate(across(.cols = ends_with(c("_wei", "_WD", "_WE")), 
                .fns = list(z = ~scale(.)),
                .names = "{fn}_{col}")) %>% 
  # special treatment for waking time, centered on 16 hours
  mutate(z_dur_day_min_wei = dur_day_min_wei- 16*60,
         z_dur_day_min_WD = dur_day_min_WD- 16*60,
         z_dur_day_min_WE = dur_day_min_WE- 16*60) %>% 
  # date for sensitivity analysis excluding those died in the first 2 y of fup
  mutate(reverse_ef21ac = if_else(pf21ac<2 & ef21ac==1, 1, 0))

# Add ADL and IADL to the full data set
data <- data %>% left_join(data_impa, by="stno")

# > Save dataset
write.csv(x = data, file = "E:/PC_FIXE/Analysis/04_MLpaper/02_PLS/PLS_mathilde_2/0_data/data_WII.csv", row.names = F)

