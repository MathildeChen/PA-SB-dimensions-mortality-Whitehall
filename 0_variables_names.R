# Table with PA metrics' name

tab.name <- tibble::tribble(
  ~var,                              ~varname,
  # Average acceleration
  "ACC_day_mg_wei",                  "Mean acceleration (mg)",
  
  # Time in SB, LIPA and MVPA
  "dur_day_total_IN_min_wei",        "Total duration in SB (min/day)",
  "dur_day_total_LIG_min_wei",       "Total duration in LIPA (min/day)",
  "dur_day_total_MVPA_min_wei",      "Total duration in MVPA (min/day)",
  "dur_day_total_MOD_min_wei",       "Total duration in MPA (min/day)",
  "dur_day_total_VIG_min_wei",       "Total duration in VPA (min/day)",
  
  # Duration in unbouted SB, LIPA and MVPA
  "dur_day_IN_unbt_min_wei",        "Time in <10 min SB bouts (min/day)",
  "dur_day_LIG_unbt_min_wei",       "Time in <10 min LIPA bouts (min/day)",
  "dur_day_MVPA_unbt_min_wei",      "Time in <10 min MVPA bouts (min/day)",
  
  # Duration in bouts of SB, LIPA, MVPA
  "dur_day_IN_bts_10_30_min_wei",   "Time in 10-30 min SB bouts (min/day)",
  "dur_day_IN_bts_30_min_wei",      "Time in >=30 min SB bouts (min/day)",
  "dur_day_LIG_bts_10_min_wei",     "Time in >=10 min LIPA bouts (min/day)",
  "dur_day_MVPA_bts_10_min_wei",    "Time in >=10 min MVPA bouts (min/day)",
  
  # Nb of days meeting guidelines
  "n_days_meeting_guidelines_wei",  "Number of days with >= 30 min of MVPA",
  
  # Mean duration of fragments of SB, LIPA and MVPA
  "FRAG_mean_dur_IN_day_wei",         "Mean duration of SB bouts (min)",
  "FRAG_mean_dur_LIPA_day_wei",       "Mean duration of LIPA bouts (min)",
  "FRAG_mean_dur_MVPA_day_wei",       "Mean duration of MVPA bouts (min)",
  "FRAG_mean_dur_MOD_day_wei",        "Mean duration of MPA bouts (min)",
  "FRAG_mean_dur_VIG_day_wei",        "Mean duration of VPA bouts (min)",
  
  # Number of fragments of SB, LIPA and MVPA
  "FRAG_Nfrag_IN_day_wei",            "Number of SB bouts (N/day)",
  "FRAG_Nfrag_LIPA_day_wei",          "Number of LIPA bouts (N/day)",
  "FRAG_Nfrag_MVPA_day_wei",          "Number of MVPA bouts (N/day)",
  "FRAG_Nfrag_MOD_day_wei",           "Number of MPA bouts (N/day)",
  "FRAG_Nfrag_VIG_day_wei",           "Number of VPA bouts (N/day)",
  "Nblocks_day_IN_unbt_wei",          "Number of < 10 min SB bouts (N/day)",
  "Nblocks_day_IN_bts_10_30_wei",     "Number of 10-30 min SB bouts (N/day)",
  "Nblocks_day_IN_bts_30_wei",        "Number of > 30 min SB bouts (N/day)",
  "Nblocks_day_LIG_unbt_wei",         "Number of < 10 min LIPA bouts (N/day)",
  "Nblocks_day_LIG_bts_10_wei",       "Number of > 10 min LIPA bouts (N/day)",
  "Nblocks_day_MVPA_unbt_wei",        "Number of < 10 min MVPA bouts (N/day)",
  "Nblocks_day_MVPA_bts_10_wei",      "Number of > 10 min MVPA bouts (N/day)",
  
  # Timing of the most 5 active hours
  "M5TIME_num_wei",                   "Timing of physical activity",
  
  # Intensity gradient
  "ig_gradient_wei",                  "Intensity gradient", 
  "ig_intercept_wei",                 "Intensity intercept",
  
  # Socio-demographic, behavioral, health-related variables and other confounder
  "dur_day_min_wei", "Waking time",
  "z_fage_s_5" , "Age, per 5 years increment",
  "fage_s_5" , "Age, per 5 years increment",
  "sex" , "Female sex",
  "ethnicity" , "Non-white ethnicity",
  "edu_conti", "Education",
  "flgrlump_i_conti" , "Occupational position",
  "flgrlump_i" , "Occupational position",
  "flgrlump_i_1", "Occupational position - Prof/exec",
  "flgrlump_i_2", "Occupational position - Clerical/support",
  "fstatusx_i_2" , "Not married/cohabitating",
  "recentex_cur_smoker" , "Smoking status",
  "recentex_cur_smoker_1",  "Long-term ex-smokers",        
  "recentex_cur_smoker_2", "Recent ex-smokers or current smokers",
  "funitwk0_i_3" , "Alcohol intake",
  "funitwk0_i_3_1", "No alcohol intake",
  "funitwk0_i_3_2", "High alcohol intake",
  "ffruitvg_i_3", "Healthy diet score",
  "ffruitvg_i_3_1", "<Daily fruits and veg intake",
  "ffruitvg_i_3_2", "Daily fruits and veg intake",
  "mvpa_hrs_cont", "Self-reported hours of MVPA",
  "mvpa_inactive", "Self-reported MVPA: 0 h/week",
  "mvpa_less_2.5", "Self-reported hours of MVPA <2.5 h/week",
  "fbmi_i_3", "BMI",
  "fbmi_i_3_1", "Overweight",
  "fbmi_i_3_2", "Obese",
  "hypertension", "Hypertension",
  "ldl_4_1_drgs", "Hyperlipidemia",
  "diabetes_prevalent", "Prevalent diabetes",
  "fmm_index_acm", "Number of chronic conditions, per new condition",
  "fadl_tot", "Number of ADL",
  "ifadl_tot", "Number of instrumental ADL",
  "fadl_tot_2", "Having at least 1 ADL",
  "ifadl_tot_2", "Having at least 1 instrumental ADL",
  
  # sPLS dimensions
  "dim.1", "Component 1",
  "dim.2", "Component 2",
  "dim.3", "Component 3",
  "dim.4", "Component 4",
  "dim.5", "Component 5",
  "dim.6", "Component 6",
  "dim.7", "Component 7",
  
  # PLS dimensions
  "Comp_1", "Component 1 (PLS)",
  "Comp_2", "Component 2 (PLS)",
  "Comp_3", "Component 3 (PLS)",
  "Comp_4", "Component 4 (PLS)",
  
  "dim.1.50", "Component 1 (50%)",
  "dim.1.60", "Component 1 (60%)",
  "dim.1.90", "Component 1 (90%)",
  
)  %>% 
  mutate(var.group = if_else(var %in% c('fage_s_5', 'sex', 'ethnicity', 'flgrlump_i','flgrlump_i_conti', 'flgrlump_i_1', 'flgrlump_i_2', 'fstatusx_i_2', 'edu_conti'), "Socio-demographic predictors",
                     if_else(var %in% c('mvpa_hrs_cont', 'recentex_cur_smoker', 'recentex_cur_smoker_1', 'recentex_cur_smoker_2', 'funitwk0_i_3', 'funitwk0_i_3_1', 'funitwk0_i_3_2', 'ffruitvg_i_3', 'ffruitvg_i_3_1', 'ffruitvg_i_3_2'), "Behavioral predictors",
                     if_else(var %in% c('fbmi_i_3', 'fbmi_i_3_1', 'fbmi_i_3_2', 'hypertension', 'ldl_4_1_drgs', 'diabetes_prevalent', 'fmm_index_acm', 'fadl_tot', 'fadl_tot_2', 'ifadl_tot', 'ifadl_tot_2'), "Health-related predictors", "Accelerometer-derived predictors")))) %>% 
  mutate(var.group = factor(var.group, levels = c("Socio-demographic predictors", "Behavioral predictors", "Health-related predictors", "Accelerometer-derived predictors")))


