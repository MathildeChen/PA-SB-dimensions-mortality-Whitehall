# Script: 0-0_model_lists.R
# Author: M. Chen, Inserm
# Date: 2022

# List of models to fit during the ranking procedure in WII and in CoLaus
# > to be provided in the bs.f() function

# - Models excluding a group of variables (socio-demographic, behavioral, health-related factors)
# - Models excluding a given variable 
# ATTENTION: the FULL model should be specified first

# --------------------------------------------------------------------------------------------
# Whitehall II (full sample)

# > Models including dim.1
model_list.1 <- data.frame(
  full = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + recentex_cur_smoker_1 + recentex_cur_smoker_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot + ifadl_tot + dim.1",
  
  no_scd = "recentex_cur_smoker_1 + recentex_cur_smoker_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot + ifadl_tot + dim.1",
  no_beh = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot + ifadl_tot + dim.1",
  no_hea = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + recentex_cur_smoker_1 + recentex_cur_smoker_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + dim.1",
  no_dim = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + recentex_cur_smoker_1 + recentex_cur_smoker_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot + ifadl_tot",
  
  no_age       = "sex + ethnicity + edu_conti + fstatusx_i_2 + recentex_cur_smoker_1 + recentex_cur_smoker_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot + ifadl_tot + dim.1",
  no_sex       = "z_fage_s_5 + ethnicity + edu_conti + fstatusx_i_2 + recentex_cur_smoker_1 + recentex_cur_smoker_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot + ifadl_tot + dim.1",
  no_ethnicity = "z_fage_s_5 + sex + edu_conti + fstatusx_i_2 + recentex_cur_smoker_1 + recentex_cur_smoker_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot + ifadl_tot + dim.1",
  no_edu       = "z_fage_s_5 + sex + ethnicity + fstatusx_i_2 + recentex_cur_smoker_1 + recentex_cur_smoker_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot + ifadl_tot + dim.1",
  no_marital   = "z_fage_s_5 + sex + ethnicity + edu_conti + recentex_cur_smoker_1 + recentex_cur_smoker_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot + ifadl_tot + dim.1",
  
  no_smoking    = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot + ifadl_tot + dim.1",
  no_alcohol    = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + recentex_cur_smoker_1 + recentex_cur_smoker_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot + ifadl_tot + dim.1",
  no_fruits_veg = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + recentex_cur_smoker_1 + recentex_cur_smoker_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot + ifadl_tot + dim.1",
  
  no_bmi    = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + recentex_cur_smoker_1 + recentex_cur_smoker_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot + ifadl_tot + dim.1",
  no_hypert = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + recentex_cur_smoker_1 + recentex_cur_smoker_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot + ifadl_tot + dim.1",
  no_hyperl = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + recentex_cur_smoker_1 + recentex_cur_smoker_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + diabetes_prevalent + fmm_index_acm + fadl_tot + ifadl_tot + dim.1",
  no_prev_d = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + recentex_cur_smoker_1 + recentex_cur_smoker_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + fmm_index_acm + fadl_tot + ifadl_tot + dim.1",
  no_fmm    = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + recentex_cur_smoker_1 + recentex_cur_smoker_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fadl_tot + ifadl_tot + dim.1",
  no_adl    = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + recentex_cur_smoker_1 + recentex_cur_smoker_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + ifadl_tot + dim.1",
  no_iadl    = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + recentex_cur_smoker_1 + recentex_cur_smoker_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot + dim.1",
  
  no_dim_1  = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + recentex_cur_smoker_1 + recentex_cur_smoker_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot + ifadl_tot"
  
) %>% gather(key="model", value="formula")


# ----------
# > Models including dim.2
model_list.2 <- data.frame(
  full = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + recentex_cur_smoker_1 + recentex_cur_smoker_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot + ifadl_tot + dim.1 + dim.2",
  
  no_scd = "recentex_cur_smoker_1 + recentex_cur_smoker_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot + ifadl_tot + dim.1 + dim.2",
  no_beh = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot + ifadl_tot + dim.1 + dim.2",
  no_hea = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + recentex_cur_smoker_1 + recentex_cur_smoker_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + dim.1 + dim.2",
  no_dim = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + recentex_cur_smoker_1 + recentex_cur_smoker_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot + ifadl_tot",
  
  no_age       = "sex + ethnicity + edu_conti + fstatusx_i_2 + recentex_cur_smoker_1 + recentex_cur_smoker_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot + ifadl_tot + dim.1 + dim.2",
  no_sex       = "z_fage_s_5 + ethnicity + edu_conti + fstatusx_i_2 + recentex_cur_smoker_1 + recentex_cur_smoker_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot + ifadl_tot + dim.1 + dim.2",
  no_ethnicity = "z_fage_s_5 + sex + edu_conti + fstatusx_i_2 + recentex_cur_smoker_1 + recentex_cur_smoker_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot + ifadl_tot + dim.1 + dim.2",
  no_edu       = "z_fage_s_5 + sex + ethnicity + fstatusx_i_2 + recentex_cur_smoker_1 + recentex_cur_smoker_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot + ifadl_tot + dim.1 + dim.2",
  no_marital   = "z_fage_s_5 + sex + ethnicity + edu_conti + recentex_cur_smoker_1 + recentex_cur_smoker_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot + ifadl_tot + dim.1 + dim.2",
  
  no_smoking    = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot + ifadl_tot + dim.1 + dim.2",
  no_alcohol    = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + recentex_cur_smoker_1 + recentex_cur_smoker_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot + ifadl_tot + dim.1 + dim.2",
  no_fruits_veg = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + recentex_cur_smoker_1 + recentex_cur_smoker_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot + ifadl_tot + dim.1 + dim.2",
  
  no_bmi    = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + recentex_cur_smoker_1 + recentex_cur_smoker_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot + ifadl_tot + dim.1 + dim.2",
  no_hypert = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + recentex_cur_smoker_1 + recentex_cur_smoker_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot + ifadl_tot + dim.1 + dim.2",
  no_hyperl = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + recentex_cur_smoker_1 + recentex_cur_smoker_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + diabetes_prevalent + fmm_index_acm + fadl_tot + ifadl_tot + dim.1 + dim.2",
  no_prev_d = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + recentex_cur_smoker_1 + recentex_cur_smoker_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + fmm_index_acm + fadl_tot + ifadl_tot + dim.1 + dim.2",
  no_fmm    = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + recentex_cur_smoker_1 + recentex_cur_smoker_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fadl_tot + ifadl_tot + dim.1 + dim.2",
  no_adl    = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + recentex_cur_smoker_1 + recentex_cur_smoker_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + ifadl_tot + dim.1 + dim.2",
  no_iadl    = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + recentex_cur_smoker_1 + recentex_cur_smoker_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot + dim.1 + dim.2",
  
  no_dim_1  = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + recentex_cur_smoker_1 + recentex_cur_smoker_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot + ifadl_tot + dim.2",
  no_dim_2  = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + recentex_cur_smoker_1 + recentex_cur_smoker_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot + ifadl_tot + dim.1"
  
) %>% gather(key="model", value="formula")

# ----------
# > Restricted list with only dim.1

model_list.1_restricted <- data.frame(
  full = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + recentex_cur_smoker_1 + recentex_cur_smoker_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot + ifadl_tot + dim.1",
  no_dim_1  = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + recentex_cur_smoker_1 + recentex_cur_smoker_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot + ifadl_tot"
  
) %>% gather(key="model", value="formula")

# ----------
# > Restricted list with only dim.1 without sex

model_list.1_restricted_sex <- data.frame(
  full = "z_fage_s_5 + ethnicity + edu_conti + fstatusx_i_2 + recentex_cur_smoker_1 + recentex_cur_smoker_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot + ifadl_tot + dim.1",
  no_dim_1  = "z_fage_s_5 + ethnicity + edu_conti + fstatusx_i_2 + recentex_cur_smoker_1 + recentex_cur_smoker_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot + ifadl_tot"
  
) %>% gather(key="model", value="formula")

# ----------
# > Restricted list with only dim.1 without BMI groups

model_list.1_restricted_bmi <- data.frame(
  full = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + recentex_cur_smoker_1 + recentex_cur_smoker_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot + ifadl_tot + dim.1",
  no_dim_1  = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + recentex_cur_smoker_1 + recentex_cur_smoker_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot + ifadl_tot"
  
) %>% gather(key="model", value="formula")

# ----------
# > Restricted list with only dim.1 and dim.2

model_list.2 <- data.frame(
  full = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + recentex_cur_smoker_1 + recentex_cur_smoker_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot + ifadl_tot + dim.1 + dim.2",
  no_dim_1 = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + recentex_cur_smoker_1 + recentex_cur_smoker_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot + ifadl_tot + dim.2",
  no_dim_2 = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + recentex_cur_smoker_1 + recentex_cur_smoker_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot + ifadl_tot + dim.1",
  no_dim_3 = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + recentex_cur_smoker_1 + recentex_cur_smoker_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot + ifadl_tot"
  
) %>% gather(key="model", value="formula")


model_list.2_sex <- data.frame(
  full =     "z_fage_s_5 + ethnicity + edu_conti + fstatusx_i_2 + recentex_cur_smoker_1 + recentex_cur_smoker_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot + ifadl_tot + dim.1 + dim.2",
  no_dim_1 = "z_fage_s_5 + ethnicity + edu_conti + fstatusx_i_2 + recentex_cur_smoker_1 + recentex_cur_smoker_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot + ifadl_tot + dim.2",
  no_dim_2 = "z_fage_s_5 + ethnicity + edu_conti + fstatusx_i_2 + recentex_cur_smoker_1 + recentex_cur_smoker_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot + ifadl_tot + dim.1",
  no_dim_3 = "z_fage_s_5 + ethnicity + edu_conti + fstatusx_i_2 + recentex_cur_smoker_1 + recentex_cur_smoker_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot + ifadl_tot"
  
) %>% gather(key="model", value="formula")

model_list.2_bmi <- data.frame(
  full     = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + recentex_cur_smoker_1 + recentex_cur_smoker_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot + ifadl_tot + dim.1 + dim.2",
  no_dim_1 = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + recentex_cur_smoker_1 + recentex_cur_smoker_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot + ifadl_tot + dim.2",
  no_dim_2 = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + recentex_cur_smoker_1 + recentex_cur_smoker_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot + ifadl_tot + dim.1",
  no_dim_3 = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + recentex_cur_smoker_1 + recentex_cur_smoker_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot + ifadl_tot"
  
) %>% gather(key="model", value="formula")

# --------------------------------------------------------------------------------------------
# CoLaus (full sample)

# ----------
# > Models including dim.1 
model_list_colaus.1 <- data.frame(
  full = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + ex_smokers_colaus + cur_smokers_colaus + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + ffruitvg_i_3_9 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot_2 + ifadl_tot_2 + dim.1",
  
  no_scd = "ex_smokers_colaus + cur_smokers_colaus + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + ffruitvg_i_3_9 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot_2 + ifadl_tot_2 + dim.1",
  no_beh = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot_2 + ifadl_tot_2 + dim.1",
  no_hea = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + ex_smokers_colaus + cur_smokers_colaus + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + ffruitvg_i_3_9 + dim.1",
  no_dim = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + ex_smokers_colaus + cur_smokers_colaus + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + ffruitvg_i_3_9 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot_2 + ifadl_tot_2",
  
  no_age       = "sex + ethnicity + edu_conti + fstatusx_i_2 + ex_smokers_colaus + cur_smokers_colaus + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + ffruitvg_i_3_9 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot_2 + ifadl_tot_2 + dim.1",
  no_sex       = "z_fage_s_5 + ethnicity + edu_conti + fstatusx_i_2 + ex_smokers_colaus + cur_smokers_colaus + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + ffruitvg_i_3_9 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot_2 + ifadl_tot_2 + dim.1",
  no_ethnicity = "z_fage_s_5 + sex + edu_conti + fstatusx_i_2 + ex_smokers_colaus + cur_smokers_colaus + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + ffruitvg_i_3_9 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot_2 + ifadl_tot_2 + dim.1",
  no_edu       = "z_fage_s_5 + sex + ethnicity + fstatusx_i_2 + ex_smokers_colaus + cur_smokers_colaus + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + ffruitvg_i_3_9 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot_2 + ifadl_tot_2 + dim.1",
  no_marital   = "z_fage_s_5 + sex + ethnicity + edu_conti + ex_smokers_colaus + cur_smokers_colaus + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + ffruitvg_i_3_9 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot_2 + ifadl_tot_2 + dim.1",
  
  no_smoking    = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + ffruitvg_i_3_9 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot_2 + ifadl_tot_2 + dim.1",
  no_alcohol    = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + ex_smokers_colaus + cur_smokers_colaus + ffruitvg_i_3_1 + ffruitvg_i_3_2 + ffruitvg_i_3_9 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot_2 + ifadl_tot_2 + dim.1",
  no_fruits_veg = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + ex_smokers_colaus + cur_smokers_colaus + funitwk0_i_3_1 + funitwk0_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot_2 + ifadl_tot_2 + dim.1",
  
  no_bmi    = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + ex_smokers_colaus + cur_smokers_colaus + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + ffruitvg_i_3_9 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot_2 + ifadl_tot_2 + dim.1",
  no_hypert = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + ex_smokers_colaus + cur_smokers_colaus + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + ffruitvg_i_3_9 + fbmi_i_3_1 + fbmi_i_3_2 + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot_2 + ifadl_tot_2 + dim.1",
  no_hyperl = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + ex_smokers_colaus + cur_smokers_colaus + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + ffruitvg_i_3_9 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + diabetes_prevalent + fmm_index_acm + fadl_tot_2 + ifadl_tot_2 + dim.1",
  no_prev_d = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + ex_smokers_colaus + cur_smokers_colaus + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + ffruitvg_i_3_9 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + fmm_index_acm + fadl_tot_2 + ifadl_tot_2 + dim.1",
  no_fmm    = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + ex_smokers_colaus + cur_smokers_colaus + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + ffruitvg_i_3_9 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fadl_tot_2 + ifadl_tot_2 + dim.1",
  no_adl    = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + ex_smokers_colaus + cur_smokers_colaus + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + ffruitvg_i_3_9 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + ifadl_tot_2 + dim.1",
  no_iadl    ="z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + ex_smokers_colaus + cur_smokers_colaus + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + ffruitvg_i_3_9 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot_2 + dim.1",
  
  no_dim_1  = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + ex_smokers_colaus + cur_smokers_colaus + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + ffruitvg_i_3_9 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot_2 + ifadl_tot_2"
  
) %>% gather(key="model", value="formula")


# ----------
# > Models including dim.1 (without missing fg data)
model_list_colaus_fg.1 <- data.frame(
  full = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + ex_smokers_colaus + cur_smokers_colaus + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot_2 + ifadl_tot_2 + dim.1",
  
  no_scd = "ex_smokers_colaus + cur_smokers_colaus + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot_2 + ifadl_tot_2 + dim.1",
  no_beh = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot_2 + ifadl_tot_2 + dim.1",
  no_hea = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + ex_smokers_colaus + cur_smokers_colaus + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + dim.1",
  no_dim = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + ex_smokers_colaus + cur_smokers_colaus + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot_2 + ifadl_tot_2",
  
  no_age       = "sex + ethnicity + edu_conti + fstatusx_i_2 + ex_smokers_colaus + cur_smokers_colaus + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot_2 + ifadl_tot_2 + dim.1",
  no_sex       = "z_fage_s_5 + ethnicity + edu_conti + fstatusx_i_2 + ex_smokers_colaus + cur_smokers_colaus + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot_2 + ifadl_tot_2 + dim.1",
  no_ethnicity = "z_fage_s_5 + sex + edu_conti + fstatusx_i_2 + ex_smokers_colaus + cur_smokers_colaus + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot_2 + ifadl_tot_2 + dim.1",
  no_edu       = "z_fage_s_5 + sex + ethnicity + fstatusx_i_2 + ex_smokers_colaus + cur_smokers_colaus + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot_2 + ifadl_tot_2 + dim.1",
  no_marital   = "z_fage_s_5 + sex + ethnicity + edu_conti + ex_smokers_colaus + cur_smokers_colaus + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot_2 + ifadl_tot_2 + dim.1",
  
  no_smoking    = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot_2 + ifadl_tot_2 + dim.1",
  no_alcohol    = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + ex_smokers_colaus + cur_smokers_colaus + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot_2 + ifadl_tot_2 + dim.1",
  no_fruits_veg = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + ex_smokers_colaus + cur_smokers_colaus + funitwk0_i_3_1 + funitwk0_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot_2 + ifadl_tot_2 + dim.1",
  
  no_bmi    = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + ex_smokers_colaus + cur_smokers_colaus + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot_2 + ifadl_tot_2 + dim.1",
  no_hypert = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + ex_smokers_colaus + cur_smokers_colaus + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot_2 + ifadl_tot_2 + dim.1",
  no_hyperl = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + ex_smokers_colaus + cur_smokers_colaus + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + diabetes_prevalent + fmm_index_acm + fadl_tot_2 + ifadl_tot_2 + dim.1",
  no_prev_d = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + ex_smokers_colaus + cur_smokers_colaus + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + fmm_index_acm + fadl_tot_2 + ifadl_tot_2 + dim.1",
  no_fmm    = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + ex_smokers_colaus + cur_smokers_colaus + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fadl_tot_2 + ifadl_tot_2 + dim.1",
  no_adl    = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + ex_smokers_colaus + cur_smokers_colaus + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + ifadl_tot_2 + dim.1",
  no_iadl    ="z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + ex_smokers_colaus + cur_smokers_colaus + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot_2 + dim.1",
  
  no_dim_1  = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + ex_smokers_colaus + cur_smokers_colaus + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot_2 + ifadl_tot_2"
  
) %>% gather(key="model", value="formula")


# ----------
# > Restricted list with only dim.1
model_list_colaus.1_restricted <- data.frame(
  full = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + ex_smokers_colaus + cur_smokers_colaus + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + ffruitvg_i_3_9 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot_2 + ifadl_tot_2 + dim.1",
  no_dim_1  = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + ex_smokers_colaus + cur_smokers_colaus + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + ffruitvg_i_3_9 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot_2 + ifadl_tot_2"
  
) %>% gather(key="model", value="formula")

# ----------
# > Restricted list with only dim.1 (without missing fg data)
model_list_colaus_fg.1_restricted <- data.frame(
  full = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + ex_smokers_colaus + cur_smokers_colaus + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot_2 + ifadl_tot_2 + dim.1",
  no_dim_1  = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + ex_smokers_colaus + cur_smokers_colaus + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot_2 + ifadl_tot_2"
  
) %>% gather(key="model", value="formula")


# --------------------------------------------------------------------------------------------
# Whitehall II (stratified analyses)

# > List for analyses among those without chronic condition (excluding number of chronic disease from the list)
model_list.fmm0 <- data.frame(
  full = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + recentex_cur_smoker_1 + recentex_cur_smoker_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fadl_tot + ifadl_tot + dim.1",
  
  no_scd = "recentex_cur_smoker_1 + recentex_cur_smoker_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fadl_tot + ifadl_tot + dim.1",
  no_beh = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fadl_tot + ifadl_tot + dim.1",
  no_hea = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + recentex_cur_smoker_1 + recentex_cur_smoker_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + dim.1",
  no_dim = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + recentex_cur_smoker_1 + recentex_cur_smoker_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fadl_tot + ifadl_tot",
  
  no_age       = "sex + ethnicity + edu_conti + fstatusx_i_2 + recentex_cur_smoker_1 + recentex_cur_smoker_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fadl_tot + ifadl_tot + dim.1",
  no_sex       = "z_fage_s_5 + ethnicity + edu_conti + fstatusx_i_2 + recentex_cur_smoker_1 + recentex_cur_smoker_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fadl_tot + ifadl_tot + dim.1",
  no_ethnicity = "z_fage_s_5 + sex + edu_conti + fstatusx_i_2 + recentex_cur_smoker_1 + recentex_cur_smoker_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fadl_tot + ifadl_tot + dim.1",
  no_edu       = "z_fage_s_5 + sex + ethnicity + fstatusx_i_2 + recentex_cur_smoker_1 + recentex_cur_smoker_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fadl_tot + ifadl_tot + dim.1",
  no_marital   = "z_fage_s_5 + sex + ethnicity + edu_conti + recentex_cur_smoker_1 + recentex_cur_smoker_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fadl_tot + ifadl_tot + dim.1",
  
  no_smoking    = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fadl_tot + ifadl_tot + dim.1",
  no_alcohol    = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + recentex_cur_smoker_1 + recentex_cur_smoker_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fadl_tot + ifadl_tot + dim.1",
  no_fruits_veg = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + recentex_cur_smoker_1 + recentex_cur_smoker_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fadl_tot + ifadl_tot + dim.1",
  
  no_bmi    = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + recentex_cur_smoker_1 + recentex_cur_smoker_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fadl_tot + ifadl_tot + dim.1",
  no_hypert = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + recentex_cur_smoker_1 + recentex_cur_smoker_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + ldl_4_1_drgs + diabetes_prevalent + fadl_tot + ifadl_tot + dim.1",
  no_hyperl = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + recentex_cur_smoker_1 + recentex_cur_smoker_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + diabetes_prevalent + fadl_tot + ifadl_tot + dim.1",
  no_prev_d = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + recentex_cur_smoker_1 + recentex_cur_smoker_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + fadl_tot + ifadl_tot + dim.1",
  no_adl    = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + recentex_cur_smoker_1 + recentex_cur_smoker_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + ifadl_tot + dim.1",
  no_iadl    = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + recentex_cur_smoker_1 + recentex_cur_smoker_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fadl_tot + dim.1",
  
  no_dim_1  = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + recentex_cur_smoker_1 + recentex_cur_smoker_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fadl_tot + ifadl_tot"
  
) %>% gather(key="model", value="formula")

# ----------
# > Restricted list with only dim.1

model_list.fmm0_restricted <- data.frame(
  full = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + recentex_cur_smoker_1 + recentex_cur_smoker_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fadl_tot + ifadl_tot + dim.1",
  no_dim_1  = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + recentex_cur_smoker_1 + recentex_cur_smoker_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fadl_tot + ifadl_tot"
  
) %>% gather(key="model", value="formula")

