# Script: 4-1_predictive_gain_ci.R
# Author: M. Chen, Inserm
# Date: 2022

# Assess the predictive gain and 95% confidence interval of PA and SB score for mortality 
# - Main analysis: in WII, in the full sample
# - Internal validation: in WII, stratified on age, sex, BMI multimorbidity (0 vs 1+ chronic disease)
# - External validation: in CoLaus, in the full sample

# -----------
# Packages & functions
library(tidyverse)
library(coxed)

# Script with home-made functions 
source("E:/PC_FIXE/Analysis/04_MLpaper/02_PLS/PLS_mathilde_2/0-0_homemade_functions.R")

# -----------
# Data 
# Load models + data including the estimated dimensions
# > WII (main analyses, internal validation, sensitivity analyses)
load("E:/PC_FIXE/Analysis/04_MLpaper/02_PLS/PLS_mathilde_2/1_sPLS/sPLS_list.rda")
sPLS_list_wii <- sPLS_list

# > COLAUS (external validation)
# Load models + data including the estimated dimensions
load("E:/PC_FIXE/Analysis/04_MLpaper/02_PLS/PLS_mathilde_2/1_sPLS/sPLS_list_with_CoLaus.rda")
sPLS_list_colaus <- sPLS_list

# Bootstrap results
load("E:/PC_FIXE/Analysis/04_MLpaper/02_PLS/PLS_mathilde_2/4_predictive_gain/01_boot_results.rda")
load("E:/PC_FIXE/Analysis/04_MLpaper/02_PLS/PLS_mathilde_2/4_predictive_gain/02_boot_results_internal_validation.rda")
load("E:/PC_FIXE/Analysis/04_MLpaper/02_PLS/PLS_mathilde_2/4_predictive_gain/03_boot_results_external_validation.rda")

# -----------
# MAIN ANALYSIS
# > WII, full sample - Model 2, adjusted for score 1 only
fs_boot_ci <- do.call(rbind, lapply(1:length(res_bs$fs_boot$t0), getCI, x = res_bs$fs_boot)) %>%
  mutate(term = names.bs.f(model_list = model_list.1)) %>% 
  mutate(type = "Whitehall II accelerometer sub-study (N=3991)")

# > WII, full sample - Model 3, adjusted for score 1 & 2
fs_boot2_ci <- do.call(rbind, lapply(1:length(res_bs$fs_boot2$t0), getCI, x = res_bs$fs_boot2)) %>%
  mutate(term = names.bs.f(model_list = model_list.2)) %>% 
  mutate(type = "Whitehall II accelerometer sub-study (N=3991)")

# > save outputs
boot_ci <- list(
  "fs_boot_ci"         = fs_boot_ci,
  "fs_boot2_ci"        = fs_boot2_ci)

save(boot_ci, 
     file = "E:/PC_FIXE/Analysis/04_MLpaper/02_PLS/PLS_mathilde_2/4_predictive_gain/01_boot_ci.rda")

# -----------
# INTERNAL VALIDATION
# Analyses stratifed by age, sex, bmi groups

# > Age 
# >> among those <74 years
fs_age_0_boot_ci <- do.call(rbind, lapply(1:length(res_bs_int$fs_age_0_boot$t0), getCI, x = res_bs_int$fs_age_0_boot)) %>%
  mutate(term = names.bs.f(model_list = model_list.1_restricted, restricted = T)) %>% 
  mutate(type = "Participants aged <74 years (N = 3001)")

# >> among those >= 74 years
fs_age_1_boot_ci <- do.call(rbind, lapply(1:length(res_bs_int$fs_age_1_boot$t0), getCI, x = res_bs_int$fs_age_1_boot)) %>%
  mutate(term = names.bs.f(model_list = model_list.1_restricted, restricted = T)) %>% 
  mutate(type = "Participants aged 74 years and older (N = 990)")

fs_age_boot_ci <- rbind(fs_age_0_boot_ci, fs_age_1_boot_ci)

# > Sex
# >> among men
fs_men_boot_ci <- do.call(rbind, lapply(1:length(res_bs_int$fs_men_boot$t0), getCI, x = res_bs_int$fs_men_boot)) %>%
  mutate(term = names.bs.f(model_list = model_list.1_restricted_sex, restricted = T)) %>% 
  mutate(type = "Men (N = 2961)")

# >> among women
fs_women_boot_ci <- do.call(rbind, lapply(1:length(res_bs_int$fs_women_boot$t0), getCI, x = res_bs_int$fs_women_boot)) %>%
  mutate(term = names.bs.f(model_list = model_list.1_restricted_sex, restricted = T)) %>% 
  mutate(type = "Women (N = 1030)")

fs_sex_boot_ci <- rbind(fs_men_boot_ci, fs_women_boot_ci)

# > BMI
# >> among normal bmi group
fs_bmi_0_boot_ci <- do.call(rbind, lapply(1:length(res_bs_int$fs_bmi_0_boot$t0), getCI, x = res_bs_int$fs_bmi_0_boot)) %>%
  mutate(term = names.bs.f(model_list = model_list.1_restricted_bmi, restricted = T)) %>% 
  mutate(type = "Normal BMI (N = 1548)")

# >> among overweight participants
fs_bmi_1_boot_ci <- do.call(rbind, lapply(1:length(res_bs_int$fs_bmi_1_boot$t0), getCI, x = res_bs_int$fs_bmi_1_boot)) %>%
  mutate(term = names.bs.f(model_list = model_list.1_restricted_bmi, restricted = T)) %>% 
  mutate(type = "Overweight (N = 1720)")

# >> among obese participants
fs_bmi_2_boot_ci <- do.call(rbind, lapply(1:length(res_bs_int$fs_bmi_2_boot$t0), getCI, x = res_bs_int$fs_bmi_2_boot)) %>%
  mutate(term = names.bs.f(model_list = model_list.1_restricted_bmi, restricted = T)) %>% 
  mutate(type = "Obese (N = 723)")

fs_bmi_boot_ci <- rbind(fs_bmi_0_boot_ci, fs_bmi_1_boot_ci, fs_bmi_2_boot_ci)

# > By morbidity status
# >> among those with 0 chronic disease
fmm0_boot_ci <- do.call(rbind, lapply(1:length(res_bs_int$fmm0_boot$t0), getCI, x = res_bs_int$fmm0_boot)) %>%
  mutate(term = names.bs.f(model_list = model_list.fmm0)) %>% 
  mutate(type = "Participants with 0 chronic disease (N = 2362)")

# >> among those with at least 1 chronic disease
fmm1_boot_ci <- do.call(rbind, lapply(1:length(res_bs_int$fmm1_boot$t0), getCI, x = res_bs_int$fmm1_boot)) %>%
  mutate(term = names.bs.f(model_list = model_list.1)) %>%
  mutate(type = "Participants with at least 1 chronic disease (N = 1629)")

fmm_boot_ci <- rbind(fmm0_boot_ci, fmm1_boot_ci)

# > Save outputs
boot_ci_int <- list(
  "fs_age_boot_ci"       = fs_age_boot_ci,
  "fs_sex_boot_ci"       = fs_sex_boot_ci,
  "fs_bmi_boot_ci"       = fs_bmi_boot_ci,
  "fmm_boot_ci"          = fmm_boot_ci
)

save(boot_ci_int, 
     file = "E:/PC_FIXE/Analysis/04_MLpaper/02_PLS/PLS_mathilde_2/4_predictive_gain/02_boot_ci_internal_validation.rda")

# -----------
# EXTERNAL VALIDATION 
# > CoLaus, full sample
fs_colaus_boot_ci <- do.call(rbind, lapply(1:length(res_bs_ext$fs_colaus_boot$t0), getCI, x = res_bs_ext$fs_colaus_boot)) %>%
  mutate(term = names.bs.f(model_list = model_list_colaus_fg.1)) %>% 
  mutate(type = "CoLaus accelerometer sub-study (N=1284)")

fs_colaus2_boot_ci <- do.call(rbind, lapply(1:length(res_bs_ext$fs_colaus_boot2$t0), getCI, x = res_bs_ext$fs_colaus_boot2)) %>%
  mutate(term = names.bs.f(model_list = model_list_colaus_fg.1)) %>% 
  mutate(type = "CoLaus accelerometer sub-study (N=1329)")

# save outputs
boot_ci_ext <- list(
  "fs_colaus_boot_ci"  = fs_colaus_boot_ci,
  "fs_colaus2_boot_ci" = fs_colaus2_boot_ci)

save(boot_ci_ext, 
     file = "E:/PC_FIXE/Analysis/04_MLpaper/02_PLS/PLS_mathilde_2/4_predictive_gain/03_boot_ci_external_validation.rda")
