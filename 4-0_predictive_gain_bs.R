# Script: 4-0_predictive_gain_bs.R
# Author: M. CHEN, Inserm
# Date: 2022

# Bootstrap to compute 95% CI of 
# Predictive gain of PA and SB composite score derived from sPLS procedure and mortality 
# - in Whitehall II (main analysis)

# - in Whitehall II (internal validation): 
# -- stratified on age (<74 y vs >= 74 y)
# -- stratified on sex
# -- stratified on BMI groups
# -- stratified on the number of chronic conditions (0 vs 1+)

# - in CoLaus (external validation)

# -----------
# Packages & functions
library(tidyverse)
library(plsRcox)
library(caret)
library(survival)
library(resample)
library(parallel)
library(doParallel)
library(foreach)
library(boot)
library(broom)

# Script with home-made functions 
source("E:/PC_FIXE/Analysis/04_MLpaper/02_PLS/PLS_mathilde_2/0-0_homemade_functions.R")

# -----------
# Data 
# Load models + data including the estimated dimensions

# > WII (main analysis & internal validation)
load("E:/PC_FIXE/Analysis/04_MLpaper/02_PLS/PLS_mathilde_2/1_sPLS/sPLS_list.rda")
sPLS_list_wii <- sPLS_list

# > COLAUS (external validation)
load("H:/data from pedro/sPLS_list_with_CoLaus.rda")
sPLS_list_colaus <- sPLS_list

# > Select participants >= 60 years
data_60 <- sPLS_list$fs_colaus$new_dat[which(sPLS_list$fs_colaus$new_dat$fage_s>=60),]
# > Impute using median value for fruits and veg intake
fg_imp <- median(data_60[which(data_60$ffruitvg_i_3_9==0), "ffruitvg_i_3"])
data_fg_imputed <- data_60 %>% 
  mutate(ffruitvg_i_3 = ifelse(ffruitvg_i_3 == 9, fg_imp, ffruitvg_i_3),
         ffruitvg_i_3_1        = if_else(ffruitvg_i_3 == 0, 1, 0),
         ffruitvg_i_3_2        = if_else(ffruitvg_i_3 == 1, 1, 0))
# > Standardize age 
data_fg_imputed$z_fage_s_5 <- (data_fg_imputed$fage_s - mean(data_fg_imputed$fage_s))/5

# ------------------------------------
# Bootstrap analyses - 1000 bootstrapped samples

# > setting for parallelization
n.cores <- parallel::detectCores() - 1 ; n.cores # 7 on the computer, 11 on the laptop

# > set the number of bootstrapped samples
B <- 1000

# ------------------------------------
# MAIN ANALYSIS

# > Model 2 - ajusted for score 1 
set.seed(123)
fs_boot <- boot(data       = sPLS_list_wii$fs$new_dat,
                model_list = model_list.1,
                statistic  = bs.f, 
                max_FU     = 9,
                R = B, parallel = "multicore", ncpus = n.cores)

save(fs_boot, file = "E:/PC_FIXE/Analysis/04_MLpaper/02_PLS/PLS_mathilde_2/4_predictive_gain/fs_boot.rda")

# > Model 3 - ajusted for score 1 & score 2
set.seed(123)
fs_boot2 <- boot(data       = sPLS_list_wii$fs$new_dat,
                model_list = model_list.2,
                statistic  = bs.f, 
                max_FU     = 9,
                R = B, parallel = "multicore", ncpus = n.cores)

save(fs_boot2, file = "E:/PC_FIXE/Analysis/04_MLpaper/02_PLS/PLS_mathilde_2/4_predictive_gain/fs_boot2.rda")

# ------------------------------------
# INTERNAL VALIDATION 

# ANALYSES STRATIFIED ON AGE
# >> younger than 74 years (n events/N total=187/3001)
set.seed(123)
fs_age_0_boot <- boot(data   = sPLS_list_wii$fs$new_dat[which(sPLS_list_wii$fs$new_dat$fage_s<74),], 
                      model_list = model_list.1_restricted,
                      statistic  = bs.f, 
                      max_FU     = 9, 
                      R = B, parallel = "multicore", ncpus = n.cores-1)

save(fs_age_0_boot, file = "E:/PC_FIXE/Analysis/04_MLpaper/02_PLS/PLS_mathilde_2/4_predictive_gain/fs_age_0_boot.rda")

# >> older than 74 years (n events/N total=223/990)
set.seed(123)
fs_age_1_boot <- boot(data   = sPLS_list_wii$fs$new_dat[which(sPLS_list_wii$fs$new_dat$fage_s>=74),], 
                      model_list = model_list.1_restricted,
                      statistic  = bs.f, 
                      max_FU     = 9, 
                      R = B, parallel = "multicore", ncpus = n.cores-1)

save(fs_age_1_boot, file = "E:/PC_FIXE/Analysis/04_MLpaper/02_PLS/PLS_mathilde_2/4_predictive_gain/fs_age_1_boot.rda")

# ---------------
# ANALYSES STRATIFIED ON SEX
# >> in men
set.seed(123)
fs_men_boot <- boot(data     = sPLS_list_wii$fs$new_dat[which(sPLS_list_wii$fs$new_dat$sex==0),], 
                    model_list = model_list.1_restricted_sex,
                    statistic  = bs.f, 
                    max_FU     = 9,
                    R = B, parallel = "multicore", ncpus = n.cores-1)

save(fs_men_boot, file = "E:/PC_FIXE/Analysis/04_MLpaper/02_PLS/PLS_mathilde_2/4_predictive_gain/fs_men_boot.rda")

# >> in women
set.seed(123)
fs_women_boot <- boot(data   = sPLS_list_wii$fs$new_dat[which(sPLS_list_wii$fs$new_dat$sex==1),], 
                      model_list = model_list.1_restricted_sex,
                      statistic  = bs.f, 
                      max_FU     = 9, 
                      R = B, parallel = "multicore", ncpus = n.cores-1)

save(fs_women_boot, file = "E:/PC_FIXE/Analysis/04_MLpaper/02_PLS/PLS_mathilde_2/4_predictive_gain/fs_women_boot.rda")

# ---------------
# ANALYSES STRATIFIED ON BMI GROUPS
# >> in normal BMI
set.seed(123)
fs_bmi_0_boot <- boot(data     = sPLS_list_wii$fs$new_dat[which(sPLS_list_wii$fs$new_dat$fbmi_i_3==0),], 
                      model_list = model_list.1_restricted_bmi,
                      statistic  = bs.f, 
                      max_FU     = 9,
                      R = B, parallel = "multicore", ncpus = n.cores-1)

save(fs_bmi_0_boot, file = "E:/PC_FIXE/Analysis/04_MLpaper/02_PLS/PLS_mathilde_2/4_predictive_gain/fs_bmi_0_boot.rda")

# >> in overweight
set.seed(123)
fs_bmi_1_boot <- boot(data   = sPLS_list_wii$fs$new_dat[which(sPLS_list_wii$fs$new_dat$fbmi_i_3==1),], 
                      model_list = model_list.1_restricted_bmi,
                      statistic  = bs.f, 
                      max_FU     = 9, 
                      R = B, parallel = "multicore", ncpus = n.cores-1)

save(fs_bmi_1_boot, file = "E:/PC_FIXE/Analysis/04_MLpaper/02_PLS/PLS_mathilde_2/4_predictive_gain/fs_bmi_1_boot.rda")

# >> in obese
set.seed(123)
fs_bmi_2_boot <- boot(data   = sPLS_list_wii$fs$new_dat[which(sPLS_list_wii$fs$new_dat$fbmi_i_3==2),], 
                      model_list = model_list.1_restricted_bmi,
                      statistic  = bs.f, 
                      max_FU     = 9, 
                      R = B, parallel = "multicore", ncpus = n.cores-1)

save(fs_bmi_2_boot, file = "E:/PC_FIXE/Analysis/04_MLpaper/02_PLS/PLS_mathilde_2/4_predictive_gain/fs_bmi_2_boot.rda")

# ---------------
# ANALYSES STRATIFIED ON THE NUMBER OF CHRONIC DISEASES
# >> in those with 0 chronic disease
set.seed(123)
fmm0_boot <- boot(data       = sPLS_list_wii$fs$new_dat[which(sPLS_list_wii$fs$new_dat$fmm_index_acm_2==0),], 
                  model_list = model_list.fmm0,
                  statistic  = bs.f, 
                  max_FU     = 9,
                  R = B, parallel = "multicore", ncpus = n.cores-1)

save(fmm0_boot, file = "E:/PC_FIXE/Analysis/04_MLpaper/02_PLS/PLS_mathilde_2/4_predictive_gain/fmm0_boot.rda")

# >> in those with 1+ chronic disease
set.seed(123)
fmm1_boot <- boot(data       = sPLS_list_wii$fs$new_dat[which(sPLS_list_wii$fs$new_dat$fmm_index_acm_2==1),], 
                  model_list = model_list.1,
                  statistic  = bs.f, 
                  max_FU     = 9, 
                  R = B, parallel = "multicore", ncpus = n.cores-1)

save(fmm1_boot, file = "E:/PC_FIXE/Analysis/04_MLpaper/02_PLS/PLS_mathilde_2/4_predictive_gain/fmm1_boot.rda")

# ------------------------------------
# EXTERNAL VALIDATION 
set.seed(123)
fs_colaus_boot2 <- boot(data        = data_fg_imputed,
                        model_list  = model_list_colaus_fg.1,
                        statistic   = bs.f, 
                        max_FU      = 4,
                        R = B, parallel = "multicore", ncpus = n.cores)

save(fs_colaus_boot2, file = "E:/PC_FIXE/Analysis/04_MLpaper/02_PLS/PLS_mathilde_2/4_predictive_gain/fs_colaus_boot2.rda")


# ------------------------------------
# Merge & save results

# > Main analysis
res_bs <- list("fs_boot"         = fs_boot,
               "fs_boot2"        = fs_boot2)
save(res_bs, file = "E:/PC_FIXE/Analysis/04_MLpaper/02_PLS/PLS_mathilde_2/4_predictive_gain/01_boot_results.rda")

# ---------------
# > Internal validation
res_bs_int <- list("fs_age_0_boot"       = fs_age_0_boot,
                   "fs_age_1_boot"       = fs_age_1_boot,
                   "fs_men_boot"         = fs_men_boot,
                   "fs_women_boot"       = fs_women_boot,
                   "fs_bmi_0_boot"       = fs_bmi_0_boot,
                   "fs_bmi_1_boot"       = fs_bmi_1_boot,
                   "fs_bmi_2_boot"       = fs_bmi_2_boot,
                   "fmm0_boot"           = fmm0_boot,
                   "fmm1_boot"           = fmm1_boot)

save(res_bs_int, file = "E:/PC_FIXE/Analysis/04_MLpaper/02_PLS/PLS_mathilde_2/4_predictive_gain/02_boot_results_internal_validation.rda")

# ---------------
# > External validation 
res_bs_ext <- list("fs_colaus_boot"  = fs_colaus_boot,
                   "fs_colaus_boot2"  = fs_colaus_boot2)

save(res_bs_ext, file = "E:/PC_FIXE/Analysis/04_MLpaper/02_PLS/PLS_mathilde_2/4_predictive_gain/03_boot_results_external_validation.rda")

