# Script: 3_2_associations_CoLaus.R
# Author: M. Chen, Inserm
# Date: 2022

# Examine the assocation between PA and SB composite score and mortality in CoLaus

# -----------
# Packages & functions
library(tidyverse)
library(survival)
library(rms)
library(aod)
library(Epi)

# -----------
# Script with home-made functions 
source("H:/scripts R/0-0_homemade_functions.R")

# -----------
# Data 
# Load models + data including the estimated dimensions
load("H:/data from pedro/sPLS_list_with_CoLaus.rda")

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

# -----------
# Covariates included

# > for analyses excluding participants with missing data on fg intake
covariates.colaus.2 <- "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + 
               ex_smokers_colaus + cur_smokers_colaus + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + 
               fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot_2 + ifadl_tot_2"

# -----------
# Associations among those >= 60 years & imputing fruits and legume by median value

# Reference model adjusted for all covariates excluding PA and SB scores
M0_c_fg_60_2 <- coxph(as.formula(paste0("Surv(pf21ac, ef21ac) ~ ", covariates.colaus.2)), 
                    data = data_fg_imputed); mod.out(M0_c_fg_60_2) %>% select(HR.lab, p.value.lab); t(mod.perf(M0_c_fg_60_2))

# Reference model additionally adjusted for first PA and SB composite score
M_dim_1_c_fg_60_2 <- coxph(as.formula(paste0("Surv(pf21ac, ef21ac) ~ dim.1 + ", covariates.colaus.2)), 
                         data = data_fg_imputed); mod.out(M_dim_1_c_fg_60_2) %>% select(HR.lab, p.value.lab); t(mod.perf(M_dim_1_c_fg_60_2))

# > Likelihood ratio test between models without score vs model with score 1 only
lmtest::lrtest(M_dim_1_c_fg_60_2, M0_c_fg_60_2) 
lmtest::waldtest(M_dim_1_c_fg_60_2, M0_c_fg_60_2)


# ------------------------------------------------------------------
# Test linearity of association between PA and SB composite score and mortality
# ---------------------------------

ref1 <- mean(data_fg_imputed$dim.1); ref1 # -1.338913
quantiles_X1 <- quantile(data_fg_imputed$dim.1, probs = c(.01, .05, .25, .75, .95, .99)) ; quantiles_X1
X1 <- seq(quantiles_X1[2], quantiles_X1[5], length.out = 1000)

# -----------
# Linear regression

# > association 
m1 <- M_dim_1_c_fg_60_2
m1_asso <- mod.out(m1) %>% filter(var %in% c("dim.1")) %>% select(HR.lab, p.value.lab); m1_asso

# > HR & 95% CI
hr1 <- make_hr_lin(m1, ref1, X1, "dim.1")

# -----------
# Restricted cubic splines regression

# > association 
m1s_dim_1 <- coxph(as.formula(paste0("Surv(pf21ac, ef21ac) ~ rcs(dim.1, 4) + ", covariates.colaus.2)), data = data_fg_imputed) 
m1s_asso <- mod.out(m1s_dim_1) %>% 
  filter(var %in% c("rcs(dim.1, 4)dim.1", "rcs(dim.1, 4)dim.1'", "rcs(dim.1, 4)dim.1''")) %>% 
  select(HR.lab, p.value.lab); m1s_asso

wald.test(Sigma = vcov(m1s_dim_1), b = coef(m1s_dim_1), Terms = 2:3, H0 = rep(0, 2)) 

lmtest::lrtest(m1s_dim_1, m1)

# > HR and 95% CI
hr1s <- make_hr_spl(m1s_dim_1, ref1, X1, attributes(rcs(data_fg_imputed$dim.1, 4))$parms, "dim.1")

# -----------
# Combine results from linear and spline regression
res.py_colaus <- bind_rows(hr1 %>%  mutate(Model = "Linear regression",  Dim = "PA and SB composite score 1"), 
                    hr1s %>% mutate(Model = "Regression splines", Dim = "PA and SB composite score 1")) %>% 
  mutate(Model = factor(Model, levels = c("Linear regression", "Regression splines")),
         subgroup = "CoLaus accelerometer sub-study (N = 1284)") %>% 
  rename("HR" = 'exp(Est.)', "HR.low" = '2.5%', "HR.up" = '97.5%')

# -----------
# Save outputs
# > Associations
associations_colaus <- list(fs_colaus = list(M0_c_fg_60_2=M0_c_fg_60_2, M_dim_1_c_fg_60_2=M_dim_1_c_fg_60_2))

save(associations_colaus, file = "E:/PC_FIXE/Analysis/04_MLpaper/02_PLS/PLS_mathilde_2/3_associations/03_associations_external_validation.rda")

# > Linear and restricted cubic splines
linear_splines_wii_colaus <- list(fs_colaus = res.py_colaus)

save(linear_splines_wii_colaus, file = "E:/PC_FIXE/Analysis/04_MLpaper/02_PLS/PLS_mathilde_2/3_associations/03_linear_splines_external_validation.rda")


