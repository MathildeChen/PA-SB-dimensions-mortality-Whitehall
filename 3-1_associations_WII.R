# Script: 3-1_associations_WII.R
# Author: M. Chen, Inserm
# Date: 2022

# Examine the association between PA and SB composite score and mortality in WII
# Test linearity of association. 

# -----------
# Packages & functions
library(tidyverse)
library(survival)
library(rms)
library(aod)
library(Epi)

# -----------
# Script with home-made functions 
source("E:/PC_FIXE/Analysis/04_MLpaper/02_PLS/PLS_mathilde_2/0-0_homemade_functions.R")

# -----------
# Data 
# Load models + data including the estimated dimensions
load("E:/PC_FIXE/Analysis/04_MLpaper/02_PLS/PLS_mathilde_2/1_sPLS/sPLS_list.rda")

# -----------
# Covariates included

covariates <- "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + 
               recentex_cur_smoker_1 + recentex_cur_smoker_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + 
               fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot + ifadl_tot"

# -----------
# Associations in the full sample

# Reference model adjusted for all covariates excluding PA and SB scores
M0 <- coxph(as.formula(paste0("Surv(pf21ac, ef21ac) ~ ", covariates)), 
            data = sPLS_list$fs$new_dat); mod.out(M0) %>% select(HR.lab, p.value.lab); t(mod.perf(M0))

# Reference model additionally adjusted for all PA and SB composite scores
M_dim <- coxph(as.formula(paste0("Surv(pf21ac, ef21ac) ~ ", paste(names(sPLS_list$fs$components_sPLS), collapse = " + "), " + ", covariates)), 
               data = sPLS_list$fs$new_dat); mod.out(M_dim) %>% select(HR.lab, p.value.lab); t(mod.perf(M_dim))

# Reference model additionally adjusted for first PA and SB composite score
M_dim_1 <- coxph(as.formula(paste0("Surv(pf21ac, ef21ac) ~ dim.1 + ", covariates)), 
                 data = sPLS_list$fs$new_dat); mod.out(M_dim_1) %>% select(HR.lab, p.value.lab); t(mod.perf(M_dim_1))

# > Likelihood ratio test between models with all scores vs model with score 1 only
lmtest::lrtest(M_dim, M_dim_1)   # p = 0.8373
lmtest::waldtest(M_dim, M_dim_1) # p = 0.8375

# ------------------------------------------------------------------
# Test linearity of association between PA and SB composite score and mortality

# Reference values
ref1 <- 0 # dim.1 build on standardized metric, ref is 0
ref2 <- 0 # dim.2 build on standardized metric, ref is 0

quantiles_X1 <- quantile(sPLS_list$fs$new_dat$dim.1, probs = c(.01, .05, .25, .75, .95, .99)) ; quantiles_X1
quantiles_X2 <- quantile(sPLS_list$fs$new_dat$dim.2, probs = c(.01, .05, .25, .75, .95, .99)) ; quantiles_X2

# > prepare x for prediction
X1 <- seq(quantiles_X1[2], quantiles_X1[5], length.out = 1000)
X2 <- seq(quantiles_X1[2], quantiles_X1[5], length.out = 1000)

# -----------
# Linear regression

# > association 
m1 <- M_dim
m1_asso <- mod.out(m1) %>% filter(var %in% c("dim.1", "dim.2")) %>% select(HR.lab, p.value.lab); m1_asso

# > HR & 95% CI
hr1 <- make_hr_lin(m1, ref1, X1, "dim.1")
hr2 <- make_hr_lin(m1, ref2, X2, "dim.2")

# -----------
# Restricted cubic splines regression

# > association
m1s_dim_1 <- coxph(as.formula(paste0("Surv(pf21ac, ef21ac) ~ rcs(dim.1, 4) + ", covariates)), data = sPLS_list$fs$new_dat) 
m1s <- coxph(as.formula(paste0("Surv(pf21ac, ef21ac) ~ rcs(dim.1, 4) + rcs(dim.2, 4) + ", covariates)), data = sPLS_list$fs$new_dat) 
m1s_asso <- mod.out(m1s) %>% 
  filter(var %in% c("rcs(dim.1, 4)dim.1", "rcs(dim.1, 4)dim.1'", "rcs(dim.1, 4)dim.1''",
                    "rcs(dim.2, 4)dim.2", "rcs(dim.2, 4)dim.2'", "rcs(dim.2, 4)dim.2''")) %>% 
  select(HR.lab, p.value.lab); m1s_asso

wald.test(Sigma = vcov(m1s), b = coef(m1s), Terms = 2:3, H0 = rep(0, 2)) 
wald.test(Sigma = vcov(m1s), b = coef(m1s), Terms = 5:6, H0 = rep(0, 2)) 

lmtest::lrtest(m1s, m1)              
lmtest::lrtest(m1s_dim_1, M_dim_1)   

# > HR and 95% CI
hr1s <- make_hr_spl(m1s, ref1, X1, attributes(rcs(sPLS_list$fs$new_dat$dim.1, 4))$parms, "dim.1")
hr2s <- make_hr_spl(m1s, ref2, X2, attributes(rcs(sPLS_list$fs$new_dat$dim.2, 4))$parms, "dim.2")

# -----------
# Combine results from linear and spline regression
res.py <- bind_rows(hr1 %>%  mutate(Model = "Linear regression",  Dim = "PA and SB composite score 1"), 
                    hr2 %>%  mutate(Model = "Linear regression",  Dim = "PA and SB composite score 2"), 
                    hr1s %>% mutate(Model = "Regression splines", Dim = "PA and SB composite score 1"),
                    hr2s %>% mutate(Model = "Regression splines", Dim = "PA and SB composite score 2")) %>% 
  mutate(Model = factor(Model, levels = c("Linear regression", "Regression splines")),
         subgroup = "Whitehall II accelerometer sub-study (N = 3991)") %>% 
  rename("HR" = 'exp(Est.)', "HR.low" = '2.5%', "HR.up" = '97.5%')

# -----------
# Save outputs 

# > Associations
associations_wii <- list(fs = list(M0=M0, M_dim=M_dim, M_dim_1=M_dim_1))

save(associations_wii, file = "E:/PC_FIXE/Analysis/04_MLpaper/02_PLS/PLS_mathilde_2/3_associations/01_associations_wii.rda")

# > Linear and restricted cubic splines
linear_splines_wii <- list(fs = res.py)

save(linear_splines_wii, file = "E:/PC_FIXE/Analysis/04_MLpaper/02_PLS/PLS_mathilde_2/3_associations/01_linear_splines_wii.rda")
