# Script: 1-1_sPLS_scores_WII.R
# Author: M. Chen, Inserm
# Date: 2022

# Fit sPLS procedure on accelerometer data of Whitehall II accelerometer sub-study
# Notes: Based on sparse PLS (sPLS) procedure tuning (1-0_sPLS_tuning_WII.R)

# -----------
# Packages & functions
library(tidyverse)
library(plsRcox)

# Script with home-made functions 
source("E:/PC_FIXE/Analysis/04_MLpaper/02_PLS/PLS_mathilde_2/0-0_homemade_functions.R")

# -----------
# Data 
dat0 <- read.csv("E:/PC_FIXE/Analysis/04_MLpaper/02_PLS/PLS_mathilde_2/0_data/data_WII.csv")

# remove unused variables
dat <- dat0 %>% 
  select(-starts_with("Nblocks"),
         -starts_with("z_Nblocks_"),
         -ends_with("_WE"), 
         -ends_with("_WD"),
         -ends_with("dur_day_total_MOD_min_wei"), 
         -ends_with("FRAG_Nfrag_MOD_day_wei"), 
         -ends_with("FRAG_mean_dur_MOD_day_wei"),
         -ends_with("dur_day_total_VIG_min_wei"), 
         -ends_with("FRAG_Nfrag_VIG_day_wei"), 
         -ends_with("FRAG_mean_dur_VIG_day_wei"),
         -ends_with("dur_spt_min_wei")) %>% 
  mutate(sex = recode(sex, "1: male"=0, "2: female"=1)) %>% 
  mutate(flgrlump_i_conti = recode(flgrlump_i, "1: administrative"="0", "2: prof/exec"="1", "3: clerical/support"="2"),
         flgrlump_i_conti = as.numeric(as.character(flgrlump_i_conti)))

# names for accelerometer variables
source("E:/PC_FIXE/Analysis/04_MLpaper/02_PLS/PLS_mathilde_2/0_data/0_variables_names.R")

# -----------
# Mortality data (outcome to predict)
Cas <- dat$ef21ac # mortality status
T   <- dat$pf21ac # delay

# -----------
# Predictors (derived from accelerometer)
X <- dat %>% select(
  fage_s, sex, fbmi_i_3,
  fmm_index_acm_2,               # multimorbidity index (0 vs 1+)
  starts_with("z_")              # 21 accelerometer-derived standardized predictors
) %>% select(-z_dur_day_min_wei, # remove waking time
             -z_fage_s_5)        # remove age

# only accelerometer-derived predictors
X_acc <- X %>% select(starts_with("z_"))

# -----------
# Results from 5-fold CV
load("E:/PC_FIXE/Analysis/04_MLpaper/02_PLS/PLS_mathilde_2/1_sPLS/cv_2/cv2_results.rda")

best_cv2 <- res_cv$best_cv2

# > Prepare list of analyses to run
list_fit <- list(fs = list(index = 1:3991, parameters = best_cv2[which(best_cv2$set_analyses=="fs"),]))

# Fit sPLS models, extract estimated dimensions, and factors loadings
sPLS_list <- list_fit %>% 
  map(., ~{ 
    
    # > fit
    sPLS_fit  = coxsplsDR(X_acc[.x$index,], T[.x$index], Cas[.x$index],
                          ncomp = .x$parameters$n_nt,
                          eta = .x$parameters$eta, 
                          allres = TRUE, 
                          scaleX = FALSE,
                          scaleY = FALSE)
    
    # > extract components estimated by the sPLS procedure
    components_sPLS <- sPLS_fit$tt_splsDR
    
    # > extract factors loadings
    loadings_sPLS <- sPLS_fit$splsDR_modplsr$loadings$X
    
    # > add components to dataframe for following analyses
    new_dat <- cbind(dat[.x$index, ], components_sPLS)
    
    # > prepare results to be exported
    list(
      parameters = .x$parameters,                  # hyperparameters used
      sPLS_model = sPLS_fit,                       # sPLS model fitted 
      loadingsX_sPLS = loadings_sPLS,              # factors loadings 
      selected_acc_sPLS = rownames(loadings_sPLS), # variables selected
      components_sPLS = components_sPLS,           # components 
      new_dat = new_dat                            # data with estimated composite scores
    )
    
  })

# Save the results for each sample
save(sPLS_list,
     file = "E:/PC_FIXE/Analysis/04_MLpaper/02_PLS/PLS_mathilde_2/1_sPLS/sPLS_list.rda")

