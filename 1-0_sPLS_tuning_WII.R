# Script: 1-0_sPLS_tuning.R
# Author: M. CHEN, Inserm
# Date: 2022

# 5-fold cross validation procedure to determine hyperparameters of the 
# sparse PLS (sPLS) procedure on accelerometer data of Whitehall II

# -----------
# Packages & functions
library(tidyverse)
library(plsRcox)
library(caret)
library(survival)
library(survcomp)
library(survAUC)
library(resample)
library(parallel)
library(doParallel)
library(foreach)

# -----------
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

# checks
dim(dat) # 3991 123

# names for accelerometer variables
source("E:/PC_FIXE/Analysis/04_MLpaper/00_DATA/00_tab-name_PA_features.R")

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

dim(X) # 3991 25
dim(X_acc) # 3991 21

# -----------
# sPLS procedure tuning

# > Partition the train data into 5 folds, to have similar data partition over the cross-validation 
set.seed(123); folds.fs <- createFolds(y = Cas, k = 5)

# > Prepare list of analyses to run
list_cv <- list(fs = list(folds = folds.fs, x = X_acc, time = T, status = Cas, name.save = "fs"))

# > Step 1 - 5-fold CV for 0.05-grid of eta
eta1 <- seq(0, 0.9, by=0.05)
nCompTest1 <- 5

# >> start cluster to parallelize the analyses
N_core_in_computer <- detectCores()-1
Create_socket <- makePSOCKcluster(N_core_in_computer)
Register_your_parameters <- registerDoParallel(Create_socket)

cv1_res <- list_cv %>% map(., ~{
  
  # > cross-validation step 1 on each dataset
  cv1 <- suppressMessages(
    lapply(eta1,
           cv_eta_ncomp,
           ncomp = nCompTest1, 
           folds = .x$folds,
           x = .x$x, time = .x$time, status = .x$status))
  
  # > save the results
  save(cv1, 
       file = paste0("E:/PC_FIXE/Analysis/04_MLpaper/02_PLS/PLS_mathilde_2/1_sPLS/cv_1/cv1_", .x$name.save, ".rda")
       
       })
  
# >>> stop cluster//
stopCluster(Create_socket)
  
# > Step 2 - 5-fold CV for 0.01-grid of eta, from 0.79 to 0.89 by 0.01-step
eta2 <- seq(0.79, 0.89, by = 0.01)
nCompTest2 <- 3

# >> start cluster to parallelize the analyses
n.cores <- parallel::detectCores() - 1; n.cores 

#create the cluster
my.cluster <- parallel::makeCluster(
  n.cores, 
  type = "PSOCK"
)

#register it to be used by %dopar%
doParallel::registerDoParallel(cl = my.cluster)

cv2_res <- list_cv %>% map(., ~{
  
  # > cross-validation step 2 on each dataset
  cv2 <- lapply(eta2,
                cv_eta_ncomp,
                ncomp = nCompTest2, 
                folds = .x$folds,
                x = .x$x, time = .x$time, status = .x$status)
  
  # > save the results
  save(cv2, 
       file = paste0("E:/PC_FIXE/Analysis/04_MLpaper/02_PLS/PLS_mathilde_2/1_sPLS/cv_2/cv2_", .x$name.save, ".rda"))
  
  cv2
  
})

# >>> stop cluster//
stopCluster(my.cluster)

cv2_res <- list(fs = fs)

# -----------
# Combine results for sPLS conducted in the full sample (fs)

cv2_tab <- cv2_res %>% 
  map_dfr(., ~ map_dfr(., ~ { .x$perf_e }), .id = "set_analyses") %>% 
  mutate(set_analyses = factor(set_analyses, levels = c("fs")))

# Select the best combination of ncomp and eta 
best_cv2 <- cv2_tab %>% 
  group_by(set_analyses) %>% 
  arrange(set_analyses, desc(cv.error10), n_nt, desc(eta)) %>% 
  slice_head()

# Save the results of the best combination for each sample
res_cv <- list(
  cv2      = cv2_tab,  # all results 
  best_cv2 = best_cv2  # best results
) 

# Save outputs
save(res_cv, 
     file = "E:/PC_FIXE/Analysis/04_MLpaper/02_PLS/PLS_mathilde_2/1_sPLS/cv_2/cv2_results.rda")




