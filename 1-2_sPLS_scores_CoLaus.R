# Script: 1-2_sPLS_scores_CoLaus.R
# Author: M. Chen, Inserm
# Date: 2022

# Compute sPLS score for CoLaus participants
# Notes: Based on sparse PLS (sPLS) procedure fit (1-1_sPLS_scores_WII.R)

# -----------
# Packages & functions
library(tidyverse)
library(plsRcox)

# Script with home-made functions 
source("E:/PC_FIXE/Analysis/04_MLpaper/02_PLS/PLS_mathilde_2/0-0_homemade_functions.R")

# -----------
# Data 

# > Factors loading estimated by sPLS procedure
load("E:/PC_FIXE/Analysis/04_MLpaper/02_PLS/PLS_mathilde_2/1_sPLS/sPLS_list.rda")
sPLS_factors_loadings <- sPLS_list$fs$loadingsX_sPLS

# > CoLaus acceleromter data
datColaus <- read.csv("E:/PC_FIXE/Analysis/04_MLpaper/02_PLS/PLS_mathilde_2/0_data/data_colaus.csv") 

# > 85 accelerometer-derived standardized predictors
datColaus_acc <- datColaus %>% 
  select(starts_with("z_")) %>% 
  select(-z_fage_s_5) # remove age

# > Keep only selected variables from sPLS model
datColaus_selected <- datColaus %>% select(starts_with(sPLS_list$fs$selected_acc_sPLS))

# -----------
# Compute score 1 in CoLaus
score_1 <- sapply(1:ncol(datColaus_selected),function(x) datColaus_selected[,x] * sPLS_factors_loadings[x] ) %>%
  apply(., 1, sum)

# -----------
# Add score 1 in the CoLaus data 
new_datColaus <- cbind(datColaus, dim.1 = score_1)
sPLS_list[["fs_colaus"]] <- list("new_dat"= new_datColaus)
save(sPLS_list, file= "E:/PC_FIXE/Analysis/04_MLpaper/02_PLS/PLS_mathilde_2/1_sPLS/sPLS_list_with_CoLaus.rda")
