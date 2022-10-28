# Script: 5_sensitivity_analysis_including_waking_period.R
# Author: M. CHEN, Inserm
# Date: 2022

# Compared to main analysis, this script includes variables standardised on 16h (~mean waking period in the study population)
# to take into account waking period in the fit
# This script includes: 
# - 5-fold cross validation procedure to determine hyperparameters of the 
# sparse PLS (sPLS) procedure on accelerometer data of Whitehall II
# - fit the sPLS model to select relevant features and to derive compposite score(s) 
# - estimate predictive gain of adding these scores to traditional risk factors
#   to predict mortality

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
library(arsenal)
library(cowplot)
library(rms)
library(aod)
library(Epi)

# -----------
# Script with home-made functions 
source("E:/PC_FIXE/Analysis/04_MLpaper/02_PLS/PLS_mathilde_2/0-0_homemade_functions.R")
# names for accelerometer variables
source("E:/PC_FIXE/Analysis/04_MLpaper/02_PLS/PLS_mathilde_2/0_data/0_variables_names.R")

# Colors palettes for plots
pal <- wesanderson::wes_palette(name = "Zissou1", 5, "discrete")
pal_bicolor <- pal[c(1,5)] 

# -----------
# Data 
dat0 <- read.csv("E:/PC_FIXE/Analysis/04_MLpaper/02_PLS/PLS_mathilde_2/0_data/data_WII.csv")

dat <- dat0 %>% 
  group_by(stno) %>% 
  # standardize the variables on waking period
  mutate(dur_day_total_IN_min_wei     = (dur_day_total_IN_min_wei    *(16*60)) / dur_day_min_wei,
         dur_day_total_LIG_min_wei    = (dur_day_total_LIG_min_wei   *(16*60)) / dur_day_min_wei,
         dur_day_total_MVPA_min_wei   = (dur_day_total_MVPA_min_wei  *(16*60)) / dur_day_min_wei,
         FRAG_Nfrag_IN_day_wei        = (FRAG_Nfrag_IN_day_wei       *(16*60)) / dur_day_min_wei,
         FRAG_Nfrag_LIPA_day_wei      = (FRAG_Nfrag_LIPA_day_wei     *(16*60)) / dur_day_min_wei,
         FRAG_Nfrag_MVPA_day_wei      = (FRAG_Nfrag_MVPA_day_wei     *(16*60)) / dur_day_min_wei,
         dur_day_IN_unbt_min_wei      = (dur_day_IN_unbt_min_wei     *(16*60)) / dur_day_min_wei,
         dur_day_LIG_unbt_min_wei     = (dur_day_LIG_unbt_min_wei    *(16*60)) / dur_day_min_wei,
         dur_day_MVPA_unbt_min_wei    = (dur_day_MVPA_unbt_min_wei   *(16*60)) / dur_day_min_wei,
         dur_day_IN_bts_10_30_min_wei = (dur_day_IN_bts_10_30_min_wei*(16*60)) / dur_day_min_wei,
         dur_day_IN_bts_30_min_wei    = (dur_day_IN_bts_30_min_wei   *(16*60)) / dur_day_min_wei,
         dur_day_LIG_bts_10_min_wei   = (dur_day_LIG_bts_10_min_wei  *(16*60)) / dur_day_min_wei,
         dur_day_MVPA_bts_10_min_wei  = (dur_day_MVPA_bts_10_min_wei *(16*60)) / dur_day_min_wei) %>% 
  # z-scores of standardized variables
  ungroup() %>%
  mutate(across(.cols = c(dur_day_total_IN_min_wei, dur_day_total_LIG_min_wei, dur_day_total_MVPA_min_wei, FRAG_Nfrag_IN_day_wei, FRAG_Nfrag_LIPA_day_wei, FRAG_Nfrag_MVPA_day_wei, dur_day_IN_unbt_min_wei, dur_day_LIG_unbt_min_wei, dur_day_MVPA_unbt_min_wei, dur_day_IN_bts_10_30_min_wei, dur_day_IN_bts_30_min_wei, dur_day_LIG_bts_10_min_wei, dur_day_MVPA_bts_10_min_wei), 
                .fns = list(z = ~scale(.)),
                .names = "{fn}_{col}")) %>%
  select(-starts_with("Nblocks"),
         -starts_with("z_Nblocks_"),
         -ends_with("_WE"), 
         -ends_with("_WD")) %>% 
  mutate(sex = recode(sex, "1: male"=0, "2: female"=1)) %>% 
  mutate(flgrlump_i_conti = recode(flgrlump_i, "1: administrative"="0", "2: prof/exec"="1", "3: clerical/support"="2"),
         flgrlump_i_conti = as.numeric(as.character(flgrlump_i_conti)))

# -----------
# Mortality data (outcome to predict)
Cas <- dat$ef21ac # mortality status
T   <- dat$pf21ac # delay

# -----------
# Predictors (derived from accelerometer)
X <- dat %>% ungroup() %>%
  select(
  fage_s, sex, fbmi_i_3,
  fmm_index_acm_2,               # multimorbidity index (0 vs 1+)
  starts_with("z_")              # 21 accelerometer-derived standardized predictors
) %>% select(-z_dur_day_min_wei, # remove waking time
             -z_dur_spt_min_wei, # remove sleep period
             -z_fage_s_5)   %>%  # remove age
  select(-ends_with("dur_day_total_MOD_min_wei"), -ends_with("FRAG_Nfrag_MOD_day_wei"), -ends_with("FRAG_mean_dur_MOD_day_wei"),
         -ends_with("dur_day_total_VIG_min_wei"), -ends_with("FRAG_Nfrag_VIG_day_wei"), -ends_with("FRAG_mean_dur_VIG_day_wei"))

# only accelerometer-derived predictors
X_acc <- X %>% select(starts_with("z_"))

dim(X) # 3991 25
dim(X_acc) # 3991 21

# -----------
# sPLS procedure tuning

# > Partition the train data into 5 folds, to have similar data partition over the cross-validation 
set.seed(123); folds.fs <- createFolds(y = Cas, k = 5)

# > Prepare list of analyses to run
list_cv <- list(fs = list(folds = folds.fs, x = X_acc, time = T, status = Cas, name.save = "fs") )

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
       file = paste0("E:/PC_FIXE/Analysis/04_MLpaper/02_PLS/PLS_mathilde_2/8_sensitivity_analyses/3_waking_period/1_sPLS/cv1_", .x$name.save, ".rda")
       
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
       file = paste0("E:/PC_FIXE/Analysis/04_MLpaper/02_PLS/PLS_mathilde_2/8_sensitivity_analyses/3_waking_period/1_sPLS/cv2_", .x$name.save, ".rda"))
  
  cv2
  
})

# >>> stop cluster//
stopCluster(my.cluster)

beepr::beep(1)

# -----------
cv1_res <- cv1
cv2_res <- cv2
# -----------
# Combine results 
cv1_tab <- map_dfr(cv1_res, ~ { cbind(.x$perf_e, nbz = .x$cv_object$nzb) })
cv2_tab <- map_dfr(cv2_res, ~ { cbind(.x$perf_e, nbz = .x$cv_object$nzb) })

# Select the best combination of ncomp and eta in the first step of the 5-fold cross validation
plot_grid(
  # Plot perf = f(eta + n_nt)
  cv1_tab %>%
    filter(n_nt!=0) %>%
    ggplot(., aes(x=eta, y=cv.error10, color=as.factor(n_nt))) +
    geom_line() + 
    geom_point() + 
    geom_vline(xintercept = c(0.75, 0.9), color = "red", lty=2) + 
    theme_cowplot() + 
    labs(y = "Mean iAUCSurvROC criterion\nover the 5 folds") + 
    theme(legend.position = "none", 
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank()) + 
    lims(x = c(0.5, 0.9)) + 
    scale_color_manual(values = pal[c(1,3,5,2,4)]),
  # Nb selected components = f(eta + n_nt)
  cv1_tab %>%
    filter(n_nt!=0) %>%
    ggplot(., aes(x=eta, y=nbz, color=as.factor(n_nt))) +
    geom_line() + 
    geom_point() + 
    geom_vline(xintercept = c(0.75, 0.9), color = "red", lty=2) + 
    theme_cowplot() + 
    labs(x="Eta (level of sparsity)", y = "Number of selected variables") + 
    theme(legend.position = "bottom") + 
    guides(colour=guide_legend(title = "Number of composite scores")) + 
    lims(x = c(0.5, 0.9)) + 
    scale_color_manual(values = pal[c(1,3,5,2,4)]),
  
  ncol = 1, align = "hv", rel_heights = c(0.45, 0.55)
)

cv1_tab %>%
  filter(n_nt!=0) %>%
  filter(eta <0.9, eta >0.69) %>% 
  arrange(desc(cv.error10), n_nt, desc(eta)) %>% 
  slice_head(n = 5)

# > check first if there is only 1 result that obtain the best performance 
plot_grid(
  # Plot perf = f(eta + n_nt)
  cv2_tab %>%
    filter(n_nt!=0) %>%
    ggplot(., aes(x=eta, y=cv.error10, color=as.factor(n_nt))) +
    geom_line() + 
    geom_point() + 
    geom_point(x = 0.85, y = cv2_tab[which(cv2_tab$eta==0.85 & cv2_tab$n_nt==2),"cv.error10"], color = "red", pch=15, size=3) + 
    geom_vline(xintercept = c(0.75, 0.9), color = "red", lty=2) + 
    theme_cowplot() + 
    labs(y = "Mean iAUCSurvROC criterion\nover the 5 folds") + 
    theme(legend.position = "none", 
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank())+ 
    scale_color_manual(values = pal[c(1,3,5,2,4)]),
  # Nb selected components = f(eta + n_nt)
  cv2_tab %>%
    filter(n_nt!=0) %>%
    ggplot(., aes(x=eta, y=nbz, color=as.factor(n_nt))) +
    geom_line() + 
    geom_point() + 
    geom_vline(xintercept = c(0.75, 0.9), color = "red", lty=2) + 
    theme_cowplot() + 
    labs(x="Eta (level of sparsity)", y = "Number of selected variables") + 
    theme(legend.position = "bottom") + 
    guides(colour=guide_legend(title = "Number of composite scores")) + 
    scale_color_manual(values = pal[c(1,3,5,2,4)]),
  
  ncol = 1, align = "hv", rel_heights = c(0.45, 0.55)
)

cv2_tab %>% 
  filter(eta>0.7) %>%
  arrange(desc(cv.error10), n_nt, desc(eta)) %>% 
  slice_head(n = 5)

#    eta n_nt cv.error10     cv.se10 nbz
# 1 0.85    3  0.6849416 0.007754346  10 <- very similar
# 2 0.85    2  0.6849225 0.007261835   8 <- very similar
# 3 0.84    2  0.6847496 0.007103373   9
# 4 0.84    3  0.6841893 0.007513158  12
# 5 0.82    2  0.6824750 0.006797622  10

# -----------
# Fit sPLS models, extract estimated dimensions, and factors loadings
sPLS_fit_085  = coxsplsDR(X_acc, T, Cas,
                      ncomp = 2,
                      eta = 0.85, 
                      allres = TRUE, 
                      scaleX = FALSE,
                      scaleY = FALSE)

# > extract components estimated by the sPLS procedure
components_sPLS_085 <- sPLS_fit_085$tt_splsDR

# > extract factors loadings
loadings_sPLS_085 <- sPLS_fit_085$splsDR_modplsr$loadings$X

# > add components to dataframe for following analyses
new_dat_085 <- cbind(dat, components_sPLS_085)

# Supplementary Figure 3
as.data.frame(loadings_sPLS_085) %>% 
  mutate(var = rownames(.)) %>% 
  gather(key="component", value="loadings", -var) %>% 
  plot_factors_loadings_bw(tab_loadings = .) + 
  theme(plot.background = element_rect(fill="white")) + 
  coord_cartesian(xlim = c(-0.65, 0.65), clip="off") + 
  scale_x_continuous(breaks = c(-0.50, -0.25, 0, 0.25, 0.50), labels = c("-0.50", "-0.25", "0.0", "0.25", "0.50")) +
  labs(tag="Dimensions                                                  Features") + 
  theme(plot.tag.position = c(0, 0.98))

ggsave(filename = "E:/PC_FIXE/Analysis/04_MLpaper/02_PLS/PLS_mathilde_2/8_sensitivity_analyses/3_waking_period/Figure1.png",
       dpi=300, width=10, height=4)

# > prepare results to be exported
sPLS_list_waking <- list(
  sPLS_model = sPLS_fit_085,                       # sPLS model fitted 
  loadingsX_sPLS = loadings_sPLS_085,              # factors loadings 
  selected_acc_sPLS = rownames(loadings_sPLS_085), # variables selected
  components_sPLS = components_sPLS_085,           # components 
  new_dat_sensi_085 = new_dat_085                  # data with estimated composite scores
)
    
# -----------
# Association with mortality

# > Covariates included

covariates <- "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + 
               recentex_cur_smoker_1 + recentex_cur_smoker_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + 
               fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot + ifadl_tot"

# > Reference model adjusted for all covariates excluding PA and SB scores
M0_085 <- coxph(as.formula(paste0("Surv(pf21ac, ef21ac) ~ ", covariates)), 
            data = sPLS_list_waking$new_dat_sensi_085); mod.out(M0_085) %>% select(HR.lab, p.value.lab); t(mod.perf(M0_085))

# > Reference model additionally adjusted for all PA and SB composite scores
M_dim_085 <- coxph(as.formula(paste0("Surv(pf21ac, ef21ac) ~ ", paste(names(sPLS_list_waking$components_sPLS), collapse = " + "), " + ", covariates)), 
               data = sPLS_list_waking$new_dat_sensi_085); mod.out(M_dim_085) %>% select(HR.lab, p.value.lab); t(mod.perf(M_dim_085))

# > Reference model additionally adjusted for first PA and SB composite score
M_dim_1_085 <- coxph(as.formula(paste0("Surv(pf21ac, ef21ac) ~ dim.1 + ", covariates)), 
                 data = sPLS_list_waking$new_dat_sensi_085); mod.out(M_dim_1_085) %>% select(HR.lab, p.value.lab); t(mod.perf(M_dim_1_085))

# > Likelihood ratio test between models with all scores vs model with score 1 only
lmtest::lrtest(M_dim_085, M_dim_1_085)   
lmtest::waldtest(M_dim_085, M_dim_1_085) 

lmtest::lrtest(M_dim_1_085, M0_085)   
lmtest::waldtest(M_dim_1_085, M0_085) 

# ------------------------------------
# Bootstrap analyses - 1000 bootstrapped samples

# > set the number of bootstrapped samples
B <- 1000

# >>>> setting for paralellization
n.cores <- parallel::detectCores() - 1 ; n.cores # 7 on the computer, 11 on the laptop

my.cluster <- parallel::makeCluster(
  n.cores, 
  type = "PSOCK"
)

#register it to be used by %dopar%
doParallel::registerDoParallel(cl = my.cluster)

# > Models that will be tested:
model_list_restricted_score_1 <- data.frame(
  # Equivalent model 2
  full      = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + recentex_cur_smoker_1 + recentex_cur_smoker_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot + ifadl_tot + dim.1",
  # Equivalent model 1
  no_dim_1  = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + recentex_cur_smoker_1 + recentex_cur_smoker_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot + ifadl_tot"
  
) %>% gather(key="model", value="formula")

model_list_restricted_score_2 <- data.frame(
  # Equivalent model 3
  full      = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + recentex_cur_smoker_1 + recentex_cur_smoker_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot + ifadl_tot + dim.1 + dim.2",
  # Equivalent model 2
  no_dim_2  = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + recentex_cur_smoker_1 + recentex_cur_smoker_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot + ifadl_tot + dim.1",
  # Equivalent model 1
  no_dim  = "z_fage_s_5 + sex + ethnicity + edu_conti + fstatusx_i_2 + recentex_cur_smoker_1 + recentex_cur_smoker_2 + funitwk0_i_3_1 + funitwk0_i_3_2 + ffruitvg_i_3_1 + ffruitvg_i_3_2 + fbmi_i_3_1 + fbmi_i_3_2 + hypertension + ldl_4_1_drgs + diabetes_prevalent + fmm_index_acm + fadl_tot + ifadl_tot"
  
) %>% gather(key="model", value="formula")

# > Model 2 - ajusted for score 1 
set.seed(123)
fs_boot_1 <- boot::boot(data       = sPLS_list_waking$new_dat_sensi_085,
                        model_list = model_list_restricted_score_1,
                        statistic  = bs.f, 
                        max_FU     = 9,
                        R = B, parallel = "multicore", ncpus = n.cores)

save(fs_boot_1, file = "E:/PC_FIXE/Analysis/04_MLpaper/02_PLS/PLS_mathilde_2/8_sensitivity_analyses/3_waking_period/2_bs_results/fs_boot_sensi_085_mod_vig_score_1.rda")


# > Model 3 - ajusted for score 1 and 2
set.seed(123)
fs_boot_2 <- boot::boot(data       = sPLS_list_waking$new_dat_sensi_085,
                        model_list = model_list_restricted_score_2,
                        statistic  = bs.f, 
                        max_FU     = 9,
                        R = B, parallel = "multicore", ncpus = n.cores)

save(fs_boot_2, file = "E:/PC_FIXE/Analysis/04_MLpaper/02_PLS/PLS_mathilde_2/8_sensitivity_analyses/3_waking_period/2_bs_results/fs_boot_sensi_085_mod_vig_score_2.rda")


# >>> stop paralellization
doParallel::stopCluster(my.cluster)

res_bs_sensi_waking <- list("fs_boot" = fs_boot_1, "fs_boot2" = fs_boot_2)
save(res_bs_sensi_waking, file = "E:/PC_FIXE/Analysis/04_MLpaper/02_PLS/PLS_mathilde_2/8_sensitivity_analyses/3_waking_period/2_bs_results/fs_boot_sensi_mod_vig.rda")

# ------------------------------------
# Assess the predictive gain and 95% confidence interval of PA and SB score for mortality 

# Examine results of bootstrap
cbind(as.data.frame(res_bs_sensi_waking$fs_boot$t0), term = names.bs.f(model_list = model_list_restricted_score_1, restricted = TRUE))
cbind(as.data.frame(res_bs_sensi_waking$fs_boot2$t0), term = names.bs.f(model_list = model_list_restricted_score_2, restricted = TRUE))

# Indicate which row numbers are relevant for each model
model_1 <- c(14, 16, 20, 24, 26, 28)           # in fs_boot_2
model_2 <- c(1, 2, 4, 6, 7, 8, 26, 28)         # in fs_boot_1
model_3 <- c(1, 2, 4, 6, 7, 8, 40, 44, 39, 43) # in fs_boot_1
  
# Supplementary Table 2.  
# > Model 1
do.call(rbind, lapply(model_1, getCI, x = res_bs_sensi_waking$fs_boot2)) %>%
  mutate(term = c("R2", "AIC", "C", "Youden", "Sensitivity", "Specificity"))  %>%
  mutate(lab = paste0(formatC(statistic, digits=3, format = "f"), " (", formatC(conf.low, digits=3, format = "f"), ", ", formatC(conf.high, digits=3, format = "f"), ")")) %>%
  select(term, lab)

# > Model 2 + diff Model 2 vs Model 1
do.call(rbind, lapply(model_2, getCI, x = res_bs_sensi_waking$fs_boot)) %>%
  mutate(term = c("R2", "AIC", "C", "Youden", "Sensitivity", "Specificity", "AIC Model 2 vs Model 1", "C-index Model 2 vs Model 1"))  %>%
  mutate(lab = paste0(formatC(statistic, digits=3, format = "f"), " (", formatC(conf.low, digits=3, format = "f"), ", ", formatC(conf.high, digits=3, format = "f"), ")")) %>%
  select(term, lab)

# > Model 3 + diff Model 3 vs Model 1 + diff Model 3 vs Model 2
do.call(rbind, lapply(model_3, getCI, x = res_bs_sensi_waking$fs_boot2)) %>%
  mutate(term = c("R2", "AIC", "C", "Youden", "Sensitivity", "Specificity", "AIC Model 3 vs Model 1", "C-index Model 3 vs Model 1", "AIC Model 3 vs Model 2", "C-index Model 3 vs Model 2"))  %>%
  mutate(lab = paste0(formatC(statistic, digits=3, format = "f"), " (", formatC(conf.low, digits=3, format = "f"), ", ", formatC(conf.high, digits=3, format = "f"), ")")) %>%
  select(term, lab)

