# Script: 6_tables_graphs_script.R
# Author: M. Chen, Inserm
# Date: 2022

# Table & graphs
# - Table 1. Sample characteristics in 2012-2013 by mortality status at the end of the follow-up (February 2021) in the Whitehall II accelerometer sub-study
# - Table 2. Features of physical activity and sedentary behaviour in 2012-2013 by mortality status at the end of the follow-up (February 2021) and their separate association with mortality in the Whitehall II accelerometer sub-study
# - Table 3. Predictive performance of physical activity and sedentary behaviour composite scores 1 and 2 for mortality risk in the Whitehall II accelerometer sub-study (N cases/N total = 410/3991, mean [standard deviation] follow-up = 8·1 [1·3] years)
# - Table 4. Predictive performance of the physical activity and sedentary behaviour composite score 1 for mortality risk in CoLaus accelerometer sub-study (N cases/N total = 105/1329, mean [standard deviation] follow-up = 3·8 [0·7] years)
# - Figure 1. Factor loadings of physical activity and sedentary behaviour variables in composite scores identified as predictors of mortality risk
# - Figure 2. Association of the physical activity and sedentary behaviour composite scores with mortality in Whitehall II accelerometer sub-study
# - Figure 3. Association of the physical activity and sedentary behaviour composite score 1 with mortality risk and its predictive performance by subgroups defined by age, sex, body mass index, and morbidity status in Whitehall II accelerometer sub-study
# - Figure 4. Association of the physical activity and sedentary behaviour composite score 1 with mortality risk in the CoLaus study

# > For supplementary material see 7_supp_tables_graphs_script.R

# -----------
# Packages & functions
library(tidyverse)
library(arsenal)
library(cowplot)
library(wesanderson)
library(corrplot)
library(ggrepel)

# Script with home-made functions 
source("E:/PC_FIXE/Analysis/04_MLpaper/02_PLS/PLS_mathilde_2/0-0_homemade_functions.R")

# Colors palettes for plots
pal <- wes_palette(name = "Zissou1", 5, "discrete")
pal_bicolor <- pal[c(1,5)] 

# -----------
# Data 
# > Baseline data
dat <- read.csv("E:/PC_FIXE/Analysis/04_MLpaper/02_PLS/PLS_mathilde_2/0_data/data_WII.csv") %>% 
  # remove unused variables
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
  # recode sex and last occupational status
  mutate(sex = recode(sex, "1: male"=0, "2: female"=1)) %>% 
  mutate(flgrlump_i_conti = recode(flgrlump_i, "1: administrative"="0", "2: prof/exec"="1", "3: clerical/support"="2"),
         flgrlump_i_conti = as.numeric(as.character(flgrlump_i_conti))); dim(dat) # 3991 123

# > Names for variables
source("E:/PC_FIXE/Analysis/04_MLpaper/02_PLS/PLS_mathilde_2/0_data/0_variables_names.R")

# > Load models + data including the estimated dimensions
# >> WII (main analyses, internal validation, sensitivity analyses)
load("E:/PC_FIXE/Analysis/04_MLpaper/02_PLS/PLS_mathilde_2/1_sPLS/sPLS_list.rda")
sPLS_list_wii <- sPLS_list

# >> CoLaus (external validation)
# Load models + data including the estimated dimensions
load("E:/PC_FIXE/Analysis/04_MLpaper/02_PLS/PLS_mathilde_2/1_sPLS/sPLS_list_with_CoLaus.rda")
sPLS_list_colaus <- sPLS_list

# > Merge factors loading results 
tab_loadings <- as.data.frame(sPLS_list_wii$fs$loadingsX_sPLS) %>% 
  mutate(var = rownames(.), set_analyses="Full sample\nN=3991") %>% 
  gather(key="component", value="loadings", -var, -set_analyses) %>% 
  mutate(id.set = "fs") %>% 
  mutate(set_analyses = factor(set_analyses, levels=c("Full sample\nN=3991")))

# > association between scores and mortality
load("E:/PC_FIXE/Analysis/04_MLpaper/02_PLS/PLS_mathilde_2/3_associations/01_associations_wii.rda")
load("E:/PC_FIXE/Analysis/04_MLpaper/02_PLS/PLS_mathilde_2/3_associations/02_association_internal_validation.rda")
load("E:/PC_FIXE/Analysis/04_MLpaper/02_PLS/PLS_mathilde_2/3_associations/03_associations_external_validation.rda")

# > linear and restricted cubic splines models
load("E:/PC_FIXE/Analysis/04_MLpaper/02_PLS/PLS_mathilde_2/3_associations/01_linear_splines_wii.rda")
load("E:/PC_FIXE/Analysis/04_MLpaper/02_PLS/PLS_mathilde_2/3_associations/03_linear_splines_external_validation.rda")

# > predictive gain and 95% CI 
load("E:/PC_FIXE/Analysis/04_MLpaper/02_PLS/PLS_mathilde_2/4_predictive_gain/01_boot_ci.rda")
load("E:/PC_FIXE/Analysis/04_MLpaper/02_PLS/PLS_mathilde_2/4_predictive_gain/02_boot_ci_internal_validation.rda")
load("E:/PC_FIXE/Analysis/04_MLpaper/02_PLS/PLS_mathilde_2/4_predictive_gain/03_boot_ci_external_validation.rda")

# -----------------------------------------------------------------------------
# MAIN TABLES & FIGURES 
# -----------------------------------------------------------------------------

# Sets of predictors
X_desc <- dat %>% select(
  stno,
  # Mortality status
  "Mortality_status"="ef21ac",
  # Socio-demographics predictors
  "Age"="fage_s", "Sex"="sex", "Ethnicity"="ethnicity", "Last_occupational_status"="flgrlump_i", "Education"="edu_imp", "Marital_status"="fstatusx_i_2", 
  flgrlump_i_1, flgrlump_i_2, # Dummy variables used in the models to check
  # Behavioral predictors
  "Smoking_status"= "recentex_cur_smoker", "Alcohol_consumption"= "funitwk0_i_3", "Fruits_vegetables_intake"="ffruitvg_i_3", "Hours_self_reported_MVPA"="mvpa_hrs_cont", "Hours_self_reported_MVPA_cat"="mvpa_hrs",
  recentex_cur_smoker_1, recentex_cur_smoker_2, funitwk0_i_3_1, funitwk0_i_3_2, ffruitvg_i_3_1, ffruitvg_i_3_2, 
  # Health-related predictors
  "BMI"="fbmi_i_3", "Waist_circumference"="fwaist_i_3", "Hypertension"="hypertension", "Hyperlipidemia"="ldl_4_1_drgs", "Prevalent_diabetes"="diabetes_prevalent", "Multimorbidity_index"="fmm_index_acm", "Number_ADL"="fadl_tot", "Number_IADL"="ifadl_tot",
  fbmi_i_3_1, fbmi_i_3_2, fmm_index_acm_2,
  # Accelerometer-derived standardized predictors
  "Mean_acceleration_mg"="ACC_day_mg_wei", 
  "Total_SB_duration_min"="dur_day_total_IN_min_wei", "Total_LIPA_duration_min"="dur_day_total_LIG_min_wei", "Total_MVPA_duration_min"="dur_day_total_MVPA_min_wei", 
  "Nblocks_SB"="FRAG_Nfrag_IN_day_wei", "Nblocks_LIPA"="FRAG_Nfrag_LIPA_day_wei", "Nblocks_MVPA"="FRAG_Nfrag_MVPA_day_wei", 
  "Mean_duration_SB_block"="FRAG_mean_dur_IN_day_wei", "Mean_duration_LIPA_block"="FRAG_mean_dur_LIPA_day_wei", "Mean_duration_MVPA_block"="FRAG_mean_dur_MVPA_day_wei", 
  "SB_duration_in_0_10_min_block"="dur_day_IN_unbt_min_wei", "LIPA_duration_in_0_10_min_block"="dur_day_LIG_unbt_min_wei", "MVPA_duration_in_0_10_min_block"="dur_day_MVPA_unbt_min_wei", 
  "SB_duration_in_10_30_min_block"="dur_day_IN_bts_10_30_min_wei", "SB_duration_in_30_min_and_more_block"="dur_day_IN_bts_30_min_wei",
  "LIPA_duration_in_10_min_and_more_block"="dur_day_LIG_bts_10_min_wei", "MVPA_duration_in_10_min_and_more_block"="dur_day_MVPA_bts_10_min_wei", 
  "Timing_5_most_active_hours"="M5TIME_num_wei", 
  "Intensity_gradient"="ig_gradient_wei","Intensity_intercept"="ig_intercept_wei", 
  "Ndays_meeting_MVPA_guidelines"="n_days_meeting_guidelines_wei") %>%
  
  # Recode factor variables as factors
  mutate(Sex = factor(Sex), Ethnicity = factor(Ethnicity), Marital_status = factor(Marital_status), Education = factor(Education),
         flgrlump_i_1 = factor(flgrlump_i_1), flgrlump_i_2 = factor(flgrlump_i_2), 
         Smoking_status = factor(Smoking_status), Alcohol_consumption = factor(Alcohol_consumption), Fruits_vegetables_intake = factor(Fruits_vegetables_intake), 
         recentex_cur_smoker_1=factor(recentex_cur_smoker_1), recentex_cur_smoker_2=factor(recentex_cur_smoker_2), 
         funitwk0_i_3_1=factor(funitwk0_i_3_1), funitwk0_i_3_2=factor(funitwk0_i_3_2), 
         ffruitvg_i_3_1=factor(ffruitvg_i_3_1), ffruitvg_i_3_2=factor(ffruitvg_i_3_2),
         BMI = factor(BMI), Hypertension=factor(Hypertension), Hyperlipidemia=factor(Hyperlipidemia), Prevalent_diabetes=factor(Prevalent_diabetes),
         fbmi_i_3_1=factor(fbmi_i_3_1), fbmi_i_3_2=factor(fbmi_i_3_2)) 

# Labels for tables
my.labels <- c(Age = "Age (years), mean (SD)", Sex = "Women", Ethnicity = "Non-white ethnicity", Marital_status = "Marital status", 
               Smoking_status = "Smoking status", Alcohol_consumption = "Alcohol intake", Fruits_vegetables_intake = "Fruit and vegetable intake", 
               BMI = "Body mass index", Prevalent_diabetes = "Prevalent diabetes", 
               Multimorbidity_index = "Number of chronic diseases, mean (SD)", 
               Number_ADL = "Number of basic ADL, mean (SD)", Number_IADL = "Number of IADL, mean (SD)",
               Intensity_intercept                    = "Intensity intercept",                   
               Total_SB_duration_min                  = "Total duration in SB (min)",                 
               Mean_duration_SB_block                 = "Mean SB bout duration (min)",                
               Nblocks_SB                             = "Number of SB bouts",                            
               SB_duration_in_0_10_min_block          = "Time in <10 min SB bouts (min)",         
               SB_duration_in_10_30_min_block         = "Time in 10-29.9 min SB bouts (min)",        
               SB_duration_in_30_min_and_more_block   = "Time in >=30 min SB bouts (min)",  
               Total_LIPA_duration_min                = "Total duration in LIPA (min)",       
               Mean_duration_LIPA_block               = "Mean LIPA bout duration (min)",             
               Nblocks_LIPA                           = "Number of LIPA bouts",            
               LIPA_duration_in_0_10_min_block        = "Time in <10 min LIPA bouts (min)",       
               LIPA_duration_in_10_min_and_more_block = "Time in >= 10 min LIPA bouts (min)",
               Total_MVPA_duration_min                = "Total duration in MVPA (min)",               
               Mean_duration_MVPA_block               = "Mean MVPA bout duration (min)",              
               Nblocks_MVPA                           = "Number of MVPA bouts",                          
               MVPA_duration_in_0_10_min_block        = "Time in <10 min MVPA bouts (min)",       
               MVPA_duration_in_10_min_and_more_block = "Time in >=10 min MVPA bouts (min)",
               Ndays_meeting_MVPA_guidelines          = "Number of days meeting physical activity guidelines",         
               Mean_acceleration_mg                   = "Acceleration (mg)",                  
               Intensity_gradient                     = "Intensity gradient",                    
               Timing_5_most_active_hours             = "Physical activity chronotype (h)")

# -----------
# Table 1: Sample characteristics in 2012-2013 by mortality status at the end of the follow-up (February 2021) in the Whitehall II accelerometer sub-study
X_desc_tab <- tableby(Mortality_status ~ Age + Sex + Ethnicity + Education + Marital_status + 
                        Smoking_status + Alcohol_consumption + Fruits_vegetables_intake + 
                        BMI + Hypertension + Hyperlipidemia + Prevalent_diabetes + Multimorbidity_index + Number_ADL + Number_IADL, 
                      data=X_desc, 
                      # Option for output table
                      control=tableby.control(total=FALSE,
                                              digits=1, digits.p = 3,
                                              numeric.simplify = TRUE,
                                              cat.simplify = TRUE,
                                              numeric.stats = c("Nmiss", "meansd")))
summary(X_desc_tab, text = TRUE, pfootnote = TRUE, title = "Baseline characteristics by mortality status in Whitehall II accelerometer sub-study",
        labelTranslations = my.labels)

# ----------
# Table 2. Features of physical activity and sedentary behaviour in 2012-2013 by mortality status at the end of the follow-up (February 2021) and their separate association with mortality in the Whitehall II accelerometer sub-study
X_desc_tab_acc <- tableby(Mortality_status ~ Intensity_intercept + Total_SB_duration_min + 
                            Mean_duration_SB_block + Nblocks_SB + SB_duration_in_0_10_min_block + SB_duration_in_10_30_min_block + SB_duration_in_30_min_and_more_block + 
                            Total_LIPA_duration_min + Mean_duration_LIPA_block + Nblocks_LIPA + LIPA_duration_in_0_10_min_block + LIPA_duration_in_10_min_and_more_block + 
                            Total_MVPA_duration_min + Mean_duration_MVPA_block + Nblocks_MVPA + MVPA_duration_in_0_10_min_block + MVPA_duration_in_10_min_and_more_block + 
                            Ndays_meeting_MVPA_guidelines + Mean_acceleration_mg +   Intensity_gradient +  Timing_5_most_active_hours, 
                          data=X_desc, control=tableby.control(total=FALSE,
                                                               digits=1, digits.p = 3,
                                                               numeric.simplify = TRUE,
                                                               cat.simplify = TRUE,
                                                               numeric.stats = c("Nmiss", "meansd")))
summary(X_desc_tab_acc, text = TRUE, pfootnote = TRUE, title = "Accelerometer variables by mortality status in Whitehall II accelerometer sub-study",
        labelTranslations = my.labels)


# -----------
# Table 3. Predictive performance of physical activity and sedentary behaviour composite scores 1 and 2 for mortality risk in the Whitehall II accelerometer sub-study (N cases/N total = 410/3991, mean [standard deviation] follow-up = 8·1 [1·3] years)

full_names     <- boot_ci$fs_boot_ci$term[str_detect(boot_ci$fs_boot_ci$term, "full")]
no_dim_1_names <- c("no_dim_1_r2_b", "no_dim_1_aic_b", "no_dim_1_bic_b", "no_dim_1_c_b", "no_dim_1_auc_b", "no_dim_1_youden_cd_b", "no_dim_1_sensitivity_cd_b", "no_dim_1_specificity_cd_b", "no_dim_1_iAUC_b", "no_dim_1_youden_id_b", "no_dim_1_sensitivity_id_b", "no_dim_1_specificity_id_b")
no_dim_2_names <- c("no_dim_2_r2_b", "no_dim_2_aic_b", "no_dim_2_bic_b", "no_dim_2_c_b", "no_dim_2_auc_b", "no_dim_2_youden_cd_b", "no_dim_2_sensitivity_cd_b", "no_dim_2_specificity_cd_b", "no_dim_2_iAUC_b", "no_dim_2_youden_id_b", "no_dim_2_sensitivity_id_b", "no_dim_2_specificity_id_b")

# > Model 1 - excluding PA and SB composite scores (reference model)
wii_0 <- boot_ci$fs_boot_ci %>% 
  filter(term %in% no_dim_1_names) %>% 
  mutate(term = substr(term, 10, nchar(term))) %>%
  mutate(lab = paste0(formatC(statistic, digits=3, format = "f"), " (", formatC(conf.low, digits=3, format = "f"), ", ", formatC(conf.high, digits=3, format = "f"), ")")) %>%
  select(term, lab) ; wii_0

# > Model 2 - adjusted for score 1 
wii_1 <- boot_ci$fs_boot_ci %>% 
  filter(term %in% full_names) %>%
  mutate(term = substr(term, 6, nchar(term))) %>%
  mutate(lab = paste0(formatC(statistic, digits=3, format = "f"), " (", formatC(conf.low, digits=3, format = "f"), ", ", formatC(conf.high, digits=3, format = "f"), ")")) %>%
  select(term, lab) ; wii_1

# > difference in AIC and C-index between model 1 and model 2
diff_wii_1 <- boot_ci$fs_boot_ci %>% 
  filter(term %in% c("d_no_dim_1_aic_b", "d_no_dim_1_c_b")) %>% 
  mutate(term = paste0("d_", substr(term, 12, nchar(term)))) %>%
  mutate(lab = paste0(formatC(statistic, digits=3, format = "f"), " (", formatC(conf.low, digits=3, format = "f"), ", ", formatC(conf.high, digits=3, format = "f"), ")")) %>%
  select(term, lab) ; diff_wii_1

# > Model 3 - adjusted for score 1 & score 2
wii_2 <- boot_ci$fs_boot2_ci %>% 
  filter(term %in% full_names) %>%
  mutate(term = substr(term, 6, nchar(term))) %>%
  mutate(lab = paste0(formatC(statistic, digits=3, format = "f"), " (", formatC(conf.low, digits=3, format = "f"), ", ", formatC(conf.high, digits=3, format = "f"), ")")) %>%
  select(term, lab) ; wii_2

# > difference in AIC and C-index between model 1 and model 3
diff_wii_3 <- boot_ci$fs_boot2_ci %>% 
  filter(term %in% c("d_no_dim_3_aic_b", "d_no_dim_3_c_b")) %>% 
  mutate(term = paste0("d_", substr(term, 12, nchar(term)))) %>%
  mutate(lab = paste0(formatC(statistic, digits=3, format = "f"), " (", formatC(conf.low, digits=3, format = "f"), ", ", formatC(conf.high, digits=3, format = "f"), ")")) %>%
  select(term, lab) ; diff_wii_3

# > difference in AIC and C-index between model 2 and model 3
diff_wii_2 <- boot_ci$fs_boot2_ci %>% 
  filter(term %in% c("d_no_dim_2_aic_b", "d_no_dim_2_c_b")) %>% 
  mutate(term = paste0("d_", substr(term, 12, nchar(term)))) %>%
  mutate(lab = paste0(formatC(statistic, digits=3, format = "f"), " (", formatC(conf.low, digits=3, format = "f"), ", ", formatC(conf.high, digits=3, format = "f"), ")")) %>%
  select(term, lab) ; diff_wii_2

# > combine in one table
rbind(wii_0 %>% rbind(., data.frame(term = c("d_aic_b", "d_c_b"), lab = c("ref", "ref"))) %>% rbind(., data.frame(term = c("d_aic_b2", "d_c_b2"), lab = c("na", "na"))) %>% mutate(Model = "Model 1", Cohort = "WII"), 
      wii_1 %>% rbind(., diff_wii_1) %>% rbind(., data.frame(term = c("d_aic_b2", "d_c_b2"), lab = c("ref", "ref"))) %>% mutate(Model = "Model 2", Cohort = "WII"),
      wii_2 %>% rbind(., diff_wii_3) %>% rbind(., diff_wii_2 %>% mutate(term=recode(term, "d_aic_b"="d_aic_b2", "d_c_b"="d_c_b2"))) %>% mutate(Model = "Model 3", Cohort = "WII")) %>% 
  mutate(Model = factor(Model, levels = c("Model 1", "Model 2", "Model 3"))) %>% 
  spread(term, lab) %>% 
  select(Model, r2_b, aic_b, d_aic_b, sensitivity_cd_b, specificity_cd_b, c_b, d_c_b)

# -----------
# Table 4. Predictive performance of the physical activity and sedentary behaviour composite score 1 for mortality risk in CoLaus accelerometer sub-study (N cases/N total = 105/1329, mean [standard deviation] follow-up = 3·8 [0·7] years)

no_dim_1_names <- c("no_dim_1_r2_b", "no_dim_1_aic_b", "no_dim_1_bic_b", "no_dim_1_c_b", "no_dim_1_auc_b", "no_dim_1_youden_cd_b", "no_dim_1_sensitivity_cd_b", "no_dim_1_specificity_cd_b", "no_dim_1_iAUC_b", "no_dim_1_youden_id_b", "no_dim_1_sensitivity_id_b", "no_dim_1_specificity_id_b")
full_names     <- boot_ci$fs_boot_ci$term[str_detect(boot_ci$fs_boot_ci$term, "full")]

# > model excluding PA and SB composite score (reference model)
colaus_0 <- boot_ci_ext$fs_colaus2_boot_ci %>% 
  filter(term %in% no_dim_1_names) %>% 
  mutate(term = substr(term, 10, nchar(term))) %>%
  mutate(lab = paste0(formatC(statistic, digits=3, format = "f"), " (", formatC(conf.low, digits=3, format = "f"), ", ", formatC(conf.high, digits=3, format = "f"), ")")) %>%
  select(term, lab) ; colaus_0

# > model fully adjusted
colaus_1 <- boot_ci_ext$fs_colaus2_boot_ci %>% 
  filter(term %in% full_names) %>%
  mutate(term = substr(term, 6, nchar(term))) %>%
  mutate(lab = paste0(formatC(statistic, digits=3, format = "f"), " (", formatC(conf.low, digits=3, format = "f"), ", ", formatC(conf.high, digits=3, format = "f"), ")")) %>%
  select(term, lab) ; colaus_1

# > difference in AIC and C-index between reference model and fully adjusted model
diff_colaus_1 <- boot_ci_ext$fs_colaus2_boot_ci %>% 
  filter(term %in% c("d_no_dim_1_aic_b", "d_no_dim_1_c_b")) %>% 
  mutate(term = paste0("d_", substr(term, 12, nchar(term)))) %>%
  mutate(lab = paste0(formatC(statistic, digits=3, format = "f"), " (", formatC(conf.low, digits=3, format = "f"), ", ", formatC(conf.high, digits=3, format = "f"), ")")) %>%
  select(term, lab) ; diff_colaus_1

# > combine in one table
rbind(colaus_0 %>% rbind(., data.frame(term = c("d_aic_b", "d_c_b"), lab = c("ref", "ref"))) %>% mutate(Model = "Reference model", Cohort = "CoLaus"), 
      colaus_1 %>% rbind(., diff_colaus_1) %>% mutate(Model = "Full model", Cohort = "CoLaus")) %>% 
  mutate(Model = factor(Model, levels = c("Reference model", "Full model"))) %>% 
  spread(term, lab) %>% 
  select(Model, r2_b, aic_b, d_aic_b, sensitivity_cd_b, specificity_cd_b, c_b, d_c_b)

# ---------
# Figure 1. Factor loadings of physical activity and sedentary behaviour variables in composite scores identified as predictors of mortality risk

Figure1 <- plot_factors_loadings_bw(tab_loadings = tab_loadings %>% filter(id.set == "fs"))+ 
         theme(plot.background = element_rect(fill="white")) + 
         coord_cartesian(xlim = c(-0.5, 0.5), clip="off") + 
         labs(tag="Dimensions                                                  Features") + 
         theme(plot.tag.position = c(0, 0.98)) ; Figure1

# ---------
# Figure 2. Association of the physical activity and sedentary behaviour composite scores with mortality in Whitehall II accelerometer sub-study

# > Whitehall II accelerometer sub-study

# > HR for PA and SB composite score in the linear model
h = 0.4
ann_text <- data.frame(score = c(-3, -3),
                       HR.low = h, HR.up = h, HR = h, 
                       Dim = c("PA and SB composite score 1", "PA and SB composite score 2"),
                       lab = c(paste0(as.character(mod.out(associations_wii$fs$M_dim) %>% filter(var =="dim.1") %>% select(HR.lab)), ", p<0.001"),
                               paste0(as.character(mod.out(associations_wii$fs$M_dim) %>% filter(var =="dim.2") %>% select(HR.lab)), ", p=0.84")),
                       subgroup = c("Whitehall II accelerometer sub-study (N = 3991)"),
                       Model = factor("Linear regression", levels = c("Linear regression", "Quadratic regression", "Cubic regression", "Regression splines")))

# > Plot
wii_association <- ggplot(linear_splines_wii$fs %>% 
                            mutate(Model = recode(Model, 
                                                  "Linear regression"="linear term",
                                                  "Regression splines"="restricted cubic splines"),
                                   Model=factor(Model, levels = rev(c("linear term", "restricted cubic splines")))) %>%
                            mutate(HR.up = if_else(HR.up>2.5, 2.5, HR.up),
                                   HR.low = if_else(HR.low<0.35, 0.35, HR.low)), 
                          aes(x = score, y = HR)) +
  geom_hline(yintercept = 1, color = "darkgrey") +
  geom_ribbon(aes(ymin=HR.low, ymax=HR.up, alpha=Model, fill = Model)) +
  geom_line(aes(color = Model, size = Model)) + 
  geom_text(data = ann_text, 
            label = paste("Hazard ratio (95% confidence interval) in the linear model:\n", ann_text$lab), 
            color = "black", size=3, hjust=0) +
  facet_wrap(.~ Dim, 
             nrow=1) + 
  theme_bw() + 
  theme(legend.position = "bottom",
        legend.direction = "vertical",
        strip.text.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) + 
  scale_x_continuous(limits = c(-3, 3), breaks = c(-3.0, -2.0, -1.0, 0.0, 1.0, 2.0, 3.0), labels = c("-3.0", "-2.0", "-1.0", "0.0", "1.0", "2.0", "3.0")) + 
  scale_y_continuous(trans = "log", breaks = c(0.5, 1, 1.5, 2, 2.5), limits = c(0.35, 2.5)) +
  scale_alpha_discrete(range=rep(c(0.3, 0.7), 2),       name = "PA and SB composite score included as:") +
  scale_size_discrete(range=rep(c(0.5, 1), 2),          name = "PA and SB composite score included as:") +
  scale_fill_manual(values=rep(c(pal[1], "black"), 2),  name = "PA and SB composite score included as:") +
  scale_color_manual(values=rep(c(pal[1], "black"), 2), name = "PA and SB composite score included as:") +
  #ggtitle("Whitehall II accelerometer sub-study (N = 3991)") +
  guides(color = guide_legend(reverse=TRUE), fill = guide_legend(reverse=TRUE), alpha = guide_legend(reverse=TRUE), size = guide_legend(reverse=TRUE)) + 
  labs(x = "Physical activity and sedentary behaviour composite score", y = "Hazard ratio", hjust = "left"); wii_association

# --------
# Figure 3. Association of the physical activity and sedentary behaviour composite score 1 with mortality risk and its predictive performance by subgroups defined by age, sex, body mass index, and morbidity status in Whitehall II accelerometer sub-study

# HR for all subgroups
HR_type <- associations_wii_int %>% 
map_dfr(., ~{ 
  
  t <- mod.out(.x$M_dim_1) %>% 
    filter(var =="dim.1") %>% 
    select(HR, HR.confint.lower, HR.confint.upper, HR.lab, p.value.lab)
  data.frame(t, row.names = NULL)
  
  }, .id = "type") %>% 
  mutate(type_group = recode(type, 
                             "age_0" = "Age",
                             "age_1" = "Age",
                             "sex_0" = "Sex",
                             "sex_1" = "Sex",
                             "bmi_0" = "Body mass\nindex",
                             "bmi_1" = "Body mass\nindex",
                             "bmi_2" = "Body mass\nindex",
                             "fmm_0" = "Morbidity",
                             "fmm_1" = "Morbidity"),
         type_group = factor(type_group, levels = c("Age", "Sex", "Body mass\nindex", "Morbidity")),
         type = recode(type, 
                       "age_0" = "<74 years (N=3001)",
                       "age_1" = paste0(intToUtf8(8805),"74 years (N=990)"),
                       "sex_0" = "Men (N=2961)",
                       "sex_1" = "Women (N=1030)",
                       "bmi_0" = "Normal body mass index (N=1548)",
                       "bmi_1" = "Overweight (N=1720)",
                       "bmi_2" = "Obese (N=723)",
                       "fmm_0" = "None (N=2362)",
                       "fmm_1" = "One or more (N=1629)")) %>%
  mutate(type = factor(type, levels=rev(c("<74 years (N=3001)",
                                          paste0(intToUtf8(8805),"74 years (N=990)"),
                                          "Men (N=2961)",
                                          "Women (N=1030)",
                                          "Normal body mass index (N=1548)",
                                          "Overweight (N=1720)",
                                          "Obese (N=723)",
                                          "None (N=2362)",
                                          "One or more (N=1629)")))) 

# Model 1
C_model_no_dim_1 <- boot_ci_int %>% 
  map_dfr(., ~{ 
    
    .x %>% 
      filter(term %in% c("no_dim_1_aic_b", "no_dim_1_c_b")) %>% 
      mutate(term = substr(term, 10, nchar(term))) %>%
      select(type, term, statistic,conf.low,conf.high) %>% 
      mutate(model="no_dim_1")
    
    
  })

# Model 2
C_model_full <- boot_ci_int %>% 
  map_dfr(., ~{ 
    
    .x %>% 
      filter(term %in% c("full_aic_b", "full_c_b")) %>% 
      mutate(term = substr(term, 6, nchar(term))) %>%
      select(type, term, statistic,conf.low,conf.high) %>% 
      mutate(model="full")
    
    
  })

# Model 1 vs Model 2
C_diff <- boot_ci_int %>% 
  map_dfr(., ~{ 
    
    .x %>% 
      filter(term %in% c("d_no_dim_1_aic_b", "d_no_dim_1_c_b")) %>% 
      mutate(term = substr(term, 12, nchar(term))) %>%
      mutate(signif=if_else(conf.low*conf.high<0," ","*"))%>%
      mutate(lab = paste0(formatC(statistic, digits=3, format = "f"), "\n(", formatC(conf.low, digits=3, format = "f"), ", ", formatC(conf.high, digits=3, format = "f"), ")", signif)) %>%
      select(type, term, lab)
    
    
  })

# Plots
# > HR with mortality
plot.HR <- HR_type %>% 
  mutate(HR.p.lab = if_else(p.value.lab == "<.001", paste0(HR.lab, ",\np<0.001"), paste0(HR.lab, ",\np=", p.value.lab))) %>% 
  ggplot(data=.) + 
  geom_hline(yintercept=1, lty=2) +
  geom_point(aes(x=type, y=HR), shape="|", size=2) + 
  geom_linerange(aes(x=type, ymin=HR.confint.lower, ymax=HR.confint.upper)) + 
  geom_text(aes(x=type, y=1.22, label=HR.p.lab), nudge_y = 0.05, size=3.25) + 
  #geom_text(aes(x=type, y=HR.confint.upper, label=HR.lab), nudge_y = 0.05, size=3.5) + 
  facet_grid(type_group~., scales = "free", switch = "y", space = "free") + 
  theme_cowplot() + 
  coord_flip(ylim = c(1, 1.22), clip = 'off') +
  scale_y_continuous(breaks = c(1.00, 1.10, 1.20), labels = c("1.00", "1.10", "1.20")) +
  theme(plot.background = element_rect(fill="white", color="white"),
        strip.placement = "outside",
        strip.text.y.left = element_text(angle = 0),
        axis.title.y = element_blank(),
        plot.margin = unit(c(0,8,1,1), "lines")) +
  ggtitle("a. Hazard ratio for mortality (Model 2)") +
  labs(y="Hazard ratio (95% confidence interval)") ; plot.HR

# > C-index for Model 1 and Model 2
plot.C_index <- rbind(C_model_no_dim_1, C_model_full) %>% 
  left_join(C_diff, by=c("type", "term")) %>% 
  mutate(type_group = recode(type, 
                             "Participants aged <74 years (N = 3001)" = "Age",
                             "Participants aged 74 years and older (N = 990)" = "Age",
                             "Men (N = 2961)" = "Sex",
                             "Women (N = 1030)" = "Sex",
                             "Normal BMI (N = 1548)" = "Body mass\nindex",
                             "Overweight (N = 1720)" = "Body mass\nindex",
                             "Obese (N = 723)" = "Body mass\nindex",
                             "Participants with 0 chronic disease (N = 2362)" = "Morbidity",
                             "Participants with at least 1 chronic disease (N = 1629)" = "Morbidity"),
         type_group = factor(type_group, levels = c("Age", "Sex","Body mass\nindex","Morbidity"))) %>% 
  mutate(type = recode(type, "Participants aged <74 years (N = 3001)"           ="<74 years (N=3001)",
                       "Participants aged 74 years and older (N = 990)"         =paste0(intToUtf8(8805),"74 years (N=990)"),
                       "Men (N = 2961)"                                         ="Men (N=2961)",
                       "Women (N = 1030)"                                       ="Women (N=1030)",
                       "Normal BMI (N = 1548)"                                  ="Normal body mass index (N=1548)",
                       "Overweight (N = 1720)"                                  ="Overweight (N=1720)",
                       "Obese (N = 723)"                                        ="Obese (N=723)",
                       "Participants with 0 chronic disease (N = 2362)"         ="None (N=2362)",
                       "Participants with at least 1 chronic disease (N = 1629)"="One or more (N=1629)")) %>% 
  mutate(type = factor(type, levels=rev(c("<74 years (N=3001)",
                                          paste0(intToUtf8(8805),"74 years (N=990)"),
                                          "Men (N=2961)",
                                          "Women (N=1030)",
                                          "Normal body mass index (N=1548)",
                                          "Overweight (N=1720)",
                                          "Obese (N=723)",
                                          "None (N=2362)",
                                          "One or more (N=1629)")))) %>% 
  mutate(model = recode(model, "no_dim_1"='Model 1', "full"='Model 2'),
         model = factor(model, levels=rev(c('Model 1', 'Model 2')))) %>% 
  filter(term=="c_b") %>% 
  ggplot(.) + 
  geom_linerange(aes(x=type, ymin=conf.low, ymax=conf.high,  color = model), position = position_dodge2(width = 0.95)) + 
  geom_point(aes(x=type, y=statistic, color = model, shape = model), position = position_dodge2(width = 0.95)) +
  scale_color_manual(values = rev(pal_bicolor),
                     guide = guide_legend(reverse = TRUE)) + 
  scale_shape_manual(values = c(16,15),
                     guide = guide_legend(reverse = TRUE)) + 
  facet_grid(type_group~., scales = "free", space = "free") + 
  theme_cowplot()+
  theme(plot.background = element_rect(fill="white", color="white"),
        strip.background.y = element_blank(),
        strip.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "bottom", legend.title = element_blank()) +
  ggtitle("b. C-index for Model 1 and 2") + 
  labs(y="Harrell's C-index (95% confidence interval)")+
  coord_flip(); plot.C_index

# > Difference in c-index
plot.diff_C <- rbind(C_model_no_dim_1, C_model_full) %>% 
  left_join(C_diff, by=c("type", "term")) %>% 
  mutate(type = factor(type, levels=rev(c("Participants aged <74 years (N = 3001)",
                                          "Participants aged 74 years and older (N = 990)",
                                          "Men (N = 2961)",
                                          "Women (N = 1030)",
                                          "Normal BMI (N = 1548)",
                                          "Overweight (N = 1720)",
                                          "Obese (N = 723)",
                                          "Participants with 0 chronic disease (N = 2362)",
                                          "Participants with at least 1 chronic disease (N = 1629)")))) %>% 
  mutate(type_group = recode(type, 
                             "Participants aged <74 years (N = 3001)" = "Age groups",
                             "Participants aged 74 years and older (N = 990)" = "Age groups",
                             "Men (N = 2961)" = "Sex",
                             "Women (N = 1030)" = "Sex",
                             "Normal BMI (N = 1548)" = "BMI",
                             "Overweight (N = 1720)" = "BMI",
                             "Obese (N = 723)" = "BMI",
                             "Participants with 0 chronic disease (N = 2362)" = "Morbidity",
                             "Participants with at least 1 chronic disease (N = 1629)" = "Morbidity"),
         type_group = factor(type_group, levels = c("Age groups", "Sex","BMI","Morbidity"))) %>% 
  filter(term=="c_b") %>% 
  ggplot(.) + 
  geom_text(aes(x=type, y = 0.9, label=lab), size=3.25, check_overlap = TRUE) + 
  facet_grid(type_group~., scales = "free", space = "free") + 
  theme_cowplot() +
  theme(plot.background = element_rect(fill="white", color="white"),
        strip.background.y = element_blank(),
        strip.text.y = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank()) +
  ggtitle("c. Difference in C-index\nbetween Model 1 and 2") +
  coord_flip(); plot.diff_C

# > Merge plots
plot.merged <- plot_grid(plot.HR, 
                         plot.C_index, 
                         plot.diff_C, 
                         nrow = 1, rel_widths = c(0.6, 0.25, 0.2), axis = "bt",  align = "hv"); plot.merged

# -----------
# Figure 4. Association of the physical activity and sedentary behaviour composite score 1 with mortality risk in the CoLaus study

# > HR for PA and SB composite score in the linear model
h = 0.4
ann_text <- data.frame(score = c(-3, -3),
                       HR.low = h, HR.up = h, HR = h, 
                       Dim = c("PA and SB composite score 1"),
                       lab = paste0(as.character(mod.out(associations_colaus$fs_colaus$M_dim_1_c_fg_60_2) %>% filter(var =="dim.1") %>% select(HR.lab)), ", p<0.001"),
                       subgroup = "CoLaus accelerometer sub-study (N = 1284)",
                       Model = factor("Linear regression", levels = c("Linear regression", "Quadratic regression", "Cubic regression", "Regression splines")))

# > Plot
ggplot(linear_splines_wii_colaus$fs_colaus %>% 
         filter(score>-3, score<3) %>% 
         mutate(Model = recode(Model, 
                               "Linear regression"="linear term",
                               "Regression splines"="restricted cubic splines"),
                Model = factor(Model, levels = rev(c("linear term", "restricted cubic splines")))) %>%
         mutate(HR.up = if_else(HR.up>2.5, 2.5, HR.up),
                HR.low = if_else(HR.low<0.35, 0.35, HR.low)), 
       aes(x = score, y = HR)) +
  geom_hline(yintercept = 1, color = "darkgrey") +
  geom_ribbon(aes(ymin=HR.low, ymax=HR.up, alpha=Model, fill = Model)) +
  geom_line(aes(color = Model, size = Model)) + 
  geom_text(data = ann_text, 
            label = paste("Hazard ratio (95% confidence interval) in the linear model:\n", ann_text$lab), 
            color = "black", size=3, hjust=0, check_overlap = TRUE) +
  theme_bw() + 
  theme(legend.position = "bottom",
        legend.direction = "vertical",
        strip.text.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) + 
  scale_x_continuous(limits = c(-3, 3), breaks = c(-3.0, -2.0, -1.0, 0.0, 1.0, 2.0, 3.0), labels = c("-3.0", "-2.0", "-1.0", "0.0", "1.0", "2.0", "3.0")) + 
  scale_y_continuous(trans = "log", breaks = c(0.5, 1, 1.5, 2, 2.5), limits = c(0.35, 2.5)) +
  #scale_y_continuous(trans = "log", breaks = c(0.01, 0.05, 0.25, 0.5, 1, 2, 5, 10, 25)) +
  scale_alpha_discrete(range=rep(c(0.3, 0.7), 2),       name = "PA and SB composite score included as:") +
  scale_size_discrete(range=rep(c(0.5, 1), 2),          name = "PA and SB composite score included as:") +
  scale_fill_manual(values=rep(c(pal[1], "black"), 2),  name = "PA and SB composite score included as:") +
  scale_color_manual(values=rep(c(pal[1], "black"), 2), name = "PA and SB composite score included as:") +
  guides(color = guide_legend(reverse=TRUE), fill = guide_legend(reverse=TRUE), alpha = guide_legend(reverse=TRUE), size = guide_legend(reverse=TRUE)) + 
  labs(x = "Physical activity and sedentary behaviour composite score", y = "Hazard ratio", hjust = "left") 

