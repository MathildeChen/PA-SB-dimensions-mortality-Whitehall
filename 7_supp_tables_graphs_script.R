# Script: 7_supp_tables_graphs_script.R
# Author: M. Chen, Inserm
# Date: 2022

# Supplementary tables & graphs

# - [NOT IN THIS SCRIPT] Supplementary Table 1. Description of the 21 accelerometer-derived daily physical activity and sedentary behaviour features
# - [NOT IN THIS SCRIPT*] Supplementary Table 2. Predictive performance for mortality risk of physical activity and sedentary behaviour composite scores 1 and 2 derived from features standardised to waking period duration in the Whitehall II accelerometer sub-study (N cases/N total = 410/3991, mean [standard deviation] follow-up = 8.1 [1.3] years)	
# - Supplementary Table 3. Baseline characteristics by subgroups in Whitehall II accelerometer sub-study
# - Supplementary Table 4. Predictive performance of the physical activity and sedentary behaviour composite score 1 for mortality risk among participants aged <74 and participants aged ???74 years in the Whitehall II accelerometer sub-study
# - Supplementary Table 5. Predictive performance of the physical activity and sedentary behaviour composite score 1 for mortality risk by sex in Whitehall II accelerometer sub-study
# - Supplementary Table 6. Predictive performance of physical activity and sedentary behaviour composite scores for mortality risk by body mass index status in Whitehall II accelerometer sub-study
# - Supplementary Table 7. Predictive performance of physical activity and sedentary behaviour composite scores for mortality risk by morbidity status in Whitehall II accelerometer sub-study
# - Supplementary Table 8. Sample characteristics in 2014-2017 by mortality status at the end of follow-up (February 2021) in CoLaus accelerometer sub-study

# - [NOT IN THIS SCRIPT] Supplementary Figure 1. Flow chart for sample selection in Whitehall II accelerometer sub-study	
# - Supplementary Figure 2. Correlation plot of the 21 accelerometer-derived physical activity and sedentary behaviour variables	
# - [NOT IN THIS SCRIPT*] Supplementary Figure 3. Factor loadings of physical activity and sedentary behaviour variables, including features standardised to waking period duration, in composite scores identified as predictors of mortality risk	
# - [NOT IN THIS SCRIPT] Supplementary Figure 4. Flow chart for sample selection in CoLaus II accelerometer sub-study	

# > For main tables and figures, see 6_tables_graphs_script.R
# > * For Supplementary Table 2 and Supplementary Figure 3, see 5_sensitivity_analysis_including_waking_period.R

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

# Path for saving plots and figures
my.path <- "E:/PC_FIXE/Analysis/04_MLpaper/02_PLS/PLS_mathilde_2/6_tables_graphs/Supplementary/"

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

my.labels.colaus <- c(Age = "Age (years), mean (SD)", Sex = "Women", Ethnicity = "Non-white ethnicity", Marital_status = "Marital status", 
                      Smoking_status = "Smoking status", Alcohol_consumption = "Alcohol intake", Fruits_vegetables_intake = "Fruit and vegetable intake", 
                      BMI = "Body mass index", Prevalent_diabetes = "Prevalent diabetes", 
                      Multimorbidity_index = "Presence of at least 1 chronic disease", 
                      ADL_2 = "Presence of ADL", Number_IADL = "Presence of IADL",
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

# Labels for index to grab from tables
full_names     <- c("full_r2_b", "full_aic_b", "full_youden_cd_b", "full_sensitivity_cd_b", "full_specificity_cd_b", "full_c_b")
no_dim_1_names <- c("no_dim_1_r2_b", "no_dim_1_aic_b", "no_dim_1_youden_cd_b", "no_dim_1_sensitivity_cd_b", "no_dim_1_specificity_cd_b", "no_dim_1_c_b")
no_dim_2_names <- c("no_dim_2_r2_b", "no_dim_2_aic_b", "no_dim_2_youden_cd_b", "no_dim_2_sensitivity_cd_b", "no_dim_2_specificity_cd_b", "no_dim_2_c_b")
no_dim_3_names <- c("no_dim_3_r2_b", "no_dim_3_aic_b", "no_dim_3_youden_cd_b", "no_dim_3_sensitivity_cd_b", "no_dim_3_specificity_cd_b", "no_dim_3_c_b")

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
tab_loadings <- rbind(
  # results for the full sample
  as.data.frame(sPLS_list_wii$fs$loadingsX_sPLS) %>% mutate(var = rownames(.), set_analyses="Full sample\nN=3991") %>% gather(key="component", value="loadings", -var, -set_analyses) %>% mutate(id.set = "fs"),
  
  # stratified analyses, by age
  as.data.frame(sPLS_list_wii$age0_strat$loadingsX_sPLS) %>% mutate(var = rownames(.), set_analyses="Participant aged\n<74 years (N=3001)")         %>% gather(key="component", value="loadings", -var, -set_analyses) %>% mutate(id.set = "age0"),
  as.data.frame(sPLS_list_wii$age1_strat$loadingsX_sPLS) %>% mutate(var = rownames(.), set_analyses="Participant aged\n74 years and older (N=990)") %>% gather(key="component", value="loadings", -var, -set_analyses) %>% mutate(id.set = "age1"),
  
  # stratified analyses, by sex
  as.data.frame(sPLS_list_wii$sex0_strat$loadingsX_sPLS) %>% mutate(var = rownames(.), set_analyses="Men\n(N=2961)")   %>% gather(key="component", value="loadings", -var, -set_analyses) %>% mutate(id.set = "sex0"),
  as.data.frame(sPLS_list_wii$sex1_strat$loadingsX_sPLS) %>% mutate(var = rownames(.), set_analyses="Women\n(N=1030)") %>% gather(key="component", value="loadings", -var, -set_analyses) %>% mutate(id.set = "sex1"),
  
  # stratified analyses, by BMI groups
  as.data.frame(sPLS_list_wii$bmi0_strat$loadingsX_sPLS) %>% mutate(var = rownames(.), set_analyses="Normal body mass index\n(N=1548)") %>% gather(key="component", value="loadings", -var, -set_analyses) %>% mutate(id.set = "bmi0"),
  as.data.frame(sPLS_list_wii$bmi1_strat$loadingsX_sPLS) %>% mutate(var = rownames(.), set_analyses="Overweight\n(N=1720)")             %>% gather(key="component", value="loadings", -var, -set_analyses) %>% mutate(id.set = "bmi1"),
  as.data.frame(sPLS_list_wii$bmi2_strat$loadingsX_sPLS) %>% mutate(var = rownames(.), set_analyses="Obese\n(N=723)")                   %>% gather(key="component", value="loadings", -var, -set_analyses) %>% mutate(id.set = "bmi2"),
  
  # stratified analyses, number of chronic diseases excluding diabetes
  as.data.frame(sPLS_list_wii$fmm0_strat$loadingsX_sPLS) %>% mutate(var = rownames(.), set_analyses="Participants without\nchronic disease\nN=2362")           %>% gather(key="component", value="loadings", -var, -set_analyses) %>% mutate(id.set = "fmm0.1"),
  as.data.frame(sPLS_list_wii$fmm1_strat$loadingsX_sPLS) %>% mutate(var = rownames(.), set_analyses="Participants with at least one\nchronic disease\nN=1629") %>% gather(key="component", value="loadings", -var, -set_analyses) %>% mutate(id.set = "fmm1.1")) %>% 
  mutate(set_analyses = factor(set_analyses, levels=c("Full sample\nN=3991",
                                                      "Participant aged\n<74 years (N=3001)", "Participant aged\n74 years and older (N=990)",
                                                      "Men\n(N=2961)", "Women\n(N=1030)", 
                                                      "Normal body mass index\n(N=1548)", "Overweight\n(N=1720)", "Obese\n(N=723)", 
                                                      "Participants without\nchronic disease\nN=2362","Participants with at least one\nchronic disease\nN=1629")))

# > association between scores and mortality
load("E:/PC_FIXE/Analysis/04_MLpaper/02_PLS/PLS_mathilde_2/3_associations/01_associations_wii.rda")
load("E:/PC_FIXE/Analysis/04_MLpaper/02_PLS/PLS_mathilde_2/3_associations/02_association_internal_validation.rda")
load("E:/PC_FIXE/Analysis/04_MLpaper/02_PLS/PLS_mathilde_2/3_associations/03_associations_external_validation.rda")
load("E:/PC_FIXE/Analysis/04_MLpaper/02_PLS/PLS_mathilde_2/3_associations/04_associations_wii_sensitivity_analyses.rda")

# > linear and restricted cubic splines models
load("E:/PC_FIXE/Analysis/04_MLpaper/02_PLS/PLS_mathilde_2/3_associations/01_linear_splines_wii.rda")
load("E:/PC_FIXE/Analysis/04_MLpaper/02_PLS/PLS_mathilde_2/3_associations/03_linear_splines_external_validation.rda")

# > predictive gain and 95% CI 
load("E:/PC_FIXE/Analysis/04_MLpaper/02_PLS/PLS_mathilde_2/4_predictive_gain/01_boot_ci.rda")
load("E:/PC_FIXE/Analysis/04_MLpaper/02_PLS/PLS_mathilde_2/4_predictive_gain/02_boot_ci_internal_validation.rda")
load("E:/PC_FIXE/Analysis/04_MLpaper/02_PLS/PLS_mathilde_2/4_predictive_gain/03_boot_ci_external_validation.rda")
load("E:/PC_FIXE/Analysis/04_MLpaper/02_PLS/PLS_mathilde_2/4_predictive_gain/04_boot_ci_sensitivity_analyses.rda")

# -----------------------------------------------------------------------------
# SUPPLEMENTARY TABLES 
# -----------------------------------------------------------------------------

# ----------
# Supplementary Table 2. 
# > see 5_sensitivity_analysis_including_waking_period.R

# ----------
# Supplementary Table 3. Baseline characteristics by subgroups in Whitehall II accelerometer sub-study

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
         fbmi_i_3_1=factor(fbmi_i_3_1), fbmi_i_3_2=factor(fbmi_i_3_2))  %>% 
  
  # Groups for sensitivity analyses
  mutate(Age_group = if_else(Age <74, 0, 1))

# > Difference between those aged <74 years old (n=3001) and those aged 74+ years (n=990)
# > Among those aged <74 years old (n=3001)
X_desc_tab_age0 <- tableby(Mortality_status ~ Age + Sex + Ethnicity + Education + Marital_status + 
                             Smoking_status + Alcohol_consumption + Fruits_vegetables_intake + 
                             BMI + Hypertension + Hyperlipidemia + Prevalent_diabetes + Multimorbidity_index + Number_ADL + Number_IADL +
                             Mean_acceleration_mg + 
                             Total_SB_duration_min + Total_LIPA_duration_min + Total_MVPA_duration_min + 
                             Mean_duration_SB_block + Mean_duration_LIPA_block + Mean_duration_MVPA_block + 
                             SB_duration_in_0_10_min_block + LIPA_duration_in_0_10_min_block + MVPA_duration_in_0_10_min_block + 
                             LIPA_duration_in_10_min_and_more_block + MVPA_duration_in_10_min_and_more_block +
                             SB_duration_in_10_30_min_block + SB_duration_in_30_min_and_more_block + 
                             Nblocks_SB + Nblocks_LIPA + Nblocks_MVPA + Ndays_meeting_MVPA_guidelines + 
                             Intensity_intercept + Intensity_gradient +
                             Timing_5_most_active_hours,  
                           data=X_desc[which(X_desc$Age<74),], 
                           # Option for output table
                           control=tableby.control(total=FALSE,
                                                   digits=1, digits.p = 3,
                                                   numeric.simplify = TRUE,
                                                   cat.simplify = TRUE,
                                                   numeric.stats = c("Nmiss", "meansd")))
summary(X_desc_tab_age0, text = TRUE, pfootnote = TRUE, title='Among those aged <74 y (N=3001)',
        labelTranslations = my.labels)

# > Among those aged 74+ years (n=990)
X_desc_tab_age1 <- tableby(Mortality_status ~ Age + Sex + Ethnicity + Education + Marital_status + 
                             Smoking_status + Alcohol_consumption + Fruits_vegetables_intake + 
                             BMI + Hypertension + Hyperlipidemia + Prevalent_diabetes + Multimorbidity_index + Number_ADL + Number_IADL +
                             Mean_acceleration_mg + 
                             Total_SB_duration_min + Total_LIPA_duration_min + Total_MVPA_duration_min + 
                             Mean_duration_SB_block + Mean_duration_LIPA_block + Mean_duration_MVPA_block + 
                             SB_duration_in_0_10_min_block + LIPA_duration_in_0_10_min_block + MVPA_duration_in_0_10_min_block + 
                             LIPA_duration_in_10_min_and_more_block + MVPA_duration_in_10_min_and_more_block +
                             SB_duration_in_10_30_min_block + SB_duration_in_30_min_and_more_block + 
                             Nblocks_SB + Nblocks_LIPA + Nblocks_MVPA + Ndays_meeting_MVPA_guidelines + 
                             Intensity_intercept + Intensity_gradient +
                             Timing_5_most_active_hours, 
                           data=X_desc[which(X_desc$Age>=74),], 
                           # Option for output table
                           control=tableby.control(total=FALSE,
                                                   digits=1, digits.p = 3,
                                                   numeric.simplify = TRUE,
                                                   cat.simplify = TRUE,
                                                   numeric.stats = c("Nmiss", "meansd")))
summary(X_desc_tab_age1, text = TRUE, pfootnote = TRUE, title='Among those aged 74+ y (N=990)',
        labelTranslations = my.labels)

# > Difference between sexes
# > Among men (n=2961)
X_desc_tab_sex0 <- tableby(Mortality_status ~ Age + Sex + Ethnicity + Education + Marital_status + 
                             Smoking_status + Alcohol_consumption + Fruits_vegetables_intake + 
                             BMI + Hypertension + Hyperlipidemia + Prevalent_diabetes + Multimorbidity_index + Number_ADL + Number_IADL +
                             Mean_acceleration_mg + 
                             Total_SB_duration_min + Total_LIPA_duration_min + Total_MVPA_duration_min + 
                             Mean_duration_SB_block + Mean_duration_LIPA_block + Mean_duration_MVPA_block + 
                             SB_duration_in_0_10_min_block + LIPA_duration_in_0_10_min_block + MVPA_duration_in_0_10_min_block + 
                             LIPA_duration_in_10_min_and_more_block + MVPA_duration_in_10_min_and_more_block +
                             SB_duration_in_10_30_min_block + SB_duration_in_30_min_and_more_block + 
                             Nblocks_SB + Nblocks_LIPA + Nblocks_MVPA + Ndays_meeting_MVPA_guidelines + 
                             Intensity_intercept + Intensity_gradient +
                             Timing_5_most_active_hours, 
                           data=X_desc[which(X_desc$Sex==0),], 
                           # Option for output table
                           control=tableby.control(total=FALSE,
                                                   digits=1, digits.p = 3,
                                                   numeric.simplify = TRUE,
                                                   cat.simplify = TRUE,
                                                   numeric.stats = c("Nmiss", "meansd")))
summary(X_desc_tab_sex0, text = TRUE, pfootnote = TRUE, title='Among men (N=2961)',
        labelTranslations = my.labels)

# > Among women (n=1030)
X_desc_tab_sex1 <- tableby(Mortality_status ~ Age + Sex + Ethnicity + Education + Marital_status + 
                             Smoking_status + Alcohol_consumption + Fruits_vegetables_intake + 
                             BMI + Hypertension + Hyperlipidemia + Prevalent_diabetes + Multimorbidity_index + Number_ADL + Number_IADL +
                             Mean_acceleration_mg + 
                             Total_SB_duration_min + Total_LIPA_duration_min + Total_MVPA_duration_min + 
                             Mean_duration_SB_block + Mean_duration_LIPA_block + Mean_duration_MVPA_block + 
                             SB_duration_in_0_10_min_block + LIPA_duration_in_0_10_min_block + MVPA_duration_in_0_10_min_block + 
                             LIPA_duration_in_10_min_and_more_block + MVPA_duration_in_10_min_and_more_block +
                             SB_duration_in_10_30_min_block + SB_duration_in_30_min_and_more_block + 
                             Nblocks_SB + Nblocks_LIPA + Nblocks_MVPA + Ndays_meeting_MVPA_guidelines + 
                             Intensity_intercept + Intensity_gradient +
                             Timing_5_most_active_hours, 
                           data=X_desc[which(X_desc$Sex==1),], 
                           # Option for output table
                           control=tableby.control(total=FALSE,
                                                   digits=1, digits.p = 3,
                                                   numeric.simplify = TRUE,
                                                   cat.simplify = TRUE,
                                                   numeric.stats = c("Nmiss", "meansd")))
summary(X_desc_tab_sex1, text = TRUE, pfootnote = TRUE, title='Among women (N=1030)',
        labelTranslations = my.labels)

# > Difference between BMI groups
# > Among normal BMI group (n=1548)
X_desc_tab_bmi0 <- tableby(Mortality_status ~ Age + Sex + Ethnicity + Education + Marital_status + 
                             Smoking_status + Alcohol_consumption + Fruits_vegetables_intake + 
                             BMI + Hypertension + Hyperlipidemia + Prevalent_diabetes + Multimorbidity_index + Number_ADL + Number_IADL +
                             Mean_acceleration_mg + 
                             Total_SB_duration_min + Total_LIPA_duration_min + Total_MVPA_duration_min + 
                             Mean_duration_SB_block + Mean_duration_LIPA_block + Mean_duration_MVPA_block + 
                             SB_duration_in_0_10_min_block + LIPA_duration_in_0_10_min_block + MVPA_duration_in_0_10_min_block + 
                             LIPA_duration_in_10_min_and_more_block + MVPA_duration_in_10_min_and_more_block +
                             SB_duration_in_10_30_min_block + SB_duration_in_30_min_and_more_block + 
                             Nblocks_SB + Nblocks_LIPA + Nblocks_MVPA + Ndays_meeting_MVPA_guidelines + 
                             Intensity_intercept + Intensity_gradient +
                             Timing_5_most_active_hours,  
                           data=X_desc[which(X_desc$BMI==0),], 
                           # Option for output table
                           control=tableby.control(total=FALSE,
                                                   digits=1, digits.p = 3,
                                                   numeric.simplify = TRUE,
                                                   cat.simplify = TRUE,
                                                   numeric.stats = c("Nmiss", "meansd")))
summary(X_desc_tab_bmi0, text = TRUE, pfootnote = TRUE, title='Among normal BMI group (N=1548)',
        labelTranslations = my.labels)

# > Among overweighted participants (n=1720)
X_desc_tab_bmi1 <- tableby(Mortality_status ~ Age + Sex + Ethnicity + Education + Marital_status + 
                             Smoking_status + Alcohol_consumption + Fruits_vegetables_intake + 
                             BMI + Hypertension + Hyperlipidemia + Prevalent_diabetes + Multimorbidity_index + Number_ADL + Number_IADL +
                             Mean_acceleration_mg + 
                             Total_SB_duration_min + Total_LIPA_duration_min + Total_MVPA_duration_min + 
                             Mean_duration_SB_block + Mean_duration_LIPA_block + Mean_duration_MVPA_block + 
                             SB_duration_in_0_10_min_block + LIPA_duration_in_0_10_min_block + MVPA_duration_in_0_10_min_block + 
                             LIPA_duration_in_10_min_and_more_block + MVPA_duration_in_10_min_and_more_block +
                             SB_duration_in_10_30_min_block + SB_duration_in_30_min_and_more_block + 
                             Nblocks_SB + Nblocks_LIPA + Nblocks_MVPA + Ndays_meeting_MVPA_guidelines + 
                             Intensity_intercept + Intensity_gradient +
                             Timing_5_most_active_hours,  
                           data=X_desc[which(X_desc$BMI==1),], 
                           # Option for output table
                           control=tableby.control(total=FALSE,
                                                   digits=1, digits.p = 3,
                                                   numeric.simplify = TRUE,
                                                   cat.simplify = TRUE,
                                                   numeric.stats = c("Nmiss", "meansd")))
summary(X_desc_tab_bmi1, text = TRUE, pfootnote = TRUE, title='Among overweight participants (N=1720)',
        labelTranslations = my.labels)

# > Among obese participants (n=723)
X_desc_tab_bmi2 <- tableby(Mortality_status ~ Age + Sex + Ethnicity + Education + Marital_status + 
                             Smoking_status + Alcohol_consumption + Fruits_vegetables_intake + 
                             BMI + Hypertension + Hyperlipidemia + Prevalent_diabetes + Multimorbidity_index + Number_ADL + Number_IADL +
                             Mean_acceleration_mg + 
                             Total_SB_duration_min + Total_LIPA_duration_min + Total_MVPA_duration_min + 
                             Mean_duration_SB_block + Mean_duration_LIPA_block + Mean_duration_MVPA_block + 
                             SB_duration_in_0_10_min_block + LIPA_duration_in_0_10_min_block + MVPA_duration_in_0_10_min_block + 
                             LIPA_duration_in_10_min_and_more_block + MVPA_duration_in_10_min_and_more_block +
                             SB_duration_in_10_30_min_block + SB_duration_in_30_min_and_more_block + 
                             Nblocks_SB + Nblocks_LIPA + Nblocks_MVPA + Ndays_meeting_MVPA_guidelines + 
                             Intensity_intercept + Intensity_gradient +
                             Timing_5_most_active_hours, 
                           data=X_desc[which(X_desc$BMI==2),], 
                           # Option for output table
                           control=tableby.control(total=FALSE,
                                                   digits=1, digits.p = 3,
                                                   numeric.simplify = TRUE,
                                                   cat.simplify = TRUE,
                                                   numeric.stats = c("Nmiss", "meansd")))
summary(X_desc_tab_bmi2, text = TRUE, pfootnote = TRUE, title='Among obese participants (N=723)',
        labelTranslations = my.labels)


# > Differences between morbidity groups
# > Among those with 0 chronic disease (N=2362)
X_desc_tab_fmm0 <- tableby(Mortality_status ~ Age + Sex + Ethnicity + Education + Marital_status + 
                             Smoking_status + Alcohol_consumption + Fruits_vegetables_intake + 
                             BMI + Hypertension + Hyperlipidemia + Prevalent_diabetes + Multimorbidity_index + Number_ADL + Number_IADL +
                             Mean_acceleration_mg + 
                             Total_SB_duration_min + Total_LIPA_duration_min + Total_MVPA_duration_min + 
                             Mean_duration_SB_block + Mean_duration_LIPA_block + Mean_duration_MVPA_block + 
                             SB_duration_in_0_10_min_block + LIPA_duration_in_0_10_min_block + MVPA_duration_in_0_10_min_block + 
                             LIPA_duration_in_10_min_and_more_block + MVPA_duration_in_10_min_and_more_block +
                             SB_duration_in_10_30_min_block + SB_duration_in_30_min_and_more_block + 
                             Nblocks_SB + Nblocks_LIPA + Nblocks_MVPA + Ndays_meeting_MVPA_guidelines + 
                             Intensity_intercept + Intensity_gradient +
                             Timing_5_most_active_hours, 
                           data=X_desc[which(X_desc$fmm_index_acm_2==0),], 
                           # Option for output table
                           control=tableby.control(total=FALSE,
                                                   digits=1, digits.p = 3,
                                                   numeric.simplify = TRUE,
                                                   cat.simplify = TRUE,
                                                   numeric.stats = c("Nmiss", "meansd")))
summary(X_desc_tab_fmm0, text = TRUE, pfootnote = TRUE, title='Among those with 0 chronic disease (N=2362)',
        labelTranslations = my.labels)

# > Among those with at least 1 chronic disease (N=1629)
X_desc_tab_fmm1 <- tableby(Mortality_status ~ Age + Sex + Ethnicity + Education + Marital_status + 
                             Smoking_status + Alcohol_consumption + Fruits_vegetables_intake + 
                             BMI + Hypertension + Hyperlipidemia + Prevalent_diabetes + Multimorbidity_index + Number_ADL + Number_IADL +
                             Mean_acceleration_mg + 
                             Total_SB_duration_min + Total_LIPA_duration_min + Total_MVPA_duration_min + 
                             Mean_duration_SB_block + Mean_duration_LIPA_block + Mean_duration_MVPA_block + 
                             SB_duration_in_0_10_min_block + LIPA_duration_in_0_10_min_block + MVPA_duration_in_0_10_min_block + 
                             LIPA_duration_in_10_min_and_more_block + MVPA_duration_in_10_min_and_more_block +
                             SB_duration_in_10_30_min_block + SB_duration_in_30_min_and_more_block + 
                             Nblocks_SB + Nblocks_LIPA + Nblocks_MVPA + Ndays_meeting_MVPA_guidelines + 
                             Intensity_intercept + Intensity_gradient +
                             Timing_5_most_active_hours,  
                           data=X_desc[which(X_desc$fmm_index_acm_2==1),], 
                           # Option for output table
                           control=tableby.control(total=FALSE,
                                                   digits=1, digits.p = 3,
                                                   numeric.simplify = TRUE,
                                                   cat.simplify = TRUE,
                                                   numeric.stats = c("Nmiss", "meansd")))
summary(X_desc_tab_fmm1, text = TRUE, pfootnote = TRUE, title='Among those with at least 1 chronic disease (N=1629)',
        labelTranslations = my.labels)

# -----------
# Supplementary Table 4. Predictive performance of the physical activity and sedentary behaviour composite score 1 for mortality risk among participants aged <74 and participants aged ???74 years in the Whitehall II accelerometer sub-study

boot_ci_int$fs_age_boot_ci %>% 
  filter(term %in% no_dim_1_names) %>% 
  mutate(term = substr(term, 10, nchar(term))) %>%
  mutate(lab = paste0(formatC(statistic, digits=3, format = "f"), " (", formatC(conf.low, digits=3, format = "f"), ", ", formatC(conf.high, digits=3, format = "f"), ")")) %>%
  select(type, term, lab)

boot_ci_int$fs_age_boot_ci %>% 
  filter(term %in% full_names) %>% 
  mutate(term = substr(term, 6, nchar(term))) %>%
  mutate(lab = paste0(formatC(statistic, digits=3, format = "f"), " (", formatC(conf.low, digits=3, format = "f"), ", ", formatC(conf.high, digits=3, format = "f"), ")")) %>%
  select(type, term, lab)

boot_ci_int$fs_age_boot_ci %>% 
  filter(term %in% c("d_no_dim_1_aic_b", "d_no_dim_1_c_b")) %>% 
  mutate(term = paste0("d_", substr(term, 12, nchar(term)))) %>%
  mutate(lab = paste0(formatC(statistic, digits=3, format = "f"), " (", formatC(conf.low, digits=3, format = "f"), ", ", formatC(conf.high, digits=3, format = "f"), ")")) %>%
  select(type, term, lab)


# -----------
# Supplementary Table 5. Predictive performance of the physical activity and sedentary behaviour composite score 1 for mortality risk by sex in Whitehall II accelerometer sub-study

boot_ci_int$fs_sex_boot_ci %>% 
  filter(term %in% no_dim_1_names) %>% 
  mutate(term = substr(term, 10, nchar(term))) %>%
  mutate(lab = paste0(formatC(statistic, digits=3, format = "f"), " (", formatC(conf.low, digits=3, format = "f"), ", ", formatC(conf.high, digits=3, format = "f"), ")")) %>%
  filter(term %in% c("r2_b", "aic_b", "c_b", "sensitivity_cd_b", "specificity_cd_b")) %>% 
  select(type, term, lab)

boot_ci_int$fs_sex_boot_ci %>% 
  filter(term %in% full_names) %>% 
  mutate(term = substr(term, 6, nchar(term))) %>%
  mutate(lab = paste0(formatC(statistic, digits=3, format = "f"), " (", formatC(conf.low, digits=3, format = "f"), ", ", formatC(conf.high, digits=3, format = "f"), ")")) %>%
  filter(term %in% c("r2_b", "aic_b", "c_b", "sensitivity_cd_b", "specificity_cd_b")) %>% 
  select(type, term, lab)

boot_ci_int$fs_sex_boot_ci %>% 
  filter(term %in% c("d_no_dim_1_aic_b", "d_no_dim_1_c_b")) %>% 
  mutate(term = paste0("d_", substr(term, 12, nchar(term)))) %>%
  mutate(lab = paste0(formatC(statistic, digits=3, format = "f"), " (", formatC(conf.low, digits=3, format = "f"), ", ", formatC(conf.high, digits=3, format = "f"), ")")) %>%
  select(type, term, lab)

# -----------
# Supplementary Table 6. Predictive performance of physical activity and sedentary behaviour composite scores for mortality risk by body mass index status in Whitehall II accelerometer sub-study

boot_ci_int$fs_bmi_boot_ci %>% 
  filter(term %in% no_dim_1_names) %>% 
  mutate(term = substr(term, 10, nchar(term))) %>%
  mutate(lab = paste0(formatC(statistic, digits=3, format = "f"), " (", formatC(conf.low, digits=3, format = "f"), ", ", formatC(conf.high, digits=3, format = "f"), ")")) %>%
  filter(term %in% c("r2_b", "aic_b", "c_b", "sensitivity_cd_b", "specificity_cd_b")) %>% 
  select(type, term, lab)

boot_ci_int$fs_bmi_boot_ci %>% 
  filter(term %in% full_names) %>% 
  mutate(term = substr(term, 6, nchar(term))) %>%
  mutate(lab = paste0(formatC(statistic, digits=3, format = "f"), " (", formatC(conf.low, digits=3, format = "f"), ", ", formatC(conf.high, digits=3, format = "f"), ")")) %>%
  filter(term %in% c("r2_b", "aic_b", "c_b", "sensitivity_cd_b", "specificity_cd_b")) %>% 
  select(type, term, lab)

boot_ci_int$fs_bmi_boot_ci %>% 
  filter(term %in% c("d_no_dim_1_aic_b", "d_no_dim_1_c_b")) %>% 
  mutate(term = paste0("d_", substr(term, 12, nchar(term)))) %>%
  mutate(lab = paste0(formatC(statistic, digits=3, format = "f"), " (", formatC(conf.low, digits=3, format = "f"), ", ", formatC(conf.high, digits=3, format = "f"), ")")) %>%
  select(type, term, lab)

# -----------
# Supplementary Table 7. Predictive performance of physical activity and sedentary behaviour composite scores for mortality risk by morbidity status in Whitehall II accelerometer sub-study

# > model excluding PA and SB composite score (reference model)
boot_ci_int$fmm_boot_ci %>% 
  filter(term %in% no_dim_1_names) %>% 
  mutate(term = substr(term, 10, nchar(term))) %>%
  mutate(lab = paste0(formatC(statistic, digits=3, format = "f"), " (", formatC(conf.low, digits=3, format = "f"), ", ", formatC(conf.high, digits=3, format = "f"), ")")) %>%
  select(type, term, lab) 

# > model fully adjusted
boot_ci_int$fmm_boot_ci %>% 
  filter(term %in% full_names) %>%
  mutate(term = substr(term, 6, nchar(term))) %>%
  mutate(lab = paste0(formatC(statistic, digits=3, format = "f"), " (", formatC(conf.low, digits=3, format = "f"), ", ", formatC(conf.high, digits=3, format = "f"), ")")) %>%
  select(type, term, lab)

# > difference in AIC and C-index between reference model and fully adjusted model
boot_ci_int$fmm_boot_ci %>% 
  filter(term %in% c("d_no_dim_1_aic_b", "d_no_dim_1_c_b")) %>% 
  mutate(term = paste0("d_", substr(term, 12, nchar(term)))) %>%
  mutate(lab = paste0(formatC(statistic, digits=3, format = "f"), " (", formatC(conf.low, digits=3, format = "f"), ", ", formatC(conf.high, digits=3, format = "f"), ")")) %>%
  select(type, term, lab)

# -----------
# Supplementary Table 8. Sample characteristics in 2014-2017 by mortality status at the end of follow-up (February 2021) in CoLaus accelerometer sub-study

data_60 <- sPLS_list_colaus$fs_colaus$new_dat[which(sPLS_list$fs_colaus$new_dat$fage_s>=60),]
fg_imp <- median(data_60[which(data_60$ffruitvg_i_3_9==0), "ffruitvg_i_3"]); fg_imp #2

X_desc_colaus <- sPLS_list$fs_colaus$new_dat %>% 
  # > select those older than 60 y old
  filter(fage_s >= 60) %>% 
  # > impute missing fg with median fg values among those > 60 y
  mutate(ffruitvg_i_3 = ifelse(ffruitvg_i_3 == 9, fg_imp, ffruitvg_i_3)) %>% 
  select(
    stno,
    # Mortality status
    "Mortality_status"="ef21ac",
    # Socio-demographics predictors
    "Age"="fage_s", "Sex"="sex", "Ethnicity"="ethnicity", "Education"="edu_conti", "Marital_status"="fstatusx_i_2", 
    # Behavioral predictors
    "Smoking_status"= "F2sbsmk_i", "Alcohol_consumption"= "funitwk0_i_3", "Fruits_vegetables_intake"="ffruitvg_i_3",
    # Health-related predictors
    "BMI"="fbmi_i_3", "Hypertension"="hypertension", "Hyperlipidemia"="ldl_4_1_drgs", "Prevalent_diabetes"="diabetes_prevalent", "Multimorbidity_index"="fmm_index_acm", "Multimorbidity_index_2"="fmm_index_acm_2",  "ADL_2"="fadl_tot_2", "IADL_2"="ifadl_tot_2",
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
         Smoking_status = factor(Smoking_status), Alcohol_consumption = factor(Alcohol_consumption), Fruits_vegetables_intake = factor(Fruits_vegetables_intake), 
         BMI = factor(BMI), Hypertension=factor(Hypertension), Hyperlipidemia=factor(Hyperlipidemia), Prevalent_diabetes=factor(Prevalent_diabetes),
         ADL_2 = factor(ADL_2), IADL_2 = factor(IADL_2), Multimorbidity_index_2=factor(Multimorbidity_index_2)) 

# Descriptive statistics
X_desc_tab_colaus <- tableby(Mortality_status ~ Age + Sex + Ethnicity + Education + Marital_status + 
                               Smoking_status + Alcohol_consumption + Fruits_vegetables_intake + 
                               BMI + Hypertension + Hyperlipidemia + Prevalent_diabetes + 
                               Multimorbidity_index + Multimorbidity_index_2 + ADL_2 + IADL_2 + 
                               Intensity_intercept + Total_SB_duration_min + 
                               Mean_duration_SB_block + Nblocks_SB + SB_duration_in_0_10_min_block + SB_duration_in_10_30_min_block + SB_duration_in_30_min_and_more_block + 
                               Total_LIPA_duration_min + Mean_duration_LIPA_block + Nblocks_LIPA + LIPA_duration_in_0_10_min_block + LIPA_duration_in_10_min_and_more_block + 
                               Total_MVPA_duration_min + Mean_duration_MVPA_block + Nblocks_MVPA + MVPA_duration_in_0_10_min_block + MVPA_duration_in_10_min_and_more_block + 
                               Ndays_meeting_MVPA_guidelines + Mean_acceleration_mg + Intensity_gradient + Timing_5_most_active_hours, 
                             data=X_desc_colaus, 
                             # Option for output table
                             control=tableby.control(total=FALSE,
                                                     digits=1, digits.p = 3,
                                                     numeric.simplify = TRUE,
                                                     cat.simplify = TRUE,
                                                     numeric.stats = c("Nmiss", "meansd")))
summary(X_desc_tab_colaus, text = TRUE, pfootnote = TRUE, title = "Baseline characteristics by mortality status in CoLaus accelerometer sub-study",
        labelTranslations = my.labels.colaus)

# -----------------------------------------------------------------------------
# SUPPLEMENTARY FIGURES 
# -----------------------------------------------------------------------------


# -----------
# Supplementary Figure 2. Correlation plot of the 21 accelerometer-derived physical activity and sedentary behaviour variables	

acc_cor <- dat %>% 
  select(stno, ends_with("_wei"), -starts_with("z_"), -dur_day_min_wei) %>% 
  # Rename variables to remove the end of the labels
  tidyr::gather(key = "var", value = "value", -stno) %>% 
  left_join(tab.name, by = c("var")) %>% 
  select(-var, -var.group) %>% 
  mutate(varname = sub(" - .*", "", varname)) %>%  
  #mutate(varname = factor(varname, levels = c('Intensity intercept', 'Total duration in SB (min/day)', 'Number of SB bouts','Mean duration of SB bouts','Time in <10 min SB bouts (min/day)', 'Time in 10-30 min SB bouts (min/day)', 'Time in >=30 min SB bouts (min/day)', 'Total duration in LIPA (min/day)', 'Mean duration of LIPA bouts', 'Number of LIPA bouts', 'Time in <10 min LIPA bouts (min/day)', 'Time in >=10 min LIPA bouts (min/day)','Total duration in MVPA (min/day)',  'Mean duration of MVPA bouts', 'Number of MVPA bouts',  'Time in <10 min MVPA bouts (min/day)',  'Time in >=10 min MVPA bouts (min/day)', 'Number of days meeting physical activity guidelines', 'Acceleration (mg)','Intensity gradient', 'Physical activity chronotype'))) %>%
  mutate(varname = recode(varname, 
                          'Time in 10-30 min SB bouts (min/day)'  = 'Time in 10-29.9 min SB bouts (min/day)', 
                          'Time in >=30 min SB bouts (min/day)'   = paste0('Time in ', intToUtf8(8805), '30 min SB bouts (min/day)'), 
                          'Time in >=10 min LIPA bouts (min/day)' = paste0('Time in ', intToUtf8(8805), '10 min LIPA bouts (min/day)'),
                          'Time in >=10 min MVPA bouts (min/day)' = paste0('Time in ', intToUtf8(8805), '10 min MVPA bouts (min/day)'),
                          "Number of days with >= 30 min of MVPA" = paste0("Number of days with ", intToUtf8(8805), "30 min of MVPA"))) %>%
  mutate(varname = factor(varname, levels = c('Mean acceleration (mg)', 
                                              'Total duration in SB (min/day)', 'Total duration in LIPA (min/day)', 'Total duration in MVPA (min/day)', 
                                              'Mean duration of SB bouts (min)', 'Mean duration of LIPA bouts (min)', 'Mean duration of MVPA bouts (min)', 
                                              'Time in <10 min SB bouts (min/day)', 'Time in <10 min LIPA bouts (min/day)', 'Time in <10 min MVPA bouts (min/day)', 
                                              'Time in 10-29.9 min SB bouts (min/day)', paste0('Time in ', intToUtf8(8805), '10 min LIPA bouts (min/day)'), paste0('Time in ', intToUtf8(8805), '10 min MVPA bouts (min/day)'), 
                                              paste0('Time in ', intToUtf8(8805), '30 min SB bouts (min/day)'), 
                                              'Number of SB bouts (N/day)', 'Number of LIPA bouts (N/day)', 'Number of MVPA bouts (N/day)', 
                                              paste0("Number of days with ", intToUtf8(8805), "30 min of MVPA"), 
                                              'Intensity intercept', 'Intensity gradient', 
                                              'Timing of physical activity'))) %>% 
  tidyr::spread(key = "varname", value = "value") %>% 
  # Remove useless variables
  dplyr::select(-stno) %>% 
  # Compute correlation coefficient
  cor(.)

corrplot(acc_cor, 
         method="color", 
         #col=col(200),  
         diag=FALSE, 
         type="lower", 
         order="original", 
         addCoef.col = "black",
         cl.cex = 1.5,
         number.cex = 1.35,
         tl.cex = 1.5,
         tl.col = "black",
         cl.pos = 'r',
         mar=c(0,0,0,0))

# -----------
# Supplementary Figure 3. Factor loadings of physical activity and sedentary behaviour variables, including features standardised to waking period duration, in composite scores identified as predictors of mortality risk	
# > see 5_sensitivity_analysis_including_waking_period.R



