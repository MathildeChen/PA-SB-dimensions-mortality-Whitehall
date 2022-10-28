# Script: plsRCox_0_functions.R
# Authors: CHEN Mathilde, 2022
# Script with home-made functions

# --------------------------------------------------------------------------------------------
# Function to extract coefficients, HR and p.values from coxph() models
mod.out <- function(mod){
  x <- summary(mod)
  
  mod.output <- 
    # extract data from models outputs
    data.frame(
      var = row.names(x$coefficients),                         # variable name
      beta = signif(x$coefficients[,1], digits=5),             # coefficient beta
      HR = signif(x$coefficients[,2], digits=5),               # HR, exp(beta)
      HR.confint.lower = signif(x$conf.int[,"lower .95"], 5),  # HR 95% CI lower bound
      HR.confint.upper = signif(x$conf.int[,"upper .95"],5),   # HR 95% CI upper bound 
      p.value = x$coefficients[,"Pr(>|z|)"]) %>% 
    # labs for HR and p.value
    mutate(HR.lab = paste0(format(round(HR, 2), nsmall = 2), " (", format(round(HR.confint.lower, 2), nsmall = 2), ", ", format(round(HR.confint.upper, 2), nsmall = 2), ")"),
           #mutate(HR.lab = paste0(format(signif(HR, 2), nsmall = 2), " (", format(signif(HR.confint.lower, 2), nsmall = 2), ", ", format(signif(HR.confint.upper, 2), nsmall = 2), ")"),
           p.value.lab = if_else(p.value < 0.001, "<.001", as.character(signif(p.value, digits=2)))) 
  
  return(mod.output)
  
}

# --------------------------------------------------------------------------------------------
# Function to extract AIC, BIC, Harrell's C-index, Royston's R² of a coxph model

mod.perf <- function(mod){
  
  mod.perf <- data.frame(
    "Royston_R2"= round(royston(mod)["R.D"], 3),
    AIC = round(AIC(mod), 2),
    BIC = round(BIC(mod), 2),
    "Harrells_C" = round(mod$concordance["concordance"], 3),
  row.names = NULL)
  
  return(mod.perf)
  
}


# --------------------------------------------------------------------------------------------
# Function to plot model outputs
plot.mod <- function(mod, title = NULL){
  
  plot_mod <- mod.out(mod) %>% 
    mutate(var = if_else(substr(var,1,2)=="z_", substr(var,3,nchar(var)), var)) %>%
    left_join(tab.name, by="var") %>% 
    filter(varname != "Waking time") %>%
    mutate(varname = factor(varname, levels=
                              rev(c("Age, per 5 years increment", "Female sex", "Non-white ethnicity", "Education", "Not married/cohabitating",
                                    'Smoking status', 'Long-term ex-smokers', 'Recent ex-smokers or current smokers', 'No alcohol intake', 'High alcohol intake', 'Healthy diet score', '<Daily fruits and veg intake', 'Daily fruits and veg intake',
                                    'BMI', 'Overweight', 'Obese', 'Hypertension', 'Hyperlipidemia', 'Prevalent diabetes', 'Number of chronic conditions, per new condition', 'Number of ADL', 'Number of instrumental ADL', 'Having at least 1 ADL', 'Having at least 1 instrumental ADL',
                                    'Self-reported MVPA: 0 h/week', 'Self-reported hours of MVPA <2.5 h/week', 'Self-reported hours of MVPA',
                                    'Time in SB (min/day)', 'Number of SB blocks', 'Mean duration of SB block', 'Time in <10 min blocks of SB (min/day)', 'Time in 10-30 min blocks of SB (min/day)', 'Time in >=30 min blocks of SB (min/day)', 'Intensity intercept',
                                    'Time in LIPA (min/day)', 'Number of LIPA blocks', 'Mean duration of LIPA block', 'Time in <10 min blocks of LIPA (min/day)', 'Time in >=10 min blocks of LIPA (min/day)',
                                    'Time in MVPA (min/day)', 'Number of MVPA blocks', 'Mean duration of MVPA block', 'Time in <10 min blocks of MVPA (min/day)', 'Time in >=10 min blocks of MVPA (min/day)', 'Intensity gradient','Acceleration (mg)', "Number of days with at least 30 min of MVPA",
                                    'Most active 5hrs timing', 'Component 1', 'Component 2', 'Component 1 (PLS)', 'Component 2 (PLS)')))) %>%
    mutate(lab = if_else(p.value<0.001, paste0(HR.lab, "; p", p.value.lab), paste0(HR.lab, "; p=", p.value.lab))) %>%
    mutate(color = if_else(p.value>0.05,"3", if_else(HR>1, "1", "2"))) %>% 
    ggplot(., aes(x = varname, y = HR, ymin = HR.confint.lower, ymax = HR.confint.upper, color = color)) + 
    geom_hline(yintercept = 1, color = "grey", lty = 1) + 
    geom_hline(yintercept = c(0.5, 0.75, 1, 1.25, 1.5), color = "grey", lty = 2) +  
    geom_point(size = 2, position = position_dodge2(width = 0.7)) + 
    geom_linerange(size = 0.7, position = position_dodge2(width = 0.7)) + 
    geom_text(aes(y = 2.75, label = lab), color = "black", size = 2.5) + 
    scale_color_manual(values = c(wesanderson::wes_palette("Zissou1", n = 5, type = "discrete")[5],
                                  wesanderson::wes_palette("Zissou1", n = 5, type = "discrete")[1],
                                  "darkgrey")) + 
    coord_flip()+
    theme_bw() + 
    theme(legend.position = "None", 
          panel.grid = element_blank(),
          axis.title.y = element_blank()) +
    labs(y = "HR (95% CI)", caption = paste0("AIC: ", round(AIC(mod), 2), "; C-index: ", round(mod$concordance['concordance'], 4))) +  
    ggtitle(paste0(title)) +
    scale_y_continuous(limits = c(0.2, 3.2), breaks = c(0.5, 0.75, 1, 1.25, 1.5), labels = c(0.5, 0.75, 1, 1.25, 1.5))
  return(plot_mod)
}

# --------------------------------------------------------------------------------------------
# Function to plot factors loadings 
# from sPLS models (see 1-1_sPLS_scores_WII.R script)

# > colors
plot_factors_loadings <- function(tab_loadings, add.theme = NULL){
  
  p <- tab_loadings %>% 
    mutate(var = if_else(substr(var,1,2)=="z_", substr(var,3,nchar(var)), var)) %>%
    left_join(tab.name %>% select(var, varname), by="var") %>% 
    filter(varname != "Waking time") %>%
    mutate(varname = factor(varname, levels=rev(c(
      'Intensity intercept', 'Total duration in SB (min/day)', 'Number of SB bouts','Mean duration of SB bouts','Time in <10 min SB bouts (min/day)', 'Time in 10-30 min SB bouts (min/day)', 'Time in >=30 min SB bouts (min/day)', 
      'Total duration in LIPA (min/day)', 'Mean duration of LIPA bouts', 'Number of LIPA bouts', 'Time in <10 min LIPA bouts (min/day)', 'Time in >=10 min LIPA bouts (min/day)',
      'Total duration in MVPA (min/day)',  'Mean duration of MVPA bouts', 'Number of MVPA bouts',  'Time in <10 min MVPA bouts (min/day)',  'Time in >=10 min MVPA bouts (min/day)', 
      'Intensity gradient', 'Acceleration (mg)','Number of days meeting physical activity guidelines', 'Physical activity chronotype')))) %>%
    mutate(component=recode(component, "comp 1"="PA and SB composite score 1", "comp 2"="PA and SB composite score 2", "comp 3"="PA and SB composite score 3", "comp 4"="PA and SB composite score 4")) %>% 
    mutate(fill_load = if_else(loadings > 0.2, 0, if_else(loadings< -0.2, 1, 2))) %>% 
    ggplot(., aes(x=loadings, y=varname, fill = as.factor(fill_load))) +
    #geom_vline(xintercept = c(-0.5, -0.25, 0, 0.25, 0.5), color = "grey", lty=2, size=0.5) +
    geom_col() + 
    facet_grid(set_analyses~component, 
               scales = "free_y", space = "free_y", switch = "y") + 
    labs(x = "Loadings") +
    scale_fill_manual(values = c(pal[1], pal[5], "grey")) +
    theme_cowplot() +
    theme(axis.title.y = element_blank(),
          strip.background = element_rect(color="black"),
          #strip.background.y = element_blank(),
          panel.border = element_rect(color="black"),
          panel.grid.major.x = element_line(color = "lightgrey", size=0.1),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          legend.position = "none", 
          strip.placement = "outside",
          strip.text.y.left = element_text(angle = 0)) 
  #+ lims(x=c(-1,1))
  
  return(p + add.theme)
  
}

# > Version with attenuated colors for absolute loads < 0.2
plot_factors_loadings_bw <- function(tab_loadings, add.theme = NULL){
  
  p <- tab_loadings %>% 
    mutate(var = if_else(substr(var,1,2)=="z_", substr(var, 3, nchar(var)), var)) %>%
    left_join(tab.name %>% select(var, varname), by="var") %>% 
    filter(varname != "Waking time") %>%
    mutate(dimension = recode(varname, 
      'Mean acceleration (mg)'                              ='Overall activity level',
      'Total duration in SB (min/day)'                      ='Total duration', 
      'Total duration in LIPA (min/day)'                    ='Total duration', 
      'Total duration in MVPA (min/day)'                    ='Total duration',  
      'Total duration in MPA (min/day)'                     ='Total duration',  
      'Total duration in VPA (min/day)'                     ='Total duration',  
      'Mean duration of SB bouts (min)'                     ='Bouts duration',
      'Mean duration of LIPA bouts (min)'                   ='Bouts duration', 
      'Mean duration of MVPA bouts (min)'                   ='Bouts duration', 
      'Mean duration of MPA bouts (min)'                    ='Bouts duration', 
      'Mean duration of VPA bouts (min)'                    ='Bouts duration', 
      'Time in <10 min SB bouts (min/day)'                  ='Bouts duration',
      'Time in <10 min LIPA bouts (min/day)'                ='Bouts duration',
      'Time in <10 min MVPA bouts (min/day)'                ='Bouts duration',
      'Time in 10-30 min SB bouts (min/day)'                ='Bouts duration', 
      'Time in >=30 min SB bouts (min/day)'                 ='Bouts duration', 
      'Time in >=10 min LIPA bouts (min/day)'               ='Bouts duration',
      'Time in >=10 min MVPA bouts (min/day)'               ='Bouts duration', 
      'Number of SB bouts (N/day)'                          ='Frequency',
      'Number of LIPA bouts (N/day)'                        ='Frequency', 
      'Number of MVPA bouts (N/day)'                        ='Frequency',
      'Number of MPA bouts (N/day)'                         ='Frequency',
      'Number of VPA bouts (N/day)'                         ='Frequency',
      'Number of days with >= 30 min of MVPA'               ='Frequency',
      'Intensity intercept'                                 ='Intensity distribution', 
      'Intensity gradient'                                  ='Intensity distribution', 
      'Timing of physical activity'                         ='Timing')) %>%
    mutate(dimension = factor(dimension, levels = c('Overall activity level', 'Total duration', 'Bouts duration', 'Frequency', 'Intensity distribution', 'Timing'))) %>% 
    mutate(varname = recode(varname, 
                            'Time in 10-30 min SB bouts (min/day)'  = 'Time in 10-29.9 min SB bouts (min/day)', 
                            'Time in >=30 min SB bouts (min/day)'   = paste0('Time in ', intToUtf8(8805), '30 min SB bouts (min/day)'), 
                            'Time in >=10 min LIPA bouts (min/day)' = paste0('Time in ', intToUtf8(8805), '10 min LIPA bouts (min/day)'),
                            'Time in >=10 min MVPA bouts (min/day)' = paste0('Time in ', intToUtf8(8805), '10 min MVPA bouts (min/day)'),
                            'Number of days with >= 30 min of MVPA' = paste0('Number of days with ', intToUtf8(8805), '30 min of MVPA'))) %>%
    mutate(varname = factor(varname, levels = rev(c('Mean acceleration (mg)', 
                                                    'Total duration in SB (min/day)', 'Total duration in LIPA (min/day)', 'Total duration in MVPA (min/day)', 'Total duration in MPA (min/day)', 'Total duration in VPA (min/day)',
                                                    'Mean duration of SB bouts (min)', 'Mean duration of LIPA bouts (min)', 'Mean duration of MVPA bouts (min)', 'Mean duration of MPA bouts (min)', 'Mean duration of VPA bouts (min)',
                                                    'Time in <10 min SB bouts (min/day)', 'Time in <10 min LIPA bouts (min/day)', 'Time in <10 min MVPA bouts (min/day)', 
                                                    'Time in 10-29.9 min SB bouts (min/day)', paste0('Time in ', intToUtf8(8805), '10 min LIPA bouts (min/day)'), paste0('Time in ', intToUtf8(8805), '10 min MVPA bouts (min/day)'), 
                                                    paste0('Time in ', intToUtf8(8805), '30 min SB bouts (min/day)'), 'Number of SB bouts (N/day)', 'Number of LIPA bouts (N/day)', 'Number of MVPA bouts (N/day)', 'Number of MPA bouts (N/day)', 'Number of VPA bouts (N/day)', 
                                                    paste0('Number of days with ', intToUtf8(8805), '30 min of MVPA'), 'Intensity intercept', 'Intensity gradient', 'Timing of physical activity')))) %>% 
    mutate(component = recode(component, "comp 1"="PA and SB composite score 1", "comp 2"="PA and SB composite score 2", "comp 3"="PA and SB composite score 3", "comp 4"="PA and SB composite score 4")) %>% 
    #mutate(fill_load = if_else(loadings > 0, 0, 1)) %>% 
    mutate(alpha_load = if_else(loadings > 0.2, 2, if_else(loadings< -0.2, 2, 1))) %>% 
    ggplot(., aes(x=loadings, y=varname, alpha = as.factor(alpha_load))) +
    #geom_vline(xintercept = c(-0.5, -0.25, 0, 0.25, 0.5), color = "grey", lty=2, size=0.5) +
    geom_col(fill = "grey30") + 
    facet_grid(#set_analyses+
      dimension~component, 
               scales = "free_y", space = "free_y", switch = "y") + 
    labs(x = "Loadings") +
    scale_alpha_discrete(range = c(0.25,1)) +
    theme_cowplot() +
    theme(axis.title.y = element_blank(),
          strip.background = element_rect(color="black"),
          #strip.background.y = element_blank(),
          panel.border = element_rect(color="black"),
          panel.grid.major.x = element_line(color = "lightgrey", size=0.1),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          legend.position = "none", 
          strip.placement = "outside",
          strip.text.y.left = element_text(angle = 0)) 
  #+ lims(x=c(-1,1))
  
  return(p + add.theme)
  
}

# --------------------------------------------------------------------------------------------
# Function for cross-validation:
# data should be: 
# list(x = X_acc, time = T, status = Cas)
cv_eta_ncomp <- function(x, time, status, eta, ncomp, folds){
  
  # >>> 5-folds cross-validation
  cv_e = cv.coxsplsDR(list(x = x, time = time, status = status),   
                      nt = ncomp,         # varies from 0 to ncomp 
                      eta = eta,          # varies according to eta grid provided
                      givefold = folds,   # provide folds to have consistent partition during the cross-validation 
                      scaleX = FALSE,     # do not scale X (already scaled)
                      scaleY = FALSE,     # do not scale Y
                      plot.it = FALSE)    # do not plot the results of the cv.coxsplsDR() function
  
  
  # >>> format results into a table
  perf_e <- data.frame(eta        = eta,             # value of eta
                       n_nt       = 0:cv_e$nt,       # value of ncomp
                       cv.error10 = cv_e$cv.error10, # predictive mean value (iAUCSurvROC criterion) of the eta*ncomp combinaison over the 5 folds
                       cv.se10    = cv_e$cv.se10)    # corresponding standard error
  
  # >>> return the CV object and the table with results for each combination of ncomp and eta
  return(list("cv_object" = cv_e,
              "perf_e" = perf_e))
  
  
}

# --------------------------------------------------------------------------------------------
# Function that compute HR and 95% CI for association between one variable and mortality

# > linear regression
make_hr_lin <- function(model, ref, X, subset)
{
  
  ctr <- sweep(data.matrix(X), 2, ref)
  hr <- ci.exp(model, ctr.mat = ctr, subset = grep(paste0(subset), names(coef(model)))) %>% 
    as.data.frame(.) %>% 
    mutate(score=X)
  return(hr)
  
}

# > restricted cubic spline regression
make_hr_spl <- function(model, ref, X, knots, subset)
{
  
  ref_spline     <- rcspline.eval(ref, knots = knots, inclx = TRUE)
  newdata_spline <- rcspline.eval(X,   knots = knots, inclx = TRUE)
  ctr_spline     <- sweep(newdata_spline, 2, ref_spline)
  
  hr_spline <- ci.exp(model, 
                      ctr.mat = ctr_spline, subset = grep(paste0(subset), names(coef(model)))) %>% 
    as.data.frame(.) %>% 
    mutate(score=X)
  
  return(hr_spline)
  
}


# --------------------------------------------------------------------------------------------
# List of models to fit during the ranking procedure
source("E:/PC_FIXE/Analysis/04_MLpaper/02_PLS/PLS_mathilde_2/0-0_model_lists.R")

# --------------------------------------------------------------------------------------------
# Function to bootstrap the difference in predictive gain for each variable
# included in the analyses, compared to the full model
# - data      : the data to use for resampling
# - indices   : resampling index
# - model_list: a list of model formulas to use for computing predictive gain (see above model_list.1 and model_list.fmm0)
# - max_FU    : for survival AUC computation; 9 for Whitehall, 4 for CoLaus
# Note: function to apply in the boot::boot() bootstrap function

bs.f <- function(data, indices, model_list, max_FU){
  
  data_b <- data[indices,]
  
  # Full cox model
  full_cox_b <- coxph(as.formula(paste0("Surv(pf21ac, ef21ac) ~", model_list[model_list$model=="full","formula"])), data = data_b)
  
  # > Uno's estimator of cumulative/dynamic AUC (for 9-year mortality in WII, 4-year mortality in CoLaus)
  full_auc_uno <- timeROC::timeROC(T         = data_b$pf21ac,
                                   delta     = data_b$ef21ac,
                                   marker    = predict(full_cox_b, type = "risk"),
                                   cause     = 1,
                                   weighting = "marginal",
                                   times     = max_FU) # Q3 follow-up = 9 in Whitehall, 4 in CoLaus
  
  # > Youden index estimate based on Uno's TP and FP
  full_youden_uno <- data.frame(sensitivity = full_auc_uno$TP[,2], 
                                specificity = 1 - full_auc_uno$FP[,2]) %>% 
    mutate(youden = sensitivity + specificity - 1) %>% 
    slice_max(youden)
  
  # > Incident/dynamic ROC analysis (Heagerty et Zheng) 
  Surv.rsp <- Surv(data_b$pf21ac, data_b$ef21ac)
  surv.prob <- unique(survfit(Surv.rsp~1)$surv)
  
  utimes <- unique( Surv.rsp[,"time"][ Surv.rsp[,"status"] == 1 ] )
  utimes <- utimes[ order(utimes) ]
  
  # > Compute AUC, sensitivity, and specificity at unique failure times
  full_AUC_id    <- rep( NA, length(utimes) )
  full_sensi_id  <- rep( NA, length(utimes) )
  full_speci_id  <- rep( NA, length(utimes) )
  full_youden_id <- rep( NA, length(utimes) )
  
  for( j in 1:length(utimes) )
  {
    # > ROC analysis
    out <- risksetROC::CoxWeights( marker = predict(full_cox_b), 
                                   Stime  = data_b$pf21ac, 
                                   status = data_b$ef21ac, 
                                   predict.time = utimes[j])
    
    # > AUC 
    full_AUC_id[j] <- out$AUC
    
    # > Youden index
    youden_j <- data.frame(sensitivity = out$TP, 
                           specificity = 1-out$FP) %>% 
      mutate(youden = sensitivity + specificity - 1) %>% 
      slice_max(youden, n = 1)
    
    # > Sensitivity and specificity
    full_sensi_id[j]  <- youden_j$sensitivity[1]
    full_speci_id[j]  <- youden_j$specificity[1]
    full_youden_id[j] <- youden_j$youden[1]
    
    youden_j <- NULL
  }
  
  # R², AIC, BIC, C-index, sensitivity, specificity, youden indices (C/D and I/D) of the full model fitted on the bth sample
  full_r2_b              <- royston(full_cox_b)["R.D"]
  full_aic_b             <- AIC(full_cox_b)
  full_bic_b             <- BIC(full_cox_b)
  full_c_b               <- full_cox_b$concordance['concordance']
  full_auc_b             <- full_auc_uno$AUC[paste0("t=",max_FU)]
  full_youden_cd_b       <- unique(full_youden_uno$youden)
  full_sensitivity_cd_b  <- unique(full_youden_uno$sensitivity)
  full_specificity_cd_b  <- unique(full_youden_uno$specificity)
  full_iAUC_b            <- risksetROC::IntegrateAUC( full_AUC_id, utimes, surv.prob, tmax=max_FU)
  full_youden_id_b       <- mean(full_youden_id)
  full_sensitivity_id_b  <- mean(full_sensi_id)
  full_specificity_id_b  <- mean(full_speci_id)
  
  # > results for the full model store 
  full_b <- c(full_r2_b, full_aic_b, full_bic_b, full_c_b, full_auc_b, full_youden_cd_b, full_sensitivity_cd_b, full_specificity_cd_b, full_iAUC_b, full_youden_id_b, full_sensitivity_id_b, full_specificity_id_b)
  
  # ----------
  # How predictive performance is decreased when excluding a variable/a group of variable, compared to the full model?
  # > objects to store the results for other models
  
  cox_r2             <- NULL
  cox_aic            <- NULL
  cox_bic            <- NULL
  cox_c              <- NULL
  cox_auc            <- NULL
  cox_youden_cd      <- NULL
  cox_sensitivity_cd <- NULL
  cox_specificity_cd <- NULL
  cox_iAUC           <- NULL
  cox_youden_id      <- NULL
  cox_sensitivity_id <- NULL
  cox_specificity_id <- NULL
  name               <- NULL
  
  # > fit models that exclude one variable or a group of variables
  for(i in 1:length(unique(model_list[model_list$model!="full","model"])))
  {
    
    # > select the model to fit 
    name_i    <- model_list$model[i+1]
    formula_i <- model_list$formula[i+1]
    
    # > fit the model excluding the variable (selected by index i) using data from the bth sample
    cox_i <- coxph(as.formula(paste0("Surv(pf21ac, ef21ac) ~ ", formula_i)), data = data_b)
    
    # > Uno's estimator of cumulative/dynamic AUC (for 9-year mortality in Whitehall, for 4-year mortality in CoLaus)
    auc_uno_i <- timeROC::timeROC(T         = data_b$pf21ac,
                                  delta     = data_b$ef21ac,
                                  marker    = predict(cox_i, type = "risk"),
                                  cause     = 1,
                                  weighting = "marginal",
                                  times     = max_FU) 
    
    # > Youden index estimate based on Uno's TP and FP
    youden_uno_i <- data.frame(sensitivity = auc_uno_i$TP[,2], 
                               specificity = 1 - auc_uno_i$FP[,2]) %>% 
      mutate(youden = sensitivity + specificity - 1) %>% 
      slice_max(youden,  n = 1)
    
    # > Incident/dynamic ROC analyses
    # > Compute AUC, sensitivity, and specificity at unique failure times
    AUC_id    <- rep( NA, length(utimes) )
    sensi_id  <- rep( NA, length(utimes) )
    speci_id  <- rep( NA, length(utimes) )
    youden_id <- rep( NA, length(utimes) )
    
    for( j in 1:length(utimes) )
    {
      out <- risksetROC::CoxWeights( marker       = predict(cox_i), 
                                     Stime        = data_b$pf21ac, 
                                     status       = data_b$ef21ac, 
                                     predict.time = utimes[j])
      # > AUC 
      AUC_id[j] <- out$AUC
      
      # > Youden index
      youden_j <- data.frame(sensitivity = out$TP, 
                             specificity = 1-out$FP) %>% 
        mutate(youden = sensitivity + specificity - 1) %>% 
        slice_max(youden, n = 1)
      
      # > Sensitivity and specificity
      sensi_id[j]  <- youden_j$sensitivity[1]
      speci_id[j]  <- youden_j$specificity[1]
      youden_id[j] <- youden_j$youden[1]
      
      youden_j <- NULL
      
    }
    
    # store the results for the model
    cox_r2             <- c(cox_r2,             royston(cox_i)["R.D"])
    cox_aic            <- c(cox_aic,            AIC(cox_i))
    cox_bic            <- c(cox_bic,            BIC(cox_i))
    cox_c              <- c(cox_c,              cox_i$concordance['concordance'])
    cox_auc            <- c(cox_auc,            auc_uno_i$AUC[paste0("t=",max_FU)])
    cox_youden_cd      <- c(cox_youden_cd,      unique(youden_uno_i$youden))
    cox_sensitivity_cd <- c(cox_sensitivity_cd, unique(youden_uno_i$sensitivity))
    cox_specificity_cd <- c(cox_specificity_cd, unique(youden_uno_i$specificity))
    cox_iAUC           <- c(cox_iAUC,           risksetROC::IntegrateAUC( AUC_id, utimes, surv.prob, tmax=max_FU))
    cox_youden_id      <- c(cox_youden_id,      mean(youden_id))
    cox_sensitivity_id <- c(cox_sensitivity_id, mean(sensi_id))
    cox_specificity_id <- c(cox_specificity_id, mean(speci_id))
    
    name <- c(name, name_i)
    
  }
  
  # > store results in a vector for boot function
  cox_b <- c(cox_r2, cox_aic, cox_bic, cox_c, cox_auc, cox_youden_cd, cox_sensitivity_cd, cox_specificity_cd, cox_iAUC, cox_youden_id, cox_sensitivity_id, cox_specificity_id)
  
  # > differences in predictive performance with the fully adjusted model
  d_r2             <- full_r2_b             - cox_r2
  d_aic            <- full_aic_b            - cox_aic
  d_bic            <- full_bic_b            - cox_bic
  d_c_index        <- full_c_b              - cox_c
  d_auc            <- full_auc_b            - cox_auc
  d_youden_cd      <- full_youden_cd_b      - cox_youden_cd
  d_sensitivity_cd <- full_sensitivity_cd_b - cox_sensitivity_cd
  d_specificity_cd <- full_specificity_cd_b - cox_specificity_cd
  d_iAUC           <- full_iAUC_b           - cox_iAUC
  d_youden_id      <- full_youden_id_b      - cox_youden_id
  d_sensitivity_id <- full_sensitivity_id_b - cox_sensitivity_id
  d_specificity_id <- full_specificity_id_b - cox_specificity_id
  
  # > store results in a vector for the boot function
  d_cox_b <- c(d_r2, d_aic, d_bic, d_c_index, d_auc, d_youden_cd, d_sensitivity_cd, d_specificity_cd, d_iAUC, d_youden_id, d_sensitivity_id, d_specificity_id)
  
  # > store results in a data frame
  Delta_tab_b <- data.frame(
    model            = name,
    d_r2             = d_r2,         
    d_aic            = d_aic,       
    d_bic            = d_bic,        
    d_c_index        = d_c_index,    
    d_auc            = d_auc,        
    d_youden_cd      = d_youden_cd,     
    d_sensitivity_cd = d_sensitivity_cd,
    d_specificity_cd = d_specificity_cd,
    d_iAUC           = d_iAUC,
    d_youden_id      = d_youden_id,     
    d_sensitivity_id = d_sensitivity_id,
    d_specificity_id = d_specificity_id) %>% 
    # > label group of variables and individual variables
    mutate(group=if_else(model %in% c("no_scd", "no_beh", "no_hea", "no_dim"), "group variables", "indivial variable"))
  
  
  # ----------
  # Comparing predictive performance of a variable/a group of variable, compared to other risk factors
  
  # > object to store the results
  D_var_b <- NULL
  
  # > for group of variables and individual variables separately
  for(g in unique(Delta_tab_b$group))
  {
    group_temp <- Delta_tab_b[Delta_tab_b$group==g,]
    
    # > for each group or individual variable v1
    for(v1 in unique(group_temp$model))
    { 
      
      temp_v1 <- group_temp[group_temp$model==v1,]          # data corresponding to the selected model
      temp_without_v1 <- group_temp[group_temp$model!=v1,]  # other models
      
      # > for each other groups or individuals variables v2
      for(v2 in unique(temp_without_v1$model))
      {
        
        temp_v2 <- temp_without_v1[temp_without_v1$model==v2,]  
        
        # > difference of differences between v1 and v2
        D_r2_b_v1v2             = temp_v1$d_r2             - temp_v2$d_r2
        D_aic_b_v1v2            = temp_v1$d_aic            - temp_v2$d_aic
        D_bic_b_v1v2            = temp_v1$d_bic            - temp_v2$d_bic
        D_c_b_v1v2              = temp_v1$d_c_index        - temp_v2$d_c_index
        D_auc_b_v1v2            = temp_v1$d_auc            - temp_v2$d_auc
        D_youden_cd_b_v1v2      = temp_v1$d_youden_cd      - temp_v2$d_youden_cd
        D_sensitivity_cd_b_v1v2 = temp_v1$d_sensitivity_cd - temp_v2$d_sensitivity_cd
        D_specificty_cd_b_v1v2  = temp_v1$d_specificity_cd - temp_v2$d_specificity_cd
        D_iAUC_b_v1v2           = temp_v1$d_iAUC           - temp_v2$d_iAUC
        D_youden_id_b_v1v2      = temp_v1$d_youden_cd      - temp_v2$d_youden_cd
        D_sensitivity_id_b_v1v2 = temp_v1$d_sensitivity_id - temp_v2$d_sensitivity_id
        D_specificty_id_b_v1v2  = temp_v1$d_specificity_id - temp_v2$d_specificity_id
        
        D_var_b <- rbind(D_var_b, data.frame(v1 = v1, v2 = v2, D_r2_b_v1v2, D_aic_b_v1v2, D_bic_b_v1v2, D_c_b_v1v2, 
                                             D_auc_b_v1v2, D_youden_cd_b_v1v2, D_sensitivity_cd_b_v1v2, D_specificty_cd_b_v1v2,
                                             D_iAUC_b_v1v2, D_youden_id_b_v1v2, D_sensitivity_id_b_v1v2, D_specificty_id_b_v1v2))
        
      }
      
    }
    
  }
  
  # > store results in a vector for boot function 
  D_cox_b <- c(D_var_b$D_r2_b_v1v2, 
               D_var_b$D_aic_b_v1v2, 
               D_var_b$D_bic_b_v1v2,  
               D_var_b$D_c_b_v1v2, 
               D_var_b$D_auc_b_v1v2, 
               D_var_b$D_youden_cd_b_v1v2, 
               D_var_b$D_sensitivity_cd_b_v1v2, 
               D_var_b$D_specificty_cd_b_v1v2, 
               D_var_b$D_iAUC_b_v1v2, 
               D_var_b$D_youden_id_b_v1v2, 
               D_var_b$D_sensitivity_id_b_v1v2, 
               D_var_b$D_specificty_id_b_v1v2)
  
  # > Results for the boot function
  res_b <- c(
    full_b,  # results of the full model
    cox_b,   # results of the models excluding each variable or group of variables
    d_cox_b, # difference between full and individual/group models
    D_cox_b) # difference between individual/group models
  
  return(res_b)
  
}

# --------------------------
# Names to attribute to bs.f() function outputs

names.bs.f <- function(names_ind = c("_r2_b", "_aic_b", "_bic_b", "_c_b", "_auc_b", "_youden_cd_b", "_sensitivity_cd_b", "_specificity_cd_b", "_iAUC_b", "_youden_id_b", "_sensitivity_id_b", "_specificity_id_b"), 
                       model_list,
                       group = TRUE,
                       restricted = FALSE)
{
  
  full_names <- NULL
  cox_names <- NULL 
  d_cox_names <- NULL 
  D_cox_names <- NULL
  
  full_names  <- paste(c("full"), names_ind, sep="")
  
  # > in the full sample and among those with at least 1 chronic disease ---
  cox_names   <- map(as.list(names_ind), ~ { paste(unique(model_list$model[!model_list$model=="full"]), .x, sep="") }) %>% unlist()
  d_cox_names <- paste("d_", cox_names, sep="")
  D_cox_names <- NULL
  
  # ------  
  # > for group of variables and individual variables separately
  if(group == TRUE)
  {
    
    # distinguish individual variables 
    individual_variables <- model_list %>% filter(!model %in% c("full", "no_scd", "no_beh", "no_hea", "no_dim")) %>% select(model)
    
    # distinguish group variables 
    group_variables <- model_list %>% filter(!model %in% c("full", unique(individual_variables$model))) %>% select(model)
    
    for(g in c("group variables", "indivial variable"))
    {
      
      # define which variable is a group or an individual variable
      ifelse(g == "group variables", 
             group_temp <- unique(group_variables$model), 
             group_temp <- unique(individual_variables$model))
      
      # pairs of variables in each category (group or individual)
      for(v1 in unique(group_temp))
      { 
        temp_v1 <- group_temp[group_temp==v1]  
        temp_without_v1 <- group_temp[group_temp!=v1] 
        
        for(v2 in unique(temp_without_v1))
        {
          temp_v2 <- temp_without_v1[temp_without_v1==v2]  
          D_cox_names <- c(D_cox_names, paste0(temp_v1, "_", temp_v2))  
        }
      }
    }
    
    D_cox_names <-  map(as.list(names_ind), ~ { paste(D_cox_names, .x, sep="") }) %>% unlist()
    
  }
  
  # > for group/individual variables only
  if(group == FALSE)
  {
    
    group_temp <- unique(model_list$model[!model_list$model=="full"])
    
    # > for each group or individual variable v1
    for(v1 in unique(group_temp))
    { 
      
      temp_v1 <- group_temp[group_temp==v1]  
      temp_without_v1 <- group_temp[group_temp!=v1] 
      
      # > for each other groups or individuals variables v2
      for(v2 in unique(temp_without_v1))
      {
        
        temp_v2 <- temp_without_v1[temp_without_v1==v2]  
        D_cox_names <- c(D_cox_names, paste0(temp_v1, "_", temp_v2))  
      }
    }
    D_cox_names <-  map(as.list(names_ind), ~ { paste(D_cox_names, .x, sep="") }) %>% unlist()
    
  }
    
  # ------  
  # > non restricted lists of models (list with groups and individuals variables)
  if(restricted == FALSE)
  {
    res<- c(full_names, cox_names, d_cox_names, D_cox_names)
  }
  
  # > restricted lists of variables (individual variables)
  if(restricted == TRUE)
  {
   res<-c(full_names, cox_names, d_cox_names)
  }
  return(res)
}

# -----------
# Function to compute 95% CI for bootstrapped estimates
# - x: a list of bootstrapped statistics, produced by implementing the bs.f() function (see above) in the boot::boot() function
# - w: the index of the bootstrapped vector for which computing 95%CI 

getCI <- function(x, w) {
  
  ci_bca <- coxed::bca(x$t[,w])
  tab_bca <- data.frame("index" = w, "method"="bca", "statistic" = x$t0[w], "conf.low" = ci_bca[1], "conf.high" = ci_bca[2])
  
  return(data.frame(tab_bca, row.names=NULL))
}  

