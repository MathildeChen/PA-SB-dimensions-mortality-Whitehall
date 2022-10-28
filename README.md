# Identification of physical activity and sedentary behaviour dimensions that predict mortality risk in older adults: development of a machine learning model in the Whitehall II accelerometer sub-study and external validation in the CoLaus study

This repository contains scripts supporting a project aiming to identify accelerometer-derived dimensions of movement behaviours that predict mortality risk in older populations. It 

These scripts were produced to: 
- derive accelerometer-derived features of physical activity (PA) and sedentary behaviour (SB) during waking period using GGIR package version 2.4-1 (https://github.com/wadpac/GGIR/releases/tag/2.4-1, @vincentvanhees) 
- dataset building in the development cohort (0-1_data_WII.R) and in the validation cohort (0-2_data_CoLaus.R)
- derive composite scores of PA and SB features that are relevant for predicting mortality among older adults; this include tunning of a machine learning algorithm using a 5-fold cross-validation procedure (1-0_sPLS_tuning_WII.R), deriving the score in the development cohort (1-1_sPLS_scores_WII.R), and in the validation cohort (1-2_sPLS_scores_CoLaus.R)
- examine association between the derived composite scores and mortality in the development cohort (3-1_associations_WII.R) and in the validation cohort (3-2_associations_CoLaus.R)
- assess the gain in performance to predict mortality of adding these scores to traditional risk factors using a bootstrap procedure (4-0_predictive_gain_bs.R and 4-1_predictive_gain_ci.R)
- conduct a sensitivity analysis (5_sensitivity_analysis_including_waking_period.R)
- produce associated tables and figures (6_tables_graphs_script.R) and supplements (7_supp_tables_graphs_script.R)

All scripts rely on other codes that contains various informations to produce the results (0_variables_names.R, 0-0_model_lists), functions created for the project (0-0_homemade_functions.R).
_________________________________________________
Authors
Mathilde Chen,*1 PhD; Benjamin Landré,*1 PhD; Pedro Marques-Vidal,2 MD, PhD; Vincent T. van Hees,3 PhD; April C.E. van Gennip,4,5 MD; Mikaela Bloomberg,6 PhD; Manasa S. Yerramalla,1 PhD; Mohamed Amine Benadjaoud,7 PhD; Séverine Sabia,1,6 PhD

1 Université Paris Cité, Inserm U1153, CRESS, Epidemiology of Ageing and Neurodegenerative diseases, 10 avenue de Verdun, 75010 Paris, France
2 Department of Medicine, Internal Medicine, Lausanne University Hospital and University of Lausanne, Switzerland
3 Accelting, Almere, The Netherlands
4 Department of Internal Medicine, Maastricht University Medical Centre, The Netherlands
5 School for Cardiovascular Diseases CARIM, Maastricht University, The Netherlands
6 Department of Epidemiology and Public Health, University College London, UK
7 Institute for Radiological Protection and Nuclear Safety (IRSN), Fontenay-Aux-Roses, France
*These authors contributed equally
