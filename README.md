# Identification of physical activity and sedentary behaviour dimensions that predict mortality risk in older adults: development of a machine learning model in the Whitehall II accelerometer sub-study and external validation in the CoLaus study

This repository contains scripts supporting a project aiming to identify accelerometer-derived dimensions of movement behaviours that predict mortality risk in older populations. 

All analyses were undertaken using R version 4.1.2 (http://www.r-project.org), analyses required the specific packages: 
- *GGIR* for accelerometer data processing (version 2.3-3, https://cran.r-project.org/web/packages/GGIR/vignettes/GGIR.html)
- *plsRcox* for sparse Partial Least Square regression (version 1.7.6, https://cran.r-project.org/web/packages/plsRcox/index.html)
- *Epi* (version 2.47, https://cran.r-project.org/web/packages/Epi/index.html), *aod* (version 1.3.2, https://cran.r-project.org/web/packages/aod/index.html), and *rms* (version 6.3-0, https://cran.r-project.org/web/packages/rms/index.html) for examine the association bewtween movement behaviour compopsite score and mortality
- *timeROC* for time-Dependent ROC Curve(version 0.4, https://cran.r-project.org/web/packages/timeROC/index.html)
- *boot* for performing bootstrap analyses (version 1.3-28, https://cran.r-project.org/web/packages/boot/index.html) 
- *coxed* for computing 95% bias-corrected and accelerated confidence intervals for bootstrapped estimates (version 0.3.3, https://cran.r-project.org/web/packages/coxed/index.html)

Here is a schema of the workflow: 

![image](https://user-images.githubusercontent.com/42891458/207081115-2d6d7f18-9f1e-426c-b652-44173fa017e5.png)

More details on each steps are provided in the following sections:

# Step 0 - Accelerometer data processing 
Syntax used in GGIR is provided (0-0_GGIR_extraction.R).

# Step 1 - Building datasets
Datasets in the development cohort (0-1_data_WII.R) and in the validation cohort (0-2_data_CoLaus.R). Data should include: 
- 21 accelerometer-derived features entered into further analyses as z-scores. For replication in another cohort, standardised values of physical activity (PA) and sedentary behaviour (SB) features based on means and standard deviations from the derivation study sample as presented in the attached table.
- traditional risk factors for mortality including: sociodemographic, behavioural, and health-related factors 
- outcome variables (time of follow-up and vital status at the end of the follow-up)

# Step 2 - Sparse Partial Least Square regression  
Derive composite scores of PA and SB features that are relevant for predicting mortality among older adults; this include tunning of a machine learning algorithm using a 5-fold cross-validation procedure (1-0_sPLS_tuning_WII.R), deriving the score in the development cohort (1-1_sPLS_scores_WII.R), and in the validation cohort (1-2_sPLS_scores_CoLaus.R).

# Step 3 - Association between the derived composite scores and mortality 
Association examined in the development cohort (3-1_associations_WII.R) and in the validation cohort (3-2_associations_CoLaus.R). Composite risk score was entered as a linear term or as a restricted cubic spline to examine linearity of the association. 

# Step 4 - Predictive added value of the composite score 
We assess the gain in performance to predict mortality of adding derived composite scores to traditional risk factors using a bootstrap procedure (4-0_predictive_gain_bs.R and 4-1_predictive_gain_ci.R)

# Step 5 - Sensitivity analyses
A sensitivity analysis was conducted to examine the potential impact of the duration of the waking period on our findings by repeating the analysis using PA and SB features standardized to the duration of the waking period before inclusion in the selection procedure (5_sensitivity_analysis_including_waking_period.R)

# Step 6 - Tables & Figures 
Scripts to produce associated tables and figures (6_tables_graphs_script.R) and supplements (7_supp_tables_graphs_script.R) are provided. 

All steps rely on other scripts containing various informations to produce the results (0_variables_names.R, 0-0_model_lists), functions created for the project (0-0_homemade_functions.R).

Appendix: 
[Procedure to compute composite risk score in a replication study.pdf](https://github.com/MathildeChen/PA-SB-dimensions-mortality-Whitehall/files/10209407/Procedure.to.compute.composite.risk.score.in.a.replication.study.pdf)

DOI: 10.5281/zenodo.7428923
_________________________________________________
Authors
Mathilde Chen,¤<sup>1</sup> PhD; Benjamin Landré,¤<sup>1</sup> PhD; Pedro Marques-Vidal,<sup>2</sup> MD, PhD; Vincent T. van Hees,<sup>3</sup> PhD; April C.E. van Gennip,<sup>4,5</sup> MD; Mikaela Bloomberg,<sup>6</sup> PhD; Manasa S. Yerramalla,<sup>1</sup> PhD; Mohamed Amine Benadjaoud,<sup>7</sup> PhD; Séverine Sabia,<sup>1,6</sup> PhD

<sup>1</sup> Université Paris Cité, Inserm U1153, CRESS, Epidemiology of Ageing and Neurodegenerative diseases, 10 avenue de Verdun, 75010 Paris, France

<sup>2</sup> Department of Medicine, Internal Medicine, Lausanne University Hospital and University of Lausanne, Switzerland

<sup>3</sup> Accelting, Almere, The Netherlands

<sup>4</sup> Department of Internal Medicine, Maastricht University Medical Centre, The Netherlands

<sup>5</sup> School for Cardiovascular Diseases CARIM, Maastricht University, The Netherlands

<sup>6</sup> Department of Epidemiology and Public Health, University College London, UK

<sup>7</sup> Institute for Radiological Protection and Nuclear Safety (IRSN), Fontenay-Aux-Roses, France

¤ These authors contributed equally


