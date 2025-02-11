
# # Sensitivity - 2006-2008/2009-2016 ---------------------------------------
# # circulatory
# # try different er constraint
# analyze_dlnm_wfpm(ExpTerm = '0608_zip_daily_wfpm', dta = data4casecross_zip %>% filter(),
#                   CaseType = "count_circ_prmy_out", Sensitivity = "Sens",  
#                   ERConstraint = "lin", LRConstraint = "4dfevenknots",
#                   lag_number = 7)
# 
# # resp
# analyze_dlnm_wfpm(ExpTerm = '0608_zip_daily_wfpm', dta = data4casecross_zip,
#                   CaseType = "count_resp_prmy_out", Sensitivity = "Sens",  
#                   ERConstraint = "lin", LRConstraint = "5dfevenknots",
#                   lag_number = 7)
# 
# # anxiety; ER = 2 or linear if overparameterization; LR = 4
# analyze_dlnm_wfpm(ExpTerm = '0608_zip_daily_wfpm', dta = data4casecross_zip,
#                   CaseType = "count_anxty_prmy_out", Sensitivity = "Sens",  
#                   ERConstraint = "lin", LRConstraint = "5dfevenknots",
#                   lag_number = 7)
# 
# 
# # depression; ER = 2 or linear if overparameterization; LR = 4
# analyze_dlnm_wfpm(ExpTerm = '0608_zip_daily_wfpm', dta = data4casecross_zip,
#                   CaseType = "count_dep_prmy_out", Sensitivity = "Sens",  
#                   ERConstraint = "lin", LRConstraint = "5dfevenknots",
#                   lag_number = 7)
# 
# 

# ##### Sensitivity models - extend to lag 14 days -------------------------------------------------------------
# 
# # read in AIC for 14 lags circulatory and anxiety
# aic.table.circ <- read_csv(paste0(data_path, "output/tables/Model_AIC.csv")) %>% 
#   filter(LagName == "Lag14",
#          str_detect(ModelIdentifier, "circ"))
# 
# aic.table.anxty <- read_csv(paste0(data_path, "output/tables/Model_AIC.csv")) %>% 
#   filter(LagName == "Lag14",
#          str_detect(ModelIdentifier, "anxty"))
# 
# # circulatory - 14 lags
# analyze_dlnm_wfpm(ExpTerm = 'zip_daily_wfpm', dta = data4casecross_zip,
#                   CaseType = "count_circ_prmy_out", Sensitivity = "Sens_lag14",  
#                   ERConstraint = "linear", LRConstraint = "4dfevenknots",
#                   lag_number = 14)
# 
# # anxiety - 14 lags
# analyze_dlnm_wfpm(ExpTerm = 'zip_daily_wfpm', dta = data4casecross_zip,
#                   CaseType = "count_anxty_prmy_out", Sensitivity = "Sens_lag14",  
#                   ERConstraint = "linear", LRConstraint = "4dfevenknots",
#                   lag_number = 14)
# 
# # 
# ##### Sensitivity Models - include nonwfpm confounder -------------------------
# # count_all_cause_hosp_out; ER = 2 or linear; LR = 4
# start_time <- Sys.time()
# analyze_dlnm_wfpm(ExpTerm = 'zip_daily_wfpm', dta = data4casecross_zip,
#                   CaseType = "count_all_cause_hosp_out", Sensitivity = "nonwfpm",
#                   ERConstraint = "lin", LRConstraint = "3dfevenknots",
#                   lag_number = 7)
# 
# # circulatory
# analyze_dlnm_wfpm(ExpTerm = 'zip_daily_wfpm', dta = data4casecross_zip,
#                   CaseType = "count_circ_prmy_out", Sensitivity = "nonwfpm",
#                   ERConstraint = "lin", LRConstraint = "4dfevenknots",
#                   lag_number = 7)
# 
# # respiratory
# analyze_dlnm_wfpm(ExpTerm = 'zip_daily_wfpm', dta = data4casecross_zip,
#                   CaseType = "count_resp_prmy_out", Sensitivity = "nonwfpm",
#                   ERConstraint = "3dfevenknots", LRConstraint = "4dfevenknots",
#                   lag_number = 7)
# 
# # try different er constraint
# analyze_dlnm_wfpm(ExpTerm = 'zip_daily_wfpm', dta = data4casecross_zip,
#                   CaseType = "count_resp_prmy_out", Sensitivity = "nonwfpm",
#                   ERConstraint = "lin", LRConstraint = "4dfevenknots",
#                   lag_number = 7)
# 
# analyze_dlnm_wfpm(ExpTerm = 'zip_daily_wfpm', dta = data4casecross_zip,
#                   CaseType = "count_anxty_prmy_out", Sensitivity = "nonwfpm",
#                   ERConstraint = "lin", LRConstraint = "5dfevenknots",
#                   lag_number = 7)
# 
# 
# # depression; ER = 2 or linear if overparameterization; LR = 4
# analyze_dlnm_wfpm(ExpTerm = 'zip_daily_wfpm', dta = data4casecross_zip,
#                   CaseType = "count_dep_prmy_out", Sensitivity = "nonwfpm",
#                   ERConstraint = "lin", LRConstraint = "5dfevenknots",
#                   lag_number = 7)

# end_time <- Sys.time()
# end_time - start_time

# Stratify by wfpm clusters - include all controls regardless of membership -----------------------------------------------
# analyze_dlnm_wfpm_stratification(ExpTerm = 'all_cntrl_ind_daily_wfpm', dta = data4casecross_ind,
#                                  CaseType = "circ_prmy_out", Stratify_Variable = "case_cluster_name",  
#                                  ERConstraint = "3dfevenknots", LRConstraint = "4dfevenknots")


analyze_dlnm_wfpm_stratification(ExpTerm = 'all_cntrl_ind_daily_wfpm', dta = data4casecross_ind,
                                 CaseType = "circ_prmy_out", Stratify_Variable = "case_cluster_name",  
                                 ERConstraint = "linear", LRConstraint = "4dfevenknots")

# analyze_dlnm_wfpm_stratification(ExpTerm = 'all_cntrl_ind_daily_wfpm', dta = data4casecross_ind,
#                                  CaseType = "resp_prmy_out", Stratify_Variable = "case_cluster_name",  
#                                  ERConstraint = "3dfevenknots", LRConstraint = "5dfevenknots")

analyze_dlnm_wfpm_stratification(ExpTerm = 'all_cntrl_ind_daily_wfpm', dta = data4casecross_ind,
                                 CaseType = "resp_prmy_out", Stratify_Variable = "case_cluster_name",  
                                 ERConstraint = "linear", LRConstraint = "5dfevenknots")

# analyze_dlnm_wfpm_stratification(ExpTerm = 'all_cntrl_ind_daily_wfpm', dta = data4casecross_ind,
#                                  CaseType = "anxty_prmy_out", Stratify_Variable = "case_cluster_name",  
#                                  ERConstraint = "2dfevenknots", LRConstraint = "5dfevenknots")

analyze_dlnm_wfpm_stratification(ExpTerm = 'all_cntrl_ind_daily_wfpm', dta = data4casecross_ind,
                                 CaseType = "anxty_prmy_out", Stratify_Variable = "case_cluster_name",  
                                 ERConstraint = "linear", LRConstraint = "5dfevenknots")

analyze_dlnm_wfpm_stratification(ExpTerm = 'all_cntrl_ind_daily_wfpm', dta = data4casecross_ind,
                                 CaseType = "dep_prmy_out", Stratify_Variable = "case_cluster_name",  
                                 ERConstraint = "linear", LRConstraint = "5dfevenknots")

# Stratify by wfpm clusters - include only controls with same case day membership -----------------------------------------------
# analyze_dlnm_wfpm_stratification(ExpTerm = 'same_cntrl_ind_daily_wfpm', dta = data4casecross_ind,
#                                  CaseType = "circ_prmy_out", Stratify_Variable = "cluster_name",  
#                                  ERConstraint = "3dfevenknots", LRConstraint = "4dfevenknots")


analyze_dlnm_wfpm_stratification(ExpTerm = 'same_cntrl_ind_daily_wfpm', dta = data4casecross_ind,
                                 CaseType = "circ_prmy_out", Stratify_Variable = "cluster_name",  
                                 ERConstraint = "linear", LRConstraint = "4dfevenknots")

# analyze_dlnm_wfpm_stratification(ExpTerm = 'same_cntrl_ind_daily_wfpm', dta = data4casecross_ind,
#                                  CaseType = "resp_prmy_out", Stratify_Variable = "cluster_name",  
#                                  ERConstraint = "3dfevenknots", LRConstraint = "5dfevenknots")

analyze_dlnm_wfpm_stratification(ExpTerm = 'same_cntrl_ind_daily_wfpm', dta = data4casecross_ind,
                                 CaseType = "resp_prmy_out", Stratify_Variable = "cluster_name",  
                                 ERConstraint = "linear", LRConstraint = "5dfevenknots")

# analyze_dlnm_wfpm_stratification(ExpTerm = 'same_cntrl_ind_daily_wfpm', dta = data4casecross_ind,
#                                  CaseType = "anxty_prmy_out", Stratify_Variable = "cluster_name",  
#                                  ERConstraint = "2dfevenknots", LRConstraint = "5dfevenknots")

analyze_dlnm_wfpm_stratification(ExpTerm = 'same_cntrl_ind_daily_wfpm', dta = data4casecross_ind,
                                 CaseType = "anxty_prmy_out", Stratify_Variable = "cluster_name",  
                                 ERConstraint = "linear", LRConstraint = "5dfevenknots")

analyze_dlnm_wfpm_stratification(ExpTerm = 'same_cntrl_ind_daily_wfpm', dta = data4casecross_ind,
                                 CaseType = "dep_prmy_out", Stratify_Variable = "cluster_name",  
                                 ERConstraint = "linear", LRConstraint = "5dfevenknots")

# Stratify by wfpm clusters CALIFORNIA -----------------------------------------------
data4casecross_ind_ca <- data4casecross_ind %>% 
  filter(state == "CA")
table(data4casecross_ind_ca$cluster_name)

analyze_dlnm_wfpm_stratification(ExpTerm = 'ca_ind_daily_wfpm', dta = data4casecross_ind_ca,
                                 CaseType = "circ_prmy_out", Stratify_Variable = "case_cluster_name",  
                                 ERConstraint = "3dfevenknots", LRConstraint = "4dfevenknots")

analyze_dlnm_wfpm_stratification(ExpTerm = 'ca_ind_daily_wfpm', dta = data4casecross_ind_ca,
                                 CaseType = "circ_prmy_out", Stratify_Variable = "case_cluster_name",  
                                 ERConstraint = "linear", LRConstraint = "4dfevenknots")

analyze_dlnm_wfpm_stratification(ExpTerm = 'ca_ind_daily_wfpm', dta = data4casecross_ind_ca,
                                 CaseType = "resp_prmy_out", Stratify_Variable = "case_cluster_name",  
                                 ERConstraint = "3dfevenknots", LRConstraint = "5dfevenknots")

analyze_dlnm_wfpm_stratification(ExpTerm = 'ca_ind_daily_wfpm', dta = data4casecross_ind_ca,
                                 CaseType = "resp_prmy_out", Stratify_Variable = "case_cluster_name",  
                                 ERConstraint = "linear", LRConstraint = "5dfevenknots")

analyze_dlnm_wfpm_stratification(ExpTerm = 'ca_ind_daily_wfpm', dta = data4casecross_ind_ca,
                                 CaseType = "anxty_prmy_out", Stratify_Variable = "case_cluster_name",  
                                 ERConstraint = "2dfevenknots", LRConstraint = "5dfevenknots")

analyze_dlnm_wfpm_stratification(ExpTerm = 'ca_ind_daily_wfpm', dta = data4casecross_ind_ca,
                                 CaseType = "anxty_prmy_out", Stratify_Variable = "case_cluster_name",  
                                 ERConstraint = "linear", LRConstraint = "5dfevenknots")

analyze_dlnm_wfpm_stratification(ExpTerm = 'ca_ind_daily_wfpm', dta = data4casecross_ind_ca,
                                 CaseType = "dep_prmy_out", Stratify_Variable = "case_cluster_name",  
                                 ERConstraint = "linear", LRConstraint = "5dfevenknots")

