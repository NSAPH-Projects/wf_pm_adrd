# run sequential codes

# gather study population - last ran 9/15 using Yoon (updated ICD codes - i should be a pro now)
source(here::here("1_gather_study_pop", "1_id_first_adrd_hosp.R"))
source(here::here("1_gather_study_pop", "2_gen_hosp_dta.R")) # last run 12/12 - included diag1-diag3; remove all_cause_hosp

# wrangle exposure outcome confounder vars - last ran 12/12
source(here::here("2_wrangle_exp_conf_vars", "1_create_control_dates.R"))
source(here::here("2_wrangle_exp_conf_vars", "2_assign_lag_exp_wfpm.R"))
source(here::here("2_wrangle_exp_conf_vars", "3_assign_lag_temp_rh.R"))
source(here::here("2_wrangle_exp_conf_vars", "4_assign_lag_nonwfpm.R")) #not run on 12/12
source(here::here("2_wrangle_exp_conf_vars", "5_assign_lag_totpm.R")) # (not run on 9/6) this is for sensitivity analyses (not used in 1_data4casecross_zip)

# combine datasets for data4casecross - last ran 11/5
source(here::here("3_combine_data4casecross", "1_data4casecross_zip.R"))
source(here::here("3_combine_data4casecross", "2_data4casecross_ind.R"))

# analyses
source(here::here("4_analyses", "0a_gen_sample_dta.R"))
source(here::here("4_analyses", "0b_gridsearch.R"))
source(here::here("4_analyses", "1_run_dlnm_case_crossover.R"))
source(here::here("4_analyses", "2_run_dlnm_case_crossover_ind_dta.R")) # need to run 10/7

# produce plots








