# Generate a table 1 for study
# Vivian Do
# 7/19/23 

####********************
#### 0: Preparation #### 
####********************

# 0a Declare root directory
project.folder <- paste0(print(here::here()),'/')

# 0b Load Packages
source(here::here("0_set_up", "1_libraries.R"))

# 0c Set up filepath(s)
data_path <- '/n/dominici_nsaph_l3/Lab/projects/wfpm_adrd_hosp/data/'
data_path_data4casecross <- "/n/dominici_nsaph_l3/Lab/projects/wfpm_adrd_hosp/data/data_process/data4casecross/"
data_path_stratification_vars <- '/n/dominici_nsaph_l3/Lab/projects/wfpm_adrd_hosp/data/data_process/stratification_vars/'
output_path <- paste0(data_path, "output")
outpath_table1 <- "/n/dominici_nsaph_l3/Lab/projects/wfpm_adrd_hosp/results/table_1/"

strat_pov_urbanicity <- read_csv(paste0(data_path_stratification_vars, "pov_ruca_zip.csv")) %>% 
  mutate(urbanicity = ifelse(RUCA1 == 10, "rural", "urban/suburban")) %>% 
  select(-c(GEOID10, STATE, ZIP_TYPE, RUCA1, RUCA2)) %>% 
  select(zip, everything()) %>% 
  mutate(zip = as.integer(zip))

# how many people in our study 
# get the zip-year combos with full data - count folks if there is all data for this combo

file_names <- list.files(paste0(data_path, "adrd_hospitalization"), full.names = TRUE)[7:17]
yearly_hosp_zip <- lapply(file_names, read_fst)

# # Define ICD 9/10 codes ---------------------------------------------------
# # circulatory (but not cardiovascular)
# icd_circ <- c((390:459), #icd 9
#               paste0("I", "0", 0:9), #icd 10
#               paste0("I", 10:99)) #icd 10
# 
# # respiratory
# icd_resp <- c((460:519), #icd 9
#               paste0("J", "0", 0:9), #icd 10
#               paste0("J", 10:99)) #icd 10
# 
# # as of 7/3/23, we have included H.E.'s suggestions for ICD codes related to mental health
# # anxiety; if 2+ visits on different days have these codes
# icd_anxty <- c(300.00, 300.01, 300.02, #icd 9
#                
#                "F40.00", "F40.01", "F40.02", "F40.10", #icd 10
#                "F40.11", "F40.9", "F41.0", "F41.1",
#                "F41.9", "F44.4", 'F44.5', "F44.6", "F44.7",
#                "F45.20", "F45.21", "F45.22", "F45.29")
# 
# # depression; if 2+ visits on different days have these codes
# icd_dep <- c(296.20, 296.21, 296.22, 296.23,
#              296.25, 296.26, 296.30, 296.31,
#              296.32, 296.33, 296.34, 296.35,
#              296.36, 298.0, 300.4, 309, 309.1, 311, 
#              
#              "F32.3", "F33.3", "F32.0", "F32.1", "F32.2",
#              "F32.4", "F32.5", "F32.9", "F33.0", "F33.1",
#              "F33.2", "F33.40", "F33.41", "F33.42",
#              "F33.9", "F32.89", "F33.8", "F43.21", "F32.A")
# 
# # identify diagnosis variables in mdcr data
# diag_vars <- c(paste0("DIAG", 1:10))


# Create functions ---------------------------

#NOTE: THE ADRD DATA YEAR IN THE FILE NAME REFERS TO DISCHARGE DATE!!!! (NOT ADMISSION DATE)
# for example, the adrd file ending in 2008 can contain admissions from 2006, 2007, 2008
filenames_and_adm_year <- data.frame(filename_year = numeric(0), adm_year = numeric(0))
for (year in seq_along(yearly_hosp_zip)){
  print(paste0("ADRD file ending in ", unique(year(yearly_hosp_zip[[year]]$DDATE)),
               " contains the following admission year: ", unique(year(yearly_hosp_zip[[year]]$ADATE))))
  
  filenames_and_adm_year <- rbind(filenames_and_adm_year, data.frame(filename_year = unique(year(yearly_hosp_zip[[year]]$DDATE)),
                                                                     adm_year = unique(year(yearly_hosp_zip[[year]]$ADATE))))
}

# function to generate individual observations per year by admission date rather than filename year (reads in a dataset)
# also restrict age to <= 115 years
gen_ind_obs_yrly_data_by_adm <- function(year_of_interest){
  
  read_in_years <- filenames_and_adm_year %>% 
    filter(filename_year == year_of_interest) %>% 
    select(adm_year)
  
  list_read_in_years <- as.list(read_in_years$adm_year)
  
  relevant_files <- data.frame()
  for (read_in in 1:length(list_read_in_years)){
  print(paste0("Reading in file year: ", list_read_in_years[read_in]))
  file <- read_fst(paste0("/n/dominici_nsaph_l3/Lab/projects/wfpm_adrd_hosp/data/adrd_hospitalization/ADRD_", list_read_in_years[read_in], ".fst"))
  relevant_files <- rbind(relevant_files, file)
  }

  relevant_files <- relevant_files %>%
    filter(year(ADATE) == year_of_interest,
           AGE <= 115)

  return(relevant_files)

}

# add stratification variables, poverty, med hh inc, and urbanicity
add_strat_vars <- function(dta_gen_ind_obs_yrly_data_by_adm){
  
  strat_pov_urbanicity <- strat_pov_urbanicity %>% 
    rename(zipcode_R = zip)
  output_data <- dta_gen_ind_obs_yrly_data_by_adm %>% 
    left_join(., strat_pov_urbanicity,
              by = ("zipcode_R"))
  
}

stack_combo_zip_year <- read_fst("/n/dominici_nsaph_l3/Lab/projects/wfpm_adrd_hosp/data/data_process/zip_days_w_conf_exp.fst")

# keep variables with confounder and exposure data; this information is taken from our zip level data4casecross
keep_vars_w_all_data <- function(dta_add_strat_vars){
  output_data <- dta_add_strat_vars %>% 
    left_join(., 
              stack_combo_zip_year %>% 
                mutate(has_conf_exp_dta = 1),
              by = c("zipcode_R" = "zip",
                     "ADATE" = "DayDateTime")) %>% 
    filter(has_conf_exp_dta == 1) %>% 
    select(-has_conf_exp_dta) %>% 
    mutate(pct_pov_final = ifelse(year < 2012, pct_pov_0711, pct_pov_1216),
           med_hh_inc_final = ifelse(year < 2012, med_hh_inc_0711, med_hh_inc_1216))
  
}

assign_hosp_outcome <- function(hosp_w_icd){
  output_data <- hosp_w_icd %>% 
    filter(nchar(as.character(zipcode_R)) == 5,
           zipcode_R != 99999,
           year(ADATE) >= 2006) %>% 
    mutate(circ_any = as.numeric(if_any(diag_vars, ~. %in% icd_circ)),
           circ_prmy = ifelse(DIAG1 %in% icd_circ, 1, 0),
           resp_any = as.numeric(if_any(diag_vars, ~. %in% icd_resp)),
           resp_prmy = ifelse(DIAG1 %in% icd_resp, 1, 0),
           anxty_any = as.numeric(if_any(diag_vars, ~. %in% icd_anxty)),
           anxty_prmy = ifelse(DIAG1 %in% icd_anxty, 1, 0),
           dep_any = as.numeric(if_any(diag_vars, ~. %in% icd_dep)),
           dep_prmy = ifelse(DIAG1 %in% icd_dep, 1, 0))
  
  output_data
}


# Generate table 1 for each year ------------------------------------------
for (year in 2006:2016){
  print(paste0("Running loop for admission year: ", year))
  
  # year <- 2006
  
  # prepare data by 
  # (1) ensuring that we are capturing all admission years across different file years
  # (2) adding stratification variables for table 1
  # (3) keeping observations that we use for our study (study population)
  yrly_dta <- gen_ind_obs_yrly_data_by_adm(year)
  yrly_dta <- add_strat_vars(yrly_dta)
  yrly_dta <- keep_vars_w_all_data(yrly_dta)
  
  yrly_dta_w_diag_codes <- assign_hosp_outcome(yrly_dta) %>%
    mutate(year = year(ADATE),
           month = month(ADATE))
  
  ##### Individual characteristics by outcome(s) -----
  tab1_ind_char_ADRD_any <- as.data.frame(table1(~ AGE + Sex_gp + pct_pov_final + med_hh_inc_final + urbanicity | as.factor(ADRD_any), data = yrly_dta_w_diag_codes)) %>% 
    write_csv(., paste0(outpath_table1, "yrly_table1/tab1_", year, "_ADRD_any.csv"))
  
  tab1_ind_char_AD_any <- as.data.frame(table1(~ AGE + Sex_gp + pct_pov_final + med_hh_inc_final + urbanicity | as.factor(AD_any), data = yrly_dta_w_diag_codes)) %>% 
    write_csv(., paste0(outpath_table1, "yrly_table1/tab1_", year, "_AD_any.csv"))
  
  tab1_ind_char_circ_any <- as.data.frame(table1(~ AGE + Sex_gp + pct_pov_final + med_hh_inc_final + urbanicity | as.factor(circ_any), data = yrly_dta_w_diag_codes)) %>% 
    write_csv(., paste0(outpath_table1, "yrly_table1/tab1_", year, "_circ_any.csv"))
  
  tab1_ind_char_resp_any <- as.data.frame(table1(~ AGE + Sex_gp + pct_pov_final + med_hh_inc_final + urbanicity | as.factor(resp_any), data = yrly_dta_w_diag_codes)) %>% 
    write_csv(., paste0(outpath_table1, "yrly_table1/tab1_", year, "_resp_any.csv"))
  
  tab1_ind_char_anxty_any <- as.data.frame(table1(~ AGE + Sex_gp + pct_pov_final + med_hh_inc_final + urbanicity | as.factor(anxty_any), data = yrly_dta_w_diag_codes)) %>% 
    write_csv(., paste0(outpath_table1, "yrly_table1/tab1_", year, "_anxty_any.csv"))
  
  tab1_ind_char_dep_any <- as.data.frame(table1(~ AGE + Sex_gp + pct_pov_final + med_hh_inc_final + urbanicity | as.factor(dep_any), data = yrly_dta_w_diag_codes)) %>% 
    write_csv(., paste0(outpath_table1, "yrly_table1/tab1_", year, "_dep_any.csv"))
  
  #### Outcomes by month -----
  tab1_mnth_ADRD_any <- as.data.frame(table1(~ as.factor(month) | as.factor(ADRD_any), data = yrly_dta_w_diag_codes)) %>% 
    write_csv(., paste0(outpath_table1, "yrly_table1/tab1_", year, "_ADRD_any_mnth.csv"))
  
  tab1_mnth_AD_any <- as.data.frame(table1(~ as.factor(month) | as.factor(AD_any), data = yrly_dta_w_diag_codes)) %>% 
    write_csv(., paste0(outpath_table1, "yrly_table1/tab1_", year, "_AD_any_mnth.csv"))
  
  tab1_mnth_circ_any <- as.data.frame(table1(~ as.factor(month) | as.factor(circ_any), data = yrly_dta_w_diag_codes)) %>% 
    write_csv(., paste0(outpath_table1, "yrly_table1/tab1_", year, "_circ_any_mnth.csv"))
  
  tab1_mnth_resp_any <- as.data.frame(table1(~ as.factor(month) | as.factor(resp_any), data = yrly_dta_w_diag_codes)) %>% 
    write_csv(., paste0(outpath_table1, "yrly_table1/tab1_", year, "_resp_any_mnth.csv"))
  
  tab1_mnth_anxty_any <- as.data.frame(table1(~ as.factor(month) | as.factor(anxty_any), data = yrly_dta_w_diag_codes)) %>% 
    write_csv(., paste0(outpath_table1, "yrly_table1/tab1_", year, "_anxty_any_mnth.csv"))
  
  tab1_mnth_dep_any <- as.data.frame(table1(~ as.factor(month) | as.factor(dep_any), data = yrly_dta_w_diag_codes)) %>% 
    write_csv(., paste0(outpath_table1, "yrly_table1/tab1_", year, "_dep_any_mnth.csv"))
}


# Generate table 1 for all data across years (individuals) ------------------------------------------
tab1_for_all_data <- list()
year_list <- c(2006:2016)
for (year in 1:length(year_list)){
  print(paste0("Running loop for year: ", year_list[year]))
  
  # year <- 2006
  
  # prepare data by 
  # (1) ensuring that we are excluding 2005
  # (2) adding stratification variables for table 1
  # (3) keeping observations that we use for our study (study population)
  yrly_dta <- read_fst(paste0("/n/dominici_nsaph_l3/Lab/projects/wfpm_adrd_hosp/data/adrd_hospitalization/ADRD_", year_list[year], ".fst")) %>% 
    filter(year >= 2006)
  yrly_dta <- add_strat_vars(yrly_dta)
  yrly_dta <- keep_vars_w_all_data(yrly_dta)
  
  yrly_dta_w_diag_codes <- assign_hosp_outcome(yrly_dta) %>%
    mutate(year = year(ADATE),
           month = month(ADATE))
  
  tab1_for_all_data[[year]] <- yrly_dta_w_diag_codes
}

all_data_hosp <- do.call(rbind, tab1_for_all_data) %>% 
  mutate(across(matches("_any"), as.factor))

all_data_ind <- all_data_hosp %>% 
  group_by(QID) %>% 
  filter(row_number() == 1)

# Characteristics of hospitalizations (includes repeated patient hosp) ---------------------------------------
# 3409734 hospitalizations (contains re-hospitalization among individuals)
as.data.frame(table1(~ AGE + Sex_gp + pct_pov_final + med_hh_inc_final + urbanicity, data = all_data_hosp)) %>% 
  write_csv(., paste0(outpath_table1, "all_years_table1/tab1_demographics_hosp.csv"))
as.data.frame(table1(~ ADRD_any + AD_any + circ_any + resp_any + anxty_any + dep_any, data = all_data_hosp)) %>% 
  write_csv(., paste0(outpath_table1, "all_years_table1/tab1_outcome_hosp.csv"))
as.data.frame(table1(~ as.factor(month), data = all_data_hosp)) %>% 
  write_csv(., paste0(outpath_table1, "all_years_table1/tab1_alloutcome_by_month_hosp.csv"))




# Characteristics of distinct individuals (a patient may have many hospitalizations) -----------------------
# 1721745 distinct patients
as.data.frame(table1(~ AGE + Sex_gp + pct_pov_final + med_hh_inc_final + urbanicity, data = all_data_ind)) %>% 
  write_csv(., paste0(outpath_table1, "all_years_table1/tab1_demographics_ind.csv"))

