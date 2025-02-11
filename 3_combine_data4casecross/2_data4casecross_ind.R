####***********************
#### Code Description ####
# Author: Vivian 
# Date: 8/8/23
# Goal: combine exposure, outcome, confounder data for individual codes
# should also contain: urbanicity, age, demographics, etc
####**********************
rm(list=ls())
source(here::here("0_set_up", "1_libraries.R"))
data_path <- '/n/dominici_nsaph_l3/Lab/projects/wfpm_adrd_hosp/data'
data_path_data4casecross <- paste0(data_path, '/data_process/data4casecross')
data_path_stratification_vars <- paste0(data_path, '/data_process/stratification_vars')

# Read in data ------------------------------------------------------------
#* this ind_hosp_outcomes data contains all years
#* only need to load this once while loop goes through all years
ind_hosp_outcomes <- read_rds(paste0(data_path, "/data_process/hosp_dta/hosp_after_first_adrd_ind.rds")) %>% 
  rename(zip = zipcode_R,
         CaseDateTime = adm_date_hosp) %>% 
  mutate(id = row_number()) #because this is individual level data, we want the id to be unique to each individual

#* this file contains zip info for stratification; urbanicity and poverty
strat_pov_urbanicity <- read_csv(paste0(data_path_stratification_vars, "/pov_ruca_zip.csv")) %>%
  mutate(urbanicity = ifelse(RUCA1 %in% c(4:10), "rural", "urban")) %>% 
  select(-c(GEOID10, ZIP_TYPE, RUCA1, RUCA2)) %>% 
  select(zip, everything()) %>% 
  mutate(zip = as.integer(zip)) %>% 
  rename(state = STATE)

# 0d Load datas for case-crossover analyses
# year <- 2006
gen_data4casecross_year <- function(year){
  data_w_lagged_wfpm <- read_fst(paste0(data_path_data4casecross, "/data_w_lagged_wfpm/data4casecross_", year, ".fst")) %>% 
    select(dayName, CaseDateTime, DayDateTime, starts_with(c("zip", "wfpm")))
  
  data_w_lagged_temp_rh <- read_fst(paste0(data_path_data4casecross, "/data_w_lagged_temp_rh/data4casecross_", year, ".fst")) %>% 
    select(DayDateTime, contains(c("zip", "temp", "rh"))) 
  
  hosp_dta <- left_join(data_w_lagged_wfpm, data_w_lagged_temp_rh, by = c("zip", "DayDateTime")) %>% 
    left_join(., ind_hosp_outcomes, by = c("zip",
                                           "CaseDateTime" = "CaseDateTime")) %>% # merge by CaseDateTime (bc we want the weights to be the daily case numbers)
    select(DayDateTime, id, dayName, zip, starts_with("count"), everything()) %>% 
    # replace the missing outcome variables with 0 because there are no hospitalizations for those days
    mutate(across(contains("count"), ~replace(., is.na(.), 0))) 
  
}

keep_data_w_all_wfpm <- function(data4casecross_year){
  
  # keep only id sets with full exposure data by excluding ids with missing exposure data
  # keep if first 7 days (0-6) are not missing bc this is our primary analysis
  ids_w_missing_wfpm <- data4casecross_year %>% 
    select(DayDateTime, id, zip, starts_with(c("wfpm"))) %>% 
    select(-ends_with(c("7", "8", "9", "10", "11", "12", "13")))  
  ids_w_missing_wfpm$number_na <- rowSums(is.na(ids_w_missing_wfpm)) 
  ids_w_missing_wfpm <- ids_w_missing_wfpm %>% 
    group_by(id) %>% 
    mutate(ind_missing = ifelse(sum(number_na) >= 1, 1, 0)) %>% 
    filter(row_number() == 1,
           ind_missing == 1) %>% 
    select(id)
  
  data4casecross_year <- data4casecross_year %>% 
    filter(!id %in% ids_w_missing_wfpm$id)
}


# Run functions to get one dataset for all dta at zip-daily level ---------
years <- c(2006:2016)
data4casecross_ind <- list()
for (year in 1:length(years)){
  
  print(years[[year]])
  data_year <- gen_data4casecross_year(years[[year]])
  data_year_no_missing_conf_exp <- keep_data_w_all_wfpm(data_year)

  data4casecross_ind[[year]] <- data_year_no_missing_conf_exp
}

# ind-day case AND control hospitalizations of interest (overlap possible, e.g., a ind day could have cardioresp and adrd)
data4casecross_ind_final <- bind_rows(data4casecross_ind) %>% 
  arrange(id) %>% 
  select(-CaseDateTime) %>% 
  left_join(., strat_pov_urbanicity, by = "zip") %>% 
  mutate(pct_pov_final = ifelse(year(DayDateTime) <= 2011, pct_pov_0711, pct_pov_1216),
         med_hh_inc_final = ifelse(year(DayDateTime) <= 2011, med_hh_inc_0711, med_hh_inc_1216),
         hosp_of_interest = if_any(contains("out"), ~. > 0, na.rm = TRUE)) %>% # keep only hospitalizations of interest bc a person could be hospitalized for many other reasons
  filter(hosp_of_interest == TRUE) %>% 
  select(-contains(c("0711", "1216")), -hosp_of_interest) 

write_fst(data4casecross_ind_final, '/n/dominici_nsaph_l3/Lab/projects/wfpm_adrd_hosp/data/data_process/data4casecross/data_w_all_vars/data4casecross_ind.fst', compress = 100)

