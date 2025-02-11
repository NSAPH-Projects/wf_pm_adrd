####***********************
#### Code Description ####
# Author: Vivian 
# Date: 7/27/2023
# Goal: using Shuxin ADRD data, identify first ADRD hospitalization from:
# 1. 2000-2016 (primary)
# 2. 2006-2016 (sensitivity)
# NOTE: this list does not consider which observations have full data for case crossover, exposure data, or validated zips
####**********************

source(here::here("0_set_up", "1_libraries.R"))
data_path <- '/n/dominici_nsaph_l3/Lab/projects/wfpm_adrd_hosp/data/'

# Read in data ------------------------------------------------------------
file_names <- list.files(paste0(data_path, "adrd_hospitalization"), full.names = TRUE)[1:17]
yearly_hosp_zip <- lapply(file_names, read_fst)

#NOTE: THE ADRD DATA YEAR IN THE FILE NAME REFERS TO DISCHARGE DATE!!!! (NOT ADMISSION DATE)
# for example, the adrd file ending in 2008 can contain admissions from 2006, 2007, 2008
for (year in seq_along(yearly_hosp_zip)){
  print(paste0("ADRD file ending in ", unique(year(yearly_hosp_zip[[year]]$DDATE)),
               " contains the following admission year: ", unique(year(yearly_hosp_zip[[year]]$ADATE))))
}


# Loop through all years, keeping only relevant vars ----------------------
yearly_unique_ids_w_adrd <- list()
for (yearly_dta in 1:length(yearly_hosp_zip)){
  
  print(yearly_dta)

  dta_w_rel_vars <- yearly_hosp_zip[[yearly_dta]] %>% 
    select(QID, ADATE, zipcode_R, AGE, Sex_gp, contains(c("primary", "secondary"))) %>% 
    group_by(QID) %>% 
    filter(row_number() == 1)
  
  yearly_unique_ids_w_adrd[[yearly_dta]] <- dta_w_rel_vars
  
}

# Keep first adrd hosp instance - baseline year 2000 --------------
first_adrd_hosp <- do.call(rbind, yearly_unique_ids_w_adrd)
first_adrd_hosp_beg_2000 <- first_adrd_hosp %>% 
  group_by(QID) %>% 
  filter(row_number() == 1)

write_fst(first_adrd_hosp_beg_2000, paste0(data_path, "data_process/first_adrd_hosp_beg_2000.fst"))

# Keep first adrd hosp instance - baseline year 2006 --------------
first_adrd_hosp_beg_2006 <- first_adrd_hosp %>% 
  filter(year(ADATE) >= 2006) %>% 
  group_by(QID) %>% 
  filter(row_number() == 1)

write_fst(first_adrd_hosp_beg_2006, paste0(data_path, "data_process/first_adrd_hosp_beg_2006.fst"))

