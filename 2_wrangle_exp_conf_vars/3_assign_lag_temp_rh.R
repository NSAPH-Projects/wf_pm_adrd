####***********************
#### Code Description ####
# Author: Vivian
# Date: 5/12/23
# Goal: assign lagged days of confounders (temp, rh, non wf pm)
####**********************

####**************
#### N: Notes #### 
####**************

# In this script, the final goal is to create a dataset with lagged daily temp, rh, non wf pm data
# assigned to hospitalization cases and controls, to be used in later 
# case-crossover analyses. These are for confounder variables/for sensitivity analyses


####********************
#### 0: Preparation #### 
####********************
rm(list=ls())
source("/n/dominici_nsaph_l3/Lab/projects/wfpm_adrd_hosp/code/0_set_up/1_libraries.R")

# 0c Set up filepath(s)
data_path <- '/n/dominici_nsaph_l3/Lab/projects/wfpm_adrd_hosp/data'
data_path_external <- paste0(here(data_path, "data_process/case_cntrl_days/"))
data_path_invest <- paste0(here(data_path, "invest/"))
data_path_temp <- "/n/dominici_nsaph_l3/Lab/data/gridmet_flat/maximum_air_temperature/"
data_path_rh <- "/n/dominici_nsaph_l3/Lab/data/gridmet_flat/minimum_relative_humidity/"

####***************************************************
#### 1: Define Functions to Assign Lagged Confounders ####
####***************************************************

# Notes: We define one function for zip code as a spatial unit 

# 1.2a Name function for assigning lagged exposure to zip codes
# var_conf should be the confounder variable name
assign_laggedConf_zip <- function(dta_outcome, dta_conf, numLag, var_conf){
  # dta_outcome <- data4casecross_zip
  # # dta_conf <- nonwfpm %>% select(pm25, zip_activeLag)
  # dta_conf <- stacked_temperature_rh
  # numLag <- 3
  # var_conf <- "max_temp_kelvin"
  
  # 1.2b Create variable name 
  VarName <- paste0(var_conf, "_", str_pad(numLag, 2, 'left', '0'))
  
  # 1.2c Create column of lag
  dta_outcome1 <- dta_outcome %>%
    # mutate(DayDateTime = date) %>%
    mutate(activeLag := DayDateTime - as.period(1 * numLag, 'day')) %>%
    mutate(zip_activeLag = paste0(zip, activeLag))
  
  # 1.2d Join with exposure data
  dta_outcome2 <- dta_outcome1 %>%
    left_join(dta_conf, by = 'zip_activeLag')
  
  # 1.2e Rename lagged exposure column
  # this is done in two steps because it is tricky to do dynamic variable naming
  # with the rename() function. mutate + select does the same thing.
  dta_outcome3 <- dta_outcome2 %>%
    mutate(!!VarName := get(var_conf)) %>%
    dplyr::select(-var_conf, -activeLag, -zip_activeLag)
}


####*******************************
#### 2: TEMPERATURE AND RH: Generate lagged confounder values for case/control days for all years ####
####*******************************

start_time <- Sys.time() 
for (year in (2006:2016)){
  
  # notes: 
  # 1. for loop, we want to read in the year before and current year to fill info
  # for any lags prior to 1/1/20XX
  # 2. we load in before/current years separately because pre-2009 and post-2010 
  # have different sets of zip codes
  
  print(year)
  
  ActiveYear <- year
  maxLag <- 14
  
  # read in case and control days
  data4casecross_zip <- read_fst(paste0(data_path, "/data_process/case_cntrl_days/cntrl_days_zip_", year, ".fst")) %>%
    rename(zip = zipcode_R)
  
  # temperature
  data_temperature_year_before <- get(load(paste0(data_path_temp, year - 1, "_maximum_air_temperature_by_zip.RData")))
  data_temperature_year_current <- get(load(paste0(data_path_temp, year, "_maximum_air_temperature_by_zip.RData")))
  
  data_temperature_year_before$zip <- row.names(data_temperature_year_before)
  data_temperature_year_current$zip <- row.names(data_temperature_year_current)
  
  data_temperature <- full_join(data_temperature_year_before, data_temperature_year_current, by = "zip")
  
  data_temperature_long <- pivot_longer(data_temperature, cols = -starts_with("zip"), names_to = "date", values_to = "max_temp_kelvin")
  
  # relative humidity
  data_rh_year_before <- get(load(paste0(data_path_rh, year - 1, "_minimum_relative_humidity_by_zip.RData")))
  data_rh_year_current <- get(load(paste0(data_path_rh, year, "_minimum_relative_humidity_by_zip.RData")))
  
  data_rh_year_before$zip <- row.names(data_rh_year_before)
  data_rh_year_current$zip <- row.names(data_rh_year_current)
  
  data_rh <- full_join(data_rh_year_before, data_rh_year_current, by = "zip")
  
  data_rh_long <- pivot_longer(data_rh, cols = -starts_with("zip"), names_to = "date", values_to = "min_rh")
  
  # join temperature and relative humidity data
  stacked_temperature_rh <- full_join(data_temperature_long, data_rh_long, by = c("zip", "date")) %>%
    mutate(zip = as.numeric(zip),
           zip_activeLag = paste0(zip, date)) %>%
    select(-zip, -date)
  
  # remove datasets we no longer need
  rm(data_temperature_year_before, data_temperature_year_current, data_temperature, data_temperature_long, 
     data_rh_year_before, data_rh_year_current, data_rh, data_rh_long)
  
  data4casecross_zip_temp <- data4casecross_zip
  data4casecross_zip_rh <- data4casecross_zip
  
  # run the actual function to create the lags for temperature and relative humidity
  for(l in 0:(maxLag-1)){
    print(l)
    
    data4casecross_zip_temp <- assign_laggedConf_zip(dta_outcome = data4casecross_zip_temp,
                                                     dta_conf = stacked_temperature_rh,
                                                     numLag = l,
                                                     var_conf = "max_temp_kelvin") %>% 
      select(-min_rh)
    data4casecross_zip_rh <- assign_laggedConf_zip(dta_outcome = data4casecross_zip_rh,
                                                   dta_conf = stacked_temperature_rh,
                                                   numLag = l,
                                                   var_conf = "min_rh") %>% 
      select(-max_temp_kelvin)
    
    data4casecross_zip_temp_rh <- full_join(data4casecross_zip_temp, data4casecross_zip_rh, by = c("zip", "CaseDateTime", "YYYY", "id", 
                                                                                                   "spatialUnit", "dayName", "DayDateTime")) %>% 
      group_by(zip, DayDateTime) %>% 
      filter(row_number() == 1) %>% 
      select(zip, DayDateTime, contains(c("temp", "rh")))
    
  }
  
  # save all our hard work
  data4casecross_zip_temp_rh %>%
    fst::write_fst(paste0(data_path, '/data_process/data4casecross/data_w_lagged_temp_rh/',
                          'data4casecross_', ActiveYear, '.fst'))
  
}
end_time <- Sys.time() 
end_time - start_time 



