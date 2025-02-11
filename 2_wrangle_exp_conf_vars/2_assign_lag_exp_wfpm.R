####***********************
#### Code Description ####
# Author: Vivian
# Date: 5/12/23
# Goal: assign lagged days of wildfire pm 
####**********************


####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Define Functions for Assigning Lagged Exposures
# 2: Assign Lagged Exposures
# 3: Save Out New Datasets


####**************
#### N: Notes #### 
####**************

# In this script, the final goal is to create a dataset with lagged daily wf pm data
# exposures assigned to hospitalization cases and controls, to be used in later 
# case-crossover analyses. 


####********************
#### 0: Preparation #### 
####********************
rm(list=ls())
source(here::here("0_set_up", "1_libraries.R"))

# 0c Set up filepath(s)
data_path <- '/n/dominici_nsaph_l3/Lab/projects/wfpm_adrd_hosp/data/'
data_path_external <- paste0(here(data_path, "data_process/case_cntrl_days/"))

####***************************************************
#### 1: Define Functions to Assign Lagged Exposure ####
####***************************************************

# Notes: We define one function for zip code as a spatial unit 

# 1.2a Name function for assigning lagged exposure to zip codes
assign_laggedExp_zip <- function(dta_outcome, dta_exp, numLag){
  # dta_outcome <- data4casecross_zip
  # dta_exp <- wfpm_preds
  # numLag <- 2
  # 
  # 1.2b Create variable name 
  VarName <- paste0('wfpm_lag', str_pad(numLag, 2, 'left', '0'))
  
  # 1.2c Create column of lag 
  dta_outcome <- dta_outcome %>% 
    mutate(activeLag := DayDateTime - as.period(1 * numLag, 'day')) %>% 
    mutate(zip_activeLag = paste0(zipcode_R, activeLag))
  
  # 1.2d Join with exposure data 
  dta_outcome <- dta_outcome %>% 
    left_join(dta_exp, by = 'zip_activeLag')
  
  # 1.2e Rename lagged exposure column
  # this is done in two steps because it is tricky to do dynamic variable naming 
  # with the rename() function. mutate + select does the same thing.
  # for this wf pm data, dynamically replace missing wf pm values with 0 because they are non wildfire pm zip-days 
  dta_outcome <- dta_outcome %>% 
    mutate(!!VarName := smoke) %>% 
    dplyr::select(-smoke, -activeLag, -zip_activeLag) 
  
}

####*******************************
#### 2: Test function of assigning lagged exposure ####
####*******************************

# Notes: zip-wide loop takes approximately XXX hour to run

# 2a Assign daily zip exposures via a loop
#    Note: no months have been excluded yet
data4casecross_zip <- read_fst(paste0(data_path, "data_process/case_cntrl_days/cntrl_days_zip_2006.fst")) %>% 
  filter(zipcode_R == "1001")
wfpm_preds <- read_csv(paste0(data_path, "wildfire_smoke_pm25_per_zipcode/daily_zip_2006.csv")) %>% 
  mutate(zip = as.numeric(zip), # change this to match the fact that outcome data treats zips as integer
         zip_activeLag = paste0(zip, date)) %>% 
  filter(zip == "1001") %>% 
  select(smoke, zip_activeLag)

## MA is finally in here
# test <- wfpm_preds %>%
#   filter(zip == "1001")

maxLag <- 2
data_w_lags <- data4casecross_zip # original data
for(l in 0:(maxLag-1)){
  print(l)
  data4casecross_zip <- assign_laggedExp_zip(dta_outcome = data4casecross_zip, 
                                             dta_exp = wfpm_preds, 
                                             numLag = l)
}


####*******************************
#### 3: Generate lagged values for case/control days for all years ####
####*******************************

start_time <- Sys.time() 
for (year in (2006:2016)){
  
  # year <- 2006
  print(year)
  
  # read in case and control days
  data4casecross_zip <- read_fst(paste0(data_path, "data_process/case_cntrl_days/cntrl_days_zip_", year, ".fst"))
  
  # read in the previous year's data because we want to include lags from prev year for 1/1/20XX (except for 2006 bc 2005 DNE)
  if (year != 2006){
    wfpm_preds_year_before <- read_csv(paste0(data_path, "wildfire_smoke_pm25_per_zipcode/daily_zip_", year - 1, ".csv")) 
    wfpm_preds_year_current <- read_csv(paste0(data_path, "wildfire_smoke_pm25_per_zipcode/daily_zip_", year, ".csv")) 
    
    wfpm_preds <- rbind(wfpm_preds_year_before, wfpm_preds_year_current)
    
    wfpm_preds <- wfpm_preds %>%
      mutate(zip = as.numeric(zip),
             zip_activeLag = paste0(zip, date)) %>%
      select(smoke, zip_activeLag)
  }
  
  else{
    wfpm_preds <- read_csv(paste0(data_path, "wildfire_smoke_pm25_per_zipcode/daily_zip_", year, ".csv")) %>%
      mutate(zip = as.numeric(zip),
             zip_activeLag = paste0(zip, date)) %>%
      select(smoke, zip_activeLag)
    
    # # does 1001 zip exist? yes!
    # zip_1001 <- wfpm_preds %>%
    #   filter(str_detect(zip, "1001"))
  }
  
  ActiveYear <- year
  maxLag <- 14
  
  for(l in 0:(maxLag-1)){
    print(l)
    data4casecross_zip <- assign_laggedExp_zip(dta_outcome = data4casecross_zip, #note that we are reading in, updating with lags, and then saving data4casecross_zip
                                               dta_exp = wfpm_preds,
                                               numLag = l)
  }
  
  data4casecross_zip %>%
    rename(zip = zipcode_R) %>%
    # group_by(zip, DayDateTime) %>%
    # filter(row_number() == 1) %>%
    fst::write_fst(paste0(data_path, 'data_process/data4casecross/data_w_lagged_wfpm/',
                          'data4casecross_', ActiveYear, '.fst'))
  
}
end_time <- Sys.time()
end_time - start_time 


