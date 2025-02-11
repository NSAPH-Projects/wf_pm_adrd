####***********************
#### Code Description ####
# Author: Vivian
# Date: 5/12/23
# Goal: create control days for each hospital admission
####**********************

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Define functions to create control days
# 2: Test function with single year
# 3: Create control days in parallel


####**************
#### N: Notes #### 
####**************

# We choose control days via time-stratified bidirectional matching 
# We match by year, month, day of the week
# We also use the function to generate the case day, 
# so that all of the days have parallel format.


####********************
#### 0: Preparation #### 
####********************

rm(list=ls())

# 0a Declare root directory
project.folder <- paste0(print(here::here()),'/')

# 0b Load Packages
source(here::here("0_set_up", "1_libraries.R"))

# 0c Set up filepath(s)
data_path <- '/n/dominici_nsaph_l3/Lab/projects/wfpm_adrd_hosp/data/'
data_path_external <- paste0(here(data_path, "data_process/case_cntrl_days/"))

# 0b Load hosp data
# We create control days by zip-date combination because many individuals in the same zip-date could be hospitalized, saves server space this way
# Generate control days for the entire dataset bc the entire dataset contains any ADRD hospitalization 
adrd_zip <- read_rds(paste0(data_path, "data_process/hosp_dta/hosp_after_first_adrd_case_days.rds")) %>% 
  rename(CaseDateTime = adm_date_hosp) %>% 
  ungroup()

summary(adrd_zip$zipcode_R) # has MA 1001 aka 01001

# 0c Confirm each row is a unique spatial unit / datetime observation 
adrd_zip %>% mutate(id = paste0(zipcode_R, CaseDateTime)) %>% summarize(unique_obs = length(unique(id))) == nrow(adrd_zip)

# 0d Add year variable to split dataset by year (for computational speed)
adrd_zip$YYYY <- year(adrd_zip$CaseDateTime)

# 0e Add a unique id variable & spatial unit variable
adrd_zip <- adrd_zip %>% mutate(id = 1:n(), spatialUnit = 'zip')

# 0f Split data into a list of yearly dataframes
years.list_zip <- adrd_zip %>% split(adrd_zip$YYYY)


####*************************************************
#### 1: Define Functions to Create Control days ####
####*************************************************

# Here I edit functions developed by Sebastian Rowland

# 1a Define function to create potentially matching datedays
# Note: our data is in Eastern Time (ET)
#       as such, we are matching on the socially-recognized time that the case would have experienced
#       In particular, due to daylight savings time, certain days do not 'exist' in ET
#       This will lead to some NA's in the dataset 
#       While converting to UTC would avoid these NA's, 
#       the cases experienced time according to ET 
#       and their time-varying factors would follow ET, not UTC 
#       so if we match on UTC we would not be matching on day of the day 

make_control_day <- function(days1, BeforeAfter, WK){ 
  # days1 <- df.YYYY; BeforeAfter <- 'Before'; WK <- 4
  # The name of the day; accounts for if control or case
  VarName <- paste0(BeforeAfter, '_', str_trunc(WK, 1, 'left', ''))    
  # adds WKs number of weeks, preserves day
  days1 %>% mutate(!!VarName := CaseDateTime+ as.period(7 * WK, 'day'))  
}

# 1b Define function to create control days
create_control_days_by_year <- function(df.YYYY){
  #df.YYYY <- years.list[[6]]
  
  # 1c Add progress bar
  # progressr::p()
  
  # 1d Use function to create bidirectionally symmetric datedays 
  days1 <-  df.YYYY
  ActiveYYYY <- days1$YYYY[1]
  days1 <- make_control_day(days1, 'Before', -4)
  days1 <- make_control_day(days1, 'Before', -3)
  days1 <- make_control_day(days1, 'Before', -2)
  days1 <- make_control_day(days1, 'Before', -1)
  days1 <- make_control_day(days1, 'Caseday', 0)
  days1 <- make_control_day(days1, 'After', 1)
  days1 <- make_control_day(days1, 'After', 2)
  days1 <- make_control_day(days1, 'After', 3)
  days1 <- make_control_day(days1, 'After', 4)
  
  # 1e Put in long format by dayName
  days2 <- days1 %>%
    gather('dayName', 'DayDateTime', contains('Caseday_'),
           contains('Before_'), contains('After_') )
  
  # 1f Stratify by month of event; keep only other obs in the month
  days3 <- days2 %>% filter(month(CaseDateTime) == month(DayDateTime))
  
  # # double Check timezone
  # #tz(days3$DayDateTime[1])
  # 
  # 1g Identify spatial unit
  spatialUnit <- ifelse(days3$spatialUnit[1] == 'zip', 'zip', 'zip')
  
  # 1h Save results
  days3 %>%
    fst::write_fst(paste0(data_path_external, 'cntrl_days_', spatialUnit,
                          '_', ActiveYYYY, '.fst'))
}


####************************************
#### 2: Test Function w Single Year ####
####************************************

case_control_2006 <- create_control_days_by_year(years.list_zip[[1]])
head(case_control_2006)

####*****************************************
#### 3: Create control days for all years ####
####*****************************************
for (i in seq_along(1:length(years.list_zip))){
  create_control_days_by_year(years.list_zip[[i]])
}


# check
cntrl_days_zip_2006 <- read_fst(paste0(data_path_external, 'cntrl_days_zip_2006.fst'))




