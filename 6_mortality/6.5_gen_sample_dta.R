####***********************
#### Code Description ####
# Author: Vivian
# Date: 12/14/23
# Goal: Generate a subsample of the dataset we will use for gridsearch to obtain df in non linear meterological terms
####**********************

####********************
#### 0: Preparation #### 
####********************
rm(list=ls())
source(here::here("0_set_up", "1_libraries.R"))

# 0c Set up filepath(s)
data_path <- '/n/dominici_nsaph_l3/Lab/projects/wfpm_adrd_hosp/data/'
data_path_data4casecross <- "/n/dominici_nsaph_l3/Lab/projects/wfpm_adrd_hosp/data/data_process_mortality/data4casecross/"
data_path_stratification_vars <- '/n/dominici_nsaph_l3/Lab/projects/wfpm_adrd_hosp/data/data_process/stratification_vars/'
output_path <- paste0(data_path, "output_mortality")


# Read in data ------------------------------------------------------------
data4casecross_zip <- read_fst('/n/dominici_nsaph_l3/Lab/projects/wfpm_adrd_hosp/data/data_process_mortality/data4casecross/data_w_all_vars/data4casecross_zip.fst')

data4casecross_zip_ids <- read_fst('/n/dominici_nsaph_l3/Lab/projects/wfpm_adrd_hosp/data/data_process_mortality/data4casecross/data_w_all_vars/data4casecross_zip.fst') %>% 
  select(DayDateTime, dayName, id, zip) %>% 
  filter(dayName == "Caseday_0") 

set.seed(888)

# get 25% of each year with no replacement
samp_25pct_ids <- data4casecross_zip_ids %>%
  mutate(year = year(DayDateTime)) %>% 
  group_by(year) %>%
  sample_n(size = round((0.25*nrow(data4casecross_zip_ids))/length(unique(year(data4casecross_zip_ids$DayDateTime)))),
           replace = FALSE)

samp_25pct <- data4casecross_zip %>% 
  filter(id %in% samp_25pct_ids$id) %>% 
  arrange(id) %>% 
  write_fst(., '/n/dominici_nsaph_l3/Lab/projects/wfpm_adrd_hosp/data/data_process_mortality/samp_data/samp_data_25pct.fst')

samp_25pct <- read_fst('/n/dominici_nsaph_l3/Lab/projects/wfpm_adrd_hosp/data/data_process/samp_data/samp_data_25pct.fst')



