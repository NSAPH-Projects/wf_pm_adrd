####***********************
#### Code Description ####
# Author: Vivian 
# Date: 5/15/23
# Goal: tidy medicare adrd hospitalizations eda
####**********************

# Read in libraries
path_libraries <- "/n/dominici_nsaph_l3/Lab/projects/power_outages-adrd_hosp-cond_log/code/0_set_up/"
source(paste0(path_libraries, "1_libraries.R"))


# Tidy data ---------------------------------------------------------------
adrd_hosp <- read_fst(paste0(adrd_dir, "ADRD_2006.fst")) %>% 
  filter(AGE <= 115)

# 92407 - San Bernardino zip code (most populated) to test
adrd_hosp_sb <- adrd_hosp %>% 
  filter(zipcode_R == 92407)


state_code <- adrd_hosp %>% 
  group_by(SSA_STATE_CD) %>% 
  mutate(n_zip_in_state = n()) %>% 
  filter(row_number() == 1) %>% 
  select(SSA_STATE_CD, n_zip_in_state)
