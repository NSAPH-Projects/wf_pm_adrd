####***********************
#### Code Description ####
# Author: Vivian 
# Date: 12/12/23
# Goal: using Yoon's data, generate mortality data of those with ADRD diagnosis
####**********************

rm(list=ls())
source(here::here("0_set_up", "1_libraries.R"))
library(vroom)
data_path <- '/n/dominici_nsaph_l3/Lab/projects/wfpm_adrd_hosp/data/'

# Read in data ------------------------------------------------------------

# first adrd hospitalization for each individual, starting in 2000 and in 2006
first_adrd_hosp_beg_2000 <- read_fst(paste0(data_path, "data_process/first_adrd_hosp_beg_2000.fst")) %>% 
  rename(adm_date_first_adrd = ADATE) %>% 
  select(QID, AGE, Sex_gp, adm_date_first_adrd)
first_adrd_hosp_beg_2006 <- read_fst(paste0(data_path, "data_process/first_adrd_hosp_beg_2006.fst")) %>% 
  rename(adm_date_first_adrd = ADATE) %>% 
  select(QID, AGE, Sex_gp, adm_date_first_adrd)

# xwalk of zcta zip - use this to keep only valid zips
valid_zips <- read_csv("/n/dominici_nsaph_l3/Lab/projects/wfpm_adrd_hosp/data/data_process/valid_zips_zctas.csv") %>% 
  select(-zcta) %>%
  mutate(zip = as.integer(zip), 
         link_zipyear = paste0(zip, year))

# read in DOB 
bene_dob <- read_csv("/n/dominici_nsaph_l3/Lab/projects/analytic/auxiliary_medicare_cols/dob.csv")


# Read in date of death (in denominator file) -----------------------------

years <- c(2006:2016)
dod_after_first_adrd_hosp <- data.frame()

for (year in years){
  
  print(year)
  yrly_dod_dta <- read_fst(paste0("/n/dominici_nsaph_l3/Lab/projects/analytic/denom_by_year/confounder_exposure_merged_nodups_health_", year, ".fst"), 
                   columns = c("qid", "zip", "bene_dod")) %>% 
    rename(zip_dod = zip)
  
  # keep valid zips
  study_dod_dta <- left_join(first_adrd_hosp_beg_2000, yrly_dod_dta, by = c("QID" = "qid")) %>% 
    filter(!is.na(bene_dod),
           nchar(bene_dod) > 0) %>%
    mutate(bene_dod = as.Date(bene_dod, format = "%d%b%Y"),
           link_zipyear = paste0(zip_dod, year(bene_dod))) %>%
    left_join(., valid_zips, by = "link_zipyear") %>%
    filter(!is.na(zip)) %>% 
    select(-ZIPType, year, zip)
  
  dod_after_first_adrd_hosp <- rbind(dod_after_first_adrd_hosp, study_dod_dta)

}

nrow(dod_after_first_adrd_hosp) #4 729 976

write_rds(dod_after_first_adrd_hosp, paste0(data_path, "data_process/mortality_dta/dod_after_first_adrd_hosp.rds")) 


# collapse to zip level ---------------------------------------------------
dod_after_first_adrd_hosp <- read_rds(paste0(data_path, "data_process_mortality/mortality_dta/dod_after_first_adrd_hosp.rds")) 

dod_after_first_adrd_hosp_zip <- dod_after_first_adrd_hosp %>% 
  group_by(bene_dod, zip_dod) %>% 
  mutate(count_death = n()) %>% 
  filter(row_number() == 1) %>% 
  select(bene_dod, zip_dod, starts_with("count"))
write_rds(dod_after_first_adrd_hosp_zip, paste0(data_path, "data_process_mortality/mortality_dta/dod_after_first_adrd_hosp_zip.rds")) 









