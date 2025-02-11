####***********************
#### Code Description ####
# Author: Vivian 
# Date: 8/3/23
# Goal: keep all hospitalizations of those with ADRD hospitalization after first ADRD diagnosis
# 1. individual level data
# 2. zip aggregated level data
# Generate for cohort determined at 2006
####**********************

rm(list=ls())
source(here::here("0_set_up", "1_libraries.R"))
library(vroom)
data_path <- '/n/dominici_nsaph_l3/Lab/projects/wfpm_adrd_hosp/data/'

# Read in data ------------------------------------------------------------

# first adrd hospitalization for each individual, starting in 2006
first_adrd_hosp_beg_2006 <- read_fst(paste0(data_path, "data_process/first_adrd_hosp_beg_2006.fst")) %>% 
  rename(adm_date_first_adrd = ADATE) %>% 
  select(QID, zipcode_R, AGE, Sex_gp, adm_date_first_adrd)

first_adrd_hosp_beg_2006 <- first_adrd_hosp_beg_2006 %>% 
  select(-zipcode_R)

# xwalk of zcta zip - use this to keep only valid zips
valid_zips <- read_csv("/n/dominici_nsaph_l3/Lab/projects/wfpm_adrd_hosp/data/data_process/valid_zips_zctas.csv") %>% 
  select(-zcta) %>%
  mutate(zip = as.integer(zip), 
         link_zipyear = paste0(zip, year))

# read in DOB 
bene_dob <- read_csv("/n/dominici_nsaph_l3/Lab/projects/analytic/auxiliary_medicare_cols/dob.csv")

# Run ---------------------------------------------------
# use yearly admission data (Yoon's data)
# read in yearly hospitalization data, keeping only
# (1) hospitalizations that occurred after first adrd hospitalization
# (2) valid zips
# (3) bound within our study period
years <- c(2006:2016)
hosp_after_first_adrd_ind_2006cohort <- data.frame()

for (year in years){
  print(year)
  
  yrly_hosp_data <-  vroom(paste0(data_path, "admissions_by_year/admissions_", year, ".csv"), delim = ",")
  
  study_hosp_data <- yrly_hosp_data %>%
    select(QID, ADATE, DIAG1, DIAG2, DIAG3, zipcode_R, AGE, Sex_gp) %>%
    mutate(adm_date_hosp = as.Date(ADATE, format = "%d%b%Y")) %>%
    inner_join(.,
               first_adrd_hosp_beg_2006 %>%
                 select(QID, adm_date_first_adrd),
               by = "QID") %>%
    filter(adm_date_first_adrd <= adm_date_hosp,
           year(adm_date_hosp) >= 2006,
           year(adm_date_hosp) <= 2016) %>%
    rename(age = AGE,
           sex_group = Sex_gp) %>%
    mutate(link_zipyear = paste0(zipcode_R, year(adm_date_hosp))) %>%
    left_join(., valid_zips, by = "link_zipyear") %>%
    filter(!is.na(zip))
  
  hosp_after_first_adrd_ind_2006cohort <- rbind(hosp_after_first_adrd_ind_2006cohort, study_hosp_data)
  
}

# # Assign hospital outcomes to icd codes --------------------------------------------------------
# identify diagnosis variables in mdcr data
source(here::here("1_gather_study_pop", "0_assign_diagnosis_codes.R"))

# the below lets us look at diag1:diag3
assign_hosp_outcome <- function(hosp_dta) {
  output_data <- hosp_dta %>%
    ungroup() %>%
    mutate(circ_prmy_out = as.numeric(rowSums(across(DIAG1:DIAG3, ~str_detect(., paste0("^(", paste(icd_circ, collapse = "|"), ")"))), na.rm = TRUE) > 0),
           resp_prmy_out = as.numeric(rowSums(across(DIAG1:DIAG3, ~str_detect(., paste0("^(", paste(icd_resp, collapse = "|"), ")"))), na.rm = TRUE) > 0),
           anxty_prmy_out = as.numeric(rowSums(across(DIAG1:DIAG3, ~str_detect(., paste0("^(", paste(icd_anxty, collapse = "|"), ")"))), na.rm = TRUE) > 0),
           dep_prmy_out = as.numeric(rowSums(across(DIAG1:DIAG3, ~str_detect(., paste0("^(", paste(icd_dep, collapse = "|"), ")"))), na.rm = TRUE) > 0)) %>% 
    ungroup()
  return(output_data)
}

hosp_after_first_adrd_ind_2006cohort <- assign_hosp_outcome(hosp_after_first_adrd_ind_2006cohort) %>% 
  select(QID, adm_date_hosp, zipcode_R, age, sex_group, ends_with("out"))
names(hosp_after_first_adrd_ind_2006cohort)

# join with the medicare beneficiary data (denominator file)
# add DOB to calculate age

hosp_after_first_adrd_ind_2006cohort <- hosp_after_first_adrd_ind_2006cohort %>% 
  left_join(., 
            bene_dob %>% 
              select(bene_id, dob) %>% 
              rename(QID = bene_id),
            by = "QID") %>% 
  mutate(age = floor(as.numeric(difftime(adm_date_hosp, dob, units = "days"))/365)) 
nrow(hosp_after_first_adrd_ind_2006cohort)

write_rds(hosp_after_first_adrd_ind_2006cohort, paste0(data_path, "data_process/hosp_dta/hosp_after_first_adrd_ind_2006cohort.rds"))


# Generate case days dataset -------------------------------------------------------------------------
#* this is to more easily identify case and control days for exposure and confounders
hosp_after_first_adrd_case_days_2006cohort <- hosp_after_first_adrd_ind_2006cohort %>% 
  select(adm_date_hosp, zipcode_R) %>% 
  group_by(adm_date_hosp, zipcode_R) %>% 
  filter(row_number() == 1) %>% 
  ungroup()
write_rds(hosp_after_first_adrd_case_days_2006cohort, paste0(data_path, "data_process/hosp_dta/hosp_after_first_adrd_case_days_2006cohort.rds"))
# hosp_after_first_adrd_case_days_2006cohort <- readRDS(paste0(data_path, "data_process/hosp_dta/hosp_after_first_adrd_case_days_2006cohort.rds"))

# Generate zip-age level dataset -------------------------------------------------------------------------
#* this is to more efficiently (computationally) run a case crossover
#* identify whether the hospitalization is one of interest and then group by outcome
# hosp_after_first_adrd_ind_2006cohort <- read_rds(paste0(data_path, "data_process/hosp_dta/hosp_after_first_adrd_ind_2006cohort.rds"))

hosp_after_first_adrd_zip_2006cohort <- hosp_after_first_adrd_ind_2006cohort %>% 
  group_by(adm_date_hosp, zipcode_R) %>% 
  mutate(across(ends_with("_out"), ~ sum(. == 1, na.rm = TRUE))) %>% 
  rename_with(~ paste0("count_", .x, recycle0 = TRUE), ends_with("_out")) %>% 
  filter(row_number() == 1) %>% 
  select(adm_date_hosp, zipcode_R, starts_with("count"))
write_rds(hosp_after_first_adrd_zip_2006cohort, paste0(data_path, "data_process/hosp_dta/hosp_after_first_adrd_zip_2006cohort.rds")) 


