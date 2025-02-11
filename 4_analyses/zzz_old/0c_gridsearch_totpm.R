####***********************
#### Code Description ####
# Author: Vivian
# Date: 8/28/23
# Goal: Run gridsearch with totpm as the main exposure and check circulatory bc results are unexpected
####**********************

# Set up ------------------------------------------------------------------
rm(list=ls())
options(digits = 18)
data_path <- '/n/dominici_nsaph_l3/Lab/projects/wfpm_adrd_hosp/data/'
output_path <-"/n/dominici_nsaph_l3/Lab/projects/wfpm_adrd_hosp/data/output"
source(here::here("0_set_up", "1_libraries.R"))

# read in sample data
samp_data_25pct <- read_fst('/n/dominici_nsaph_l3/Lab/projects/wfpm_adrd_hosp/data/data_process/samp_data/samp_data_25pct.fst')

# Add needed vars for  analysis
samp_data_25pct <- samp_data_25pct %>% 
  mutate(dayzip = paste0(DayDateTime, zip),
         Case = ifelse(dayName == 'Caseday_0', 1, 0), # bivariate Case variable
         TimetoEvent = ifelse(dayName == 'Caseday_0', 1, 2)) # bivariate Case variable for coxph model (if using instead of clogit) 

# read through totpm years 
years <- c(2006:2016)
totpm <- data.frame()
for (year in 1:length(years)){
  
  print(years[year])
  totpm_yr <- read_fst(paste0(data_path, "/data_process/data4casecross/data_w_lagged_totpm/data4casecross_", years[year], ".fst")) %>%
    select(-contains("year")) %>% 
    mutate(dayzip = paste0(DayDateTime, zip)) %>%
    filter(dayzip %in% samp_data_25pct$dayzip)

  totpm <- rbind(totpm, totpm_yr)

}

# join sampe data and totpm
samp_data_25pct_w_totpm <- samp_data_25pct %>% 
  left_join(., totpm)

ids_w_missing_totpm <- samp_data_25pct_w_totpm %>% 
  select(DayDateTime, id, zip, contains(c("totpm")))
ids_w_missing_totpm$number_na <- rowSums(is.na(ids_w_missing_totpm)) 
ids_w_missing_totpm <- ids_w_missing_totpm %>% 
  group_by(id) %>% 
  mutate(ind_missing = ifelse(sum(number_na) >= 1, 1, 0)) %>% 
  filter(row_number() == 1,
         ind_missing == 1) %>% 
  select(id)

samp_data_25pct_w_totpm_full <- samp_data_25pct_w_totpm %>%
  filter(!id %in% ids_w_missing_totpm$id)
  

####****************************************************************************** BEGIN FUNCTION
# Here I edit functions developed by Sebastian Rowland


####*******************************************************
#### Create function to run gridsearch #### 
####*******************************************************

# 4a Name function
gridsearch_df <- function(ExpTerm, CaseType, ERConstraint, LRConstraint, dta, lag_number){
  # ExpTerm <- 'totpm'; CaseType <- 'count_all_cause_hosp_out';
  # ERConstraint <- '4dfevenknots'; LRConstraint <- '3dfevenknots'; #ERConstraint <- '4dfevenknots'
  # dta <- samp_data_25pct_w_totpm_full
  
  # filter to days with cases along with their controls (bc a case cannot be a day without 0 events)
  dta <- dta %>% 
    filter(!!sym(CaseType) > 0)
  
  # 4b Create ModelName
  ModelIdentifier <- paste0(ExpTerm, '_', CaseType)
  ExpConstraints <- paste0('ER', ERConstraint, '_LR', LRConstraint)
  LagName <- paste0("Lag", lag_number)
  ModelName <- paste0(ModelIdentifier,'_', ExpConstraints, "_", LagName)
  NumLag <- as.numeric(lag_number)
  
  ####***************************
  #### Create cross basis #### 
  ####***************************
  
  # Set ER (exposure response) and LR (lagged response) constraints
  ERdf <- as.numeric(str_remove_all(ERConstraint, '[A-z]')) # Remove all letters, leaving only number of df (degrees of freedom)
  LRdf <- as.numeric(str_remove_all(LRConstraint, '[A-z]'))
  
  # Create cross basis for temperature
  cb.temp <- crossbasis(
    as.matrix(dplyr::select(dta, contains('temp')))[,1:NumLag], 
    lag=c(0,(NumLag-1)),
    argvar=list(fun='ns', df = 3),
    arglag=list(fun='ns', df = LRdf))
  
  # Create cross basis for relative humidity
  cb.rh <- crossbasis(
    as.matrix(dplyr::select(dta, contains('rh')))[,1:NumLag], 
    lag=c(0,(NumLag-1)),
    argvar=list(fun='ns', df = 3),
    arglag=list(fun='ns', df = LRdf))
  
  # Create cross basis for totpm
  # a nonlinear totpm term
  if(str_detect(ERConstraint, 'evenknots') & str_detect(LRConstraint, 'evenknots')){
    cb.totpm <- crossbasis(
      as.matrix(dplyr::select(dta, starts_with('totpm_lag')))[,1:NumLag], 
      lag=c(0,(NumLag-1)),
      argvar=list(fun='ns', df = ERdf),
      arglag=list(fun='ns', df = LRdf))
  }
  
  # a linear totpm term
  if(str_detect(ERConstraint, 'lin') & str_detect(LRConstraint, 'evenknots')){
    cb.totpm <- crossbasis(
      as.matrix(dplyr::select(dta, starts_with('totpm_lag')))[,1:NumLag],
      lag=c(0,(NumLag-1)),
      argvar=list(fun='lin'),
      arglag=list(fun='ns', df = LRdf))
  }
  
  ####****************************
  #### Create health model #### 
  ####****************************
  
  # Health model for main analysis  
  # prepare the weights variable
  dta_weights <- dta %>% 
    select(!!sym(CaseType))
  
  # clogit
  mod <- clogit(Case ~ cb.totpm +
                  cb.temp +                        # LR and ER temp
                  cb.rh +                          # LR and ER rh
                  strata(id),
                weights = dta_weights[[1]],        # num of daily events (could only use if exposure is same for all cases/controls in a given hour); set this to be our outcome of interest, which can change by input!
                method = "efron",
                data = dta)
  
  
  ####***************************************
  #### Save model AIC for grid search #### 
  ####***************************************
  
  # 8a Readin the table of AIC's
  aic.table <- read_csv(paste0(output_path, '/tables/', 'Model_AIC.csv'), 
                        col_types = 'ccccdT') %>% 
    mutate(LagName = as.character(LagName)) %>% 
    select(ModelIdentifier, ERConstraint, LRConstraint, LagName, AIC, RunDate)
  
  # 8b Add this aic to the set of AIC's
  aic.table[1+nrow(aic.table),] <- list(ModelIdentifier,
                                        ERConstraint, LRConstraint, LagName, AIC(mod), Sys.time())
  
  # 8c Remove any old AICs and then save
  # at the slice step you keep only the earliest AIC for each model-constraint combo
  aic.table %>% 
    group_by(ModelIdentifier,ERConstraint, LRConstraint, LagName) %>% 
    arrange(desc(RunDate)) %>% 
    slice(0:1) %>% 
    filter(!is.na(ModelIdentifier)) %>%
    write_csv(paste0(output_path, '/tables/', 'Model_AIC.csv'))
}

####****************************************************************************** END FUNCTION


####**************************************************
#### Implement grid search to determine df for ER and LR #### 
####**************************************************

# create a grid of candidate df; used for all searches across hospitalization types
CandidateConstraintsGrid_lag7 <- data.frame(
  ERConstraint = rep(c('linear','2dfevenknots', '3dfevenknots'), 3), 
  LRConstraint = c(rep('4dfevenknots', 3), rep('5dfevenknots', 3), rep('6dfevenknots', 3)),
  lag_number = c(rep("7", 9)))

CandidateConstraintsGrid_lag14 <- data.frame(
  ERConstraint = rep(c('linear','2dfevenknots', '3dfevenknots'), 3), 
  LRConstraint = c(rep('4dfevenknots', 3), rep('5dfevenknots', 3), rep('6dfevenknots', 3)),
  lag_number = c(rep("14", 9)))

CandidateConstraintsGrid <- rbind(CandidateConstraintsGrid_lag7, CandidateConstraintsGrid_lag14)

# set variables for function
ExpTerm <- 'totpm'
dta <- samp_data_25pct_w_totpm_full
list_CaseType <- c('count_all_cause_hosp_out', 'count_ADRD_prmy_out', 'count_resp_prmy_out', 
                   'count_circ_prmy_out', 'count_anxty_prmy_out', 'count_dep_prmy_out')

# loop through all primary hospitalization outcomes for gridsearch
start_time <- Sys.time() 
for (case in 1:length(list_CaseType)){
  print(list_CaseType[[case]])
  
  ModelIdentifier <- paste0(ExpTerm, '_', list_CaseType[[case]])
  # print(ModelIdentifier)
  
  for(i in 1:nrow(CandidateConstraintsGrid)){
    gridsearch_df(ExpTerm = 'totpm',
                  CaseType = list_CaseType[[case]], 
                  ERConstraint = CandidateConstraintsGrid$ERConstraint[i], 
                  LRConstraint = CandidateConstraintsGrid$LRConstraint[i], 
                  lag_number = CandidateConstraintsGrid$lag_number[i],
                  dta = samp_data_25pct_w_totpm_full)
  }
}

end_time <- Sys.time()
end_time - start_time

# Read in table of model AICs
aic.table0 <- read_csv(paste0(output_path, '/tables/', 'Model_AIC.csv'), 
                       col_types = 'ccccdT') 

aic.table <- aic.table0 %>% 
  filter(LagName == "Lag7",
         str_detect(ModelIdentifier, "totpm"),
         str_detect(ModelIdentifier, "circ"))

View(aic.table0)





