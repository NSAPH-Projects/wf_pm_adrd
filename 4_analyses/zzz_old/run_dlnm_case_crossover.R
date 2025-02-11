# Conduct DLNM Case-crossover Analyses
# wf pm - hospitalizations
# primary hosp - adrd
# secondary hosp - cardiovascular, respiratory, mental health
# Vivian Do
# Updated 5/23/23

# note 7/14/23 --> read in data used for stratification --> poverty/medhhinc, urbanicity, AGE (age will be diff bc need to create new data)

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Calculate summary stats for use in DLNM crosspreds
# 2: Create counterfactual exposure vector for use in DLNM crosspreds
# 3: Add case and time-to-event variables for models
# 4: Create function to conduct dlnm case-crossover
# 5: Create cross basis
# 6: Create health model
# 7: Save model and create crosspredictions
# 8: Save model AIC for grid search
# 9: Grid search to determine df for ER and lR
# 10: Run main and sensitivity analyses


####**************
#### N: Notes #### 
####**************

# In this script, we create distributed lag non-linear models using a case-crossover
# study design, for the main analysis, secondary analysis, and all sensitivity 
# analyses. Functions in this script were originally developed by Sebastian Rowland.

# spatial resolution - zip code
# temporal resolution - daily

# Main analysis: wf pm - adrd hospitalizations

# Secondary analysis: 
#  Different outcomes
#  Main: wfpm - any adrd outcome
#  1. wf pm - cardiovascular hosp
#  2. wf pm - respiratory hosp
#  3. wf pm - mental health hosp
#   Stratification 
#  1. poverty/medhhinc
#  2. urbanicity
#  3. age

# Sensitivity analyses:
#  1. Include non wildfire pm (nonlinear lag term, 0-6 lag) as a confounder



####********************
#### 0: Preparation #### 
####********************

# 0a Declare root directory
project.folder <- paste0(print(here::here()),'/')

# 0b Load Packages
source(here::here("0_set_up", "1_libraries.R"))

# 0c Set up filepath(s)
data_path <- '/n/dominici_nsaph_l3/Lab/projects/wfpm_adrd_hosp/data/'
data_path_data4casecross <- "/n/dominici_nsaph_l3/Lab/projects/wfpm_adrd_hosp/data/data_process/data4casecross/"
data_path_stratification_vars <- '/n/dominici_nsaph_l3/Lab/projects/wfpm_adrd_hosp/data/data_process/stratification_vars/'
output_path <- paste0(print(here::here('Outputs')),'/')

# 0d Load datas for case-crossover analyses
# 7/5/23 - currently have lagged values for 
data_w_lagged_wfpm_2006 <- read_fst(paste0(data_path_data4casecross, "data_w_lagged_wfpm/data4casecross_2006.fst")) %>% 
  select(dayName, DayDateTime, id, starts_with(c("zip", "wfpm")))

data_w_lagged_nonwfpm_2006 <- read_fst(paste0(data_path_data4casecross, "data_w_lagged_nonwfpm/data4casecross_2006.fst")) %>% 
  select(DayDateTime, starts_with(c("zip", "pm25"))) 

data_w_lagged_temp_rh_2006 <- read_fst(paste0(data_path_data4casecross, "data_w_lagged_temp_rh/data4casecross_2006.fst")) %>% 
  select(DayDateTime, contains(c("zip", "temp", "rh"))) 

zip_count_hosp_outcomes_2006 <- read_fst(paste0(data_path_data4casecross, "zip_count_hosp_outcomes/by_adm_year/zip_count_hosp_outcomes_2006.fst")) %>% 
  rename(zip = zipcode_R,
         CaseDateTime = ADATE) %>% 
  select(CaseDateTime, starts_with(c("zip", "count"))) 

strat_pov_urbanicity <- read_csv(paste0(data_path_stratification_vars, "pov_ruca_zip.csv")) %>%
  mutate(urbanicity = ifelse(RUCA1 == 10, "rural", "urban/suburban")) %>% 
  select(-c(GEOID10, STATE, ZIP_TYPE, RUCA1, RUCA2)) %>% 
  select(zip, everything()) %>% 
  mutate(zip = as.integer(zip))

data4casecross_exp_out_conf <- left_join(data_w_lagged_wfpm_2006, data_w_lagged_nonwfpm_2006, by = c("zip", "DayDateTime")) %>% 
  left_join(., data_w_lagged_temp_rh_2006, by = c("zip", "DayDateTime")) %>% 
  left_join(., strat_pov_urbanicity, by = "zip") %>% 
  left_join(., zip_count_hosp_outcomes_2006, by = c("zip", 
                                                    "DayDateTime" = "CaseDateTime")) %>% 
  select(DayDateTime, id, zip, starts_with("count"), everything()) %>% 
  # replace the missing outcome variables with 0 because there are no hospitalizations for those days
  mutate(across(contains("count"), ~replace(., is.na(.), 0))) 

# read in sample dataset 20%
sample_data_20pct <- read_fst('/n/dominici_nsaph_l3/Lab/projects/wfpm_adrd_hosp/data/data_process/samp_data/samp_data_20pct.fst')

####************************************************************
#### 1: Calculate summary stats for use in DLNM crosspreds  #### 
####************************************************************

# 1a Create vector of observed daily wf pm concentrations 
alldata <- data4casecross_exp_out_conf %>%
  dplyr::select(contains('wfpm_lag'))
alldata <- as.matrix(alldata)[,1:6]
obs_wfpm <- as.vector(alldata)

# 1b Calculate summary statistics
daily_wfpm.mean <- mean(obs_wfpm, na.rm = TRUE)
daily_wfpm.sd <- sd(obs_wfpm, na.rm = TRUE)
daily_wfpm.min <- min(obs_wfpm, na.rm = TRUE)
daily_wfpm.max <- max(obs_wfpm, na.rm = TRUE)
daily_wfpm.per05 <- quantile(obs_wfpm, 0.05, type = 1, na.rm = TRUE)
daily_wfpm.per95 <- quantile(obs_wfpm, 0.95, type = 1, na.rm = TRUE)


####**************************************************************************
#### 2: Create counterfactual exposure vector for use in DLNM crosspreds  #### 
####**************************************************************************

# 2a Create counterfactual exposure vector
# Notes: We use this exposure vector for creating the estimates from the dlnm model. 
#        These are the counterfactual exposure levels, and we generate effect estimates
#        for a contrast between the reference level (the mean) and these levels. 
#        We likely won't need all of these, but making a big exposure contrast table 
#        makes it much easier to deal with any changes to analysis, e.g., reporting a different effect estimate
expContrasts <- data.frame(  
  Counterfactual_wfpm = c(seq(daily_wfpm.min, daily_wfpm.max, length.out = 100), 
                          quantile(obs_wfpm, 0.01, type = 1, na.rm = T), quantile(obs_wfpm, 0.99, type = 1, na.rm = T), 
                          quantile(obs_wfpm, 0.05, type = 1, na.rm = T), quantile(obs_wfpm, 0.95, type = 1, na.rm = T), 
                          quantile(obs_wfpm, 0.10, type = 1, na.rm = T), quantile(obs_wfpm, 0.90, type = 1, na.rm = T),  
                          quantile(obs_wfpm, 0.15, type = 1, na.rm = T), quantile(obs_wfpm, 0.85, type = 1, na.rm = T),
                          quantile(obs_wfpm, 0.20, type = 1, na.rm = T), quantile(obs_wfpm, 0.80, type = 1, na.rm = T), 
                          quantile(obs_wfpm, 0.25, type = 1, na.rm = T), quantile(obs_wfpm, 0.75, type = 1, na.rm = T), 
                          daily_wfpm.mean - daily_wfpm.sd,  daily_wfpm.mean + daily_wfpm.sd, 
                          daily_wfpm.mean - 10,  daily_wfpm.mean + 10),
  Label = c(rep('ERValues', 100), 'per01','per99', 'per05', 'per95', 'per10', 'per90',
            'per15', 'per85', 'per20', 'per80', 'per25', 'per75', 'MeanMinusSD', 'MeanPlusSD', 
            'MeanMinus10', 'MeanPlus10')) %>% 
  mutate(Counterfactual_wfpm = round(Counterfactual_wfpm, 7))

# 2b Clean up 
rm(obs_wfpm, alldata)


####********************************************************
#### 3: Add case and time-to-event variables for models #### 
####********************************************************

# 3a Add needed vars for city analysis
data4casecross_exp_out_conf <- data4casecross_exp_out_conf %>% 
  mutate(Case = ifelse(dayName == 'Caseday_0', 1, 0)) # bivariate Case variable

tmp <- data4casecross_exp_out_conf %>% 
  filter(zipcode_R == 10001)


# NEXT STEP: JOIN COUNT HOSP DATA -----------------------------------------
# note: different size data because some control days do not have any hospitalization 
# but check that every case day has a matching observation in COUNT HOSP DATa

count_hosp_2006 <- read_fst(paste0(data_path, "data_process/zip_count_hosp_outcomes/zip_count_hosp_outcomes_2006.fst"))
tmp2 <- count_hosp_2006 %>% 
  filter(zipcode_R == 10001)



# Notes for addtl analyses ------------------------------------------------
# Below we would want to subset to appropriate outcomes (e.g., indicator for cvd, resp, mh)
# # 3e Add needed vars for NYC only secondary analysis
# data4casecross_NYC <- data4casecross_exp_out_conf %>% 
#   filter(city == 'New York') %>% 
#   mutate(Case = ifelse(dayName == 'CaseDay_0', 1, 0),         # bivariate Case variable
#          TimetoEvent = if_else(dayName == 'CaseDay_0', 1, 2)) # bivariate Case variable for coxph model (if using instead of clogit)    
# 
# # 3d Add needed vars for MI_count_DXA410X1 sens analysis
# data4casecross_DXA410X1 <- data4casecross_exp_out_conf %>% 
#   filter(MI_count_DXA410X1 == 1 | MI_count_DXA410X1 == 2 | 
#            MI_count_DXA410X1 == 3 | MI_count_DXA410X1 == 4 |
#            MI_count_DXA410X1 == 5 | MI_count_DXA410X1 == 6 | 
#            MI_count_DXA410X1 == 7) %>% 
#   mutate(Case = ifelse(dayName == 'CaseDay_0', 1, 0),         # bivariate Case variable
#          TimetoEvent = if_else(dayName == 'CaseDay_0', 1, 2)) # bivariate Case variable for coxph model (if using instead of clogit)    



####****************************************************************************** BEGIN FUNCTION
# Here I edit functions developed by Sebastian Rowland


####*******************************************************
#### 4: Create function to conduct dlnm case-crossover #### 
####*******************************************************

# 4a Name function
analyze_dlnm_wfpm <- function(ExpTerm, CaseType, Sensitivity,  
                              ERConstraint, LRConstraint, SaveModel, dta){
  ExpTerm <- 'daily_wfpm'; CaseType <- 'count_ADRD_any';
  Sensitivity <- 'Main'; ERConstraint <- '4dfevenknots'; LRConstraint <- '3dfevenknots';
  SaveModel <- 'StoreAIC'; dta <- data4casecross_exp_out_conf
  
  # 4b Create ModelName
  ModelIdentifier <- paste0(ExpTerm, '_', CaseType, '_', Sensitivity)
  ExpConstraints <- paste0('ER', ERConstraint, '_LR', LRConstraint)
  ModelName <- paste0(ModelIdentifier,'_', ExpConstraints)
  
  # 4c Determine number of lag hours to include 
  # if(str_detect(Sensitivity, 'Main') | str_detect(Sensitivity, 'NYC')
  #    | str_detect(Sensitivity, 'NoRH') | str_detect(Sensitivity, 'Zips')
  #    | str_detect(Sensitivity, 'MI_count_DXA410X1') | str_detect(Sensitivity, 'notNYC')
  #    | str_detect(Sensitivity, '1415NoPM') | str_detect(Sensitivity, '1415PM')){NumLag <- 24} 
  # if(str_detect(Sensitivity, '48Lags')){NumLag <- 48}
  NumLag <- 6
  
  
  ####***************************
  #### 5: Create cross basis #### 
  ####***************************
  
  
  # 5a Set ER (exposure response) and LR (lagged response) constraints
  ERdf <- as.numeric(str_remove_all(ERConstraint, '[A-z]')) # Remove all letters, leaving only number of df (degrees of freedom)
  LRdf <- as.numeric(str_remove_all(LRConstraint, '[A-z]'))
  
  # 5b Create cross basis for temperature
  cb.hrlytemp <- crossbasis(
    as.matrix(dplyr::select(dta, contains('temp_lag')))[,1:NumLag], 
    lag=c(0,(NumLag-1)),
    argvar=list(fun='ns', df = 3),
    arglag=list(fun='ns', df = 4))
  
  # 5c Create cross basis for relative humidity
  cb.hrlyrh <- crossbasis(
    as.matrix(dplyr::select(dta, contains('rh_lag')))[,1:NumLag], 
    lag=c(0,(NumLag-1)),
    argvar=list(fun='ns', df = 3),
    arglag=list(fun='ns', df = 4))
  
  # 5d Create cross basis for wfpm (~ 1min)
  if(str_detect(ERConstraint, 'evenknots') & str_detect(LRConstraint, 'evenknots')){
    cb.hrlyNO2 <- crossbasis(
      as.matrix(dplyr::select(dta, contains('wfpm_lag')))[,1:NumLag], 
      lag=c(0,(NumLag-1)),
      argvar=list(fun='ns', df = ERdf),
      arglag=list(fun='ns', df = LRdf))}
  
  if(str_detect(ERConstraint, 'lin') & str_detect(LRConstraint, 'evenknots')){
    cb.hrlyNO2 <- crossbasis(
      as.matrix(dplyr::select(dta, contains('wfpm_lag')))[,1:NumLag], 
      lag=c(0,(NumLag-1)),
      argvar=list(fun='lin'),
      arglag=list(fun='ns', df = LRdf))}
  
  # 5e Create cross basis for nonwfpm 
  if(str_detect(Sensitivity, 'nonwfpm')){ #check to see if we need something here
    cb.hrlyPM25 <- crossbasis(
      as.matrix(dplyr::select(dta, contains('nonwfpm_lag')))[,1:NumLag], 
      lag=c(0,(NumLag-1)),
      argvar=list(fun='lin'),
      arglag=list(fun='ns', df = LRdf))}
  
  
  ####****************************
  #### 6: Create health model #### 
  ### 7/5/23 WORK ON THIS
  ####****************************
  
  # 6a Health model for main analysis, NYC sub analysis, zip code sens analysis,
  #    48 hr lag sens analysis, all but NYC sub analysis, and 2014-2015 NYC analysis
  #    (conducted to compare with 2014-2015 NYC adjusted for PM analysis) 
  if(str_detect(Sensitivity, 'Main') | str_detect(Sensitivity, 'NYC')
     | str_detect(Sensitivity, 'Zips') | str_detect(Sensitivity, '48Lags')
     | str_detect(Sensitivity, 'notNYC') | str_detect(Sensitivity, '1415NoPM')){
    mod <- coxph(Surv(TimetoEvent, Case) ~ cb.hrlyNO2 +
                   #ns(temp, df = 3) + 
                   #ns(rh, df = 3) +
                   cb.hrlytemp +
                   cb.hrlyrh +
                   strata(id),              # each case id is a strata
                 weights = MI_count_Prim,   # num of events in each hour (could only use if exposure is same for all cases/controls in a given hour)
                 method = "efron",          # the method tells the model how to deal with ties
                 data = dta)}
  
  # 6b Health model for No Rel Hum sens analysis
  if(str_detect(Sensitivity, 'NoRH')){
    mod <- coxph(Surv(TimetoEvent, Case) ~ cb.hrlyNO2 + 
                   cb.hrlytemp +
                   strata(id),              # each case id is a strata
                 weights = MI_count_Prim,   # num of events in each hour (could only use if exposure is same for all cases/controls in a given hour)
                 method = "efron",          # the method tells the model how to deal with ties
                 data = dta)}
  
  # 6c Health model for MI_count_DXA410X1 sens analysis
  if(str_detect(Sensitivity, 'MI_count_DXA410X1')){
    mod <- coxph(Surv(TimetoEvent, Case) ~ cb.hrlyNO2 + 
                   cb.hrlytemp +
                   cb.hrlyrh +
                   strata(id),                  # each case id is a strata
                 weights = MI_count_DXA410X1,   # num of events in each hour (could only use if exposure is same for all cases/controls in a given hour)
                 method = "efron",              # the method tells the model how to deal with ties
                 data = dta)}
  
  # 6d Health model for 2014-2015 NYC PM2.5 sens analysis 
  if(str_detect(Sensitivity, '1415PM')){
    mod <- coxph(Surv(TimetoEvent, Case) ~ cb.hrlyNO2 +
                   #ns(temp, df = 3) + 
                   #ns(rh, df = 3) +
                   cb.hrlytemp +
                   cb.hrlyrh +
                   cb.hrlyPM25 +
                   strata(id),              # each case id is a strata
                 weights = MI_count_Prim,   # num of events in each hour (could only use if exposure is same for all cases/controls in a given hour)
                 method = "efron",          # the method tells the model how to deal with ties
                 data = dta)}
  
  
  ####***********************************************
  #### 7: Save model and create crosspredictions #### 
  ####***********************************************
  
  # 7a Begin option  
  if(str_detect(SaveModel, 'SaveModel')){
    
    # 7b Save the model 
    mod %>% saveRDS(paste0(output_path, 'Models/', ModelName, '.RDS'))
    
    # 7c Begin option for nonlinear ER and LR 
    if(str_detect(ERConstraint, 'evenknots') & str_detect(LRConstraint, 'evenknots')){ 
      
      # 7d Generate estimates for nonlinear ER and LR   
      # the cen argument sets the reference exposure level for our effect estimates (set to the mean NO2 value)
      # crosspred() will yield estimates for 100 exposure levels plus those exposure levels we set
      est <- crosspred(cb.hrlyNO2,                                   # exposure crossbasis
                       mod,                                          # health model
                       cen = daily_wfpm.mean,                         # center at mean NO2
                       at = expContrasts$Counterfactual_wfpm,          # compute estimated association for each integer value of NO2 in Counterfactual_wfpm vector
                       cumul = TRUE,                                 # also compute cumulative associations
                       bylag = 1)                                    # estimates association along each lag
      
      # 7e Extract coefficient fit and CI 
      fit.table <- as.data.frame(est$matRRfit)  
      colnames(fit.table) <- paste0('fit.or.', colnames(fit.table))
      fit.table <- fit.table %>%  
        mutate(Counterfactual_wfpm = as.numeric(row.names(fit.table)))
      
      lci.table <- as.data.frame(est$matRRlow)  
      colnames(lci.table) <- paste0('lci.or.', colnames(lci.table))
      
      uci.table <- as.data.frame(est$matRRhigh)  
      colnames(uci.table) <- paste0('uci.or.', colnames(uci.table))
      
      # 7f Combine fit and se for individual lags 
      #    Notes: All OR are relative to the mean NO2 
      est.table <- bind_cols(fit.table, lci.table, uci.table)
      
      # 7g Attach the labels of the exposure contrasts
      est.table <- est.table %>% full_join(expContrasts, by = 'Counterfactual_wfpm')
      
      # 7h Save estimate table for individual lags 
      est.table %>%
        write.csv(paste0(output_path, 'Estimates/', 
                         'EstInd_', ModelName, '.csv'))
      
      # 7i Extract cumulative coefficient fit and ci  
      fit.table <- as.data.frame(est$cumRRfit)  
      colnames(fit.table) <- paste0('fit.or.', colnames(fit.table))
      fit.table <- fit.table %>%  
        mutate(Counterfactual_wfpm = as.numeric(row.names(fit.table)))
      
      lci.table <- as.data.frame(est$cumRRlow)  
      colnames(lci.table) <- paste0('lci.or.', colnames(lci.table))
      
      uci.table <- as.data.frame(est$cumRRhigh)  
      colnames(uci.table) <- paste0('uci.or.', colnames(uci.table))
      
      # 7j Combine fit and se for individual lags 
      #    Notes: All OR are relative to the mean NO2  
      est.table <- bind_cols(fit.table, lci.table, uci.table)
      
      # 7k Attach the labels of the exposure contrasts
      est.table <- est.table %>% full_join(expContrasts, by = 'Counterfactual_wfpm')
      
      # 7l Save cumulative estimates table 
      est.table %>% 
        write.csv(paste0(output_path, 'Estimates/', 
                         'EstCumul_', ModelName, '.csv'))
    }
    
    # 7m Begin option for linear ER and nonlinear LR 
    if(str_detect(ERConstraint, 'lin') & str_detect(LRConstraint, 'evenknots')){ 
      
      # 7n Generate estimates for nonlinear ER and LR   
      est <- crosspred(cb.hrlyNO2,                    # exposure crossbasis
                       mod,                           # health model
                       at = 0:daily_wfpm.max,          # compute estimated association for each integer value of NO2
                       cumul = TRUE,                  # also compute cumulative associations
                       bylag = 1)                     # estimates association along each lag
      
      # 7o Extract coefficient fit and CI 
      fit.table <- as.data.frame(est$matRRfit)  
      colnames(fit.table) <- paste0('fit.or.', colnames(fit.table))
      fit.table <- fit.table %>%  
        mutate(Counterfactual_wfpm = as.numeric(row.names(fit.table)))
      
      lci.table <- as.data.frame(est$matRRlow)  
      colnames(lci.table) <- paste0('lci.or.', colnames(lci.table))
      
      uci.table <- as.data.frame(est$matRRhigh)  
      colnames(uci.table) <- paste0('uci.or.', colnames(uci.table))
      
      # 7p Combine fit and se for individual lags 
      #    Notes: All OR are relative 0 ppb
      est.table <- bind_cols(fit.table, lci.table, uci.table)
      
      # 7q Save estimate table for individual lags 
      est.table %>%
        write.csv(paste0(output_path, 'Estimates/', 
                         'EstInd_', ModelName, '.csv'))
      
      # 7r Extract cumulative coefficient fit and ci  
      fit.table <- as.data.frame(est$cumRRfit)  
      colnames(fit.table) <- paste0('fit.or.', colnames(fit.table))
      fit.table <- fit.table %>%  
        mutate(Counterfactual_wfpm = as.numeric(row.names(fit.table)))
      
      lci.table <- as.data.frame(est$cumRRlow)  
      colnames(lci.table) <- paste0('lci.or.', colnames(lci.table))
      
      uci.table <- as.data.frame(est$cumRRhigh)  
      colnames(uci.table) <- paste0('uci.or.', colnames(uci.table))
      
      # 7s Combine fit and se for individual lags 
      #    Notes: All OR are relative to the mean NO2  
      est.table <- bind_cols(fit.table, lci.table, uci.table)
      
      # 7t Save cumulative estimates table 
      est.table %>% 
        write.csv(paste0(output_path, 'Estimates/', 
                         'EstCumul_', ModelName, '.csv'))
    }
    
  } 
  
  ####***************************************
  #### 8: Save model AIC for grid search #### 
  ####***************************************
  
  if(str_detect(SaveModel, 'StoreAIC')){
    
    # 8a Readin the table of AIC's
    aic.table <- read_csv(paste0(output_path, 'Tables/', 'Model_AIC.csv'), 
                          col_types = 'cccdT')
    
    # 8b Add this aic to the set of AIC's
    aic.table[1+nrow(aic.table),] <- list(ModelIdentifier,
                                          ERConstraint, LRConstraint, AIC(mod), Sys.time())
    
    # 8c Remove any old AICs and then save
    # at the slice step you keep only the earliest AIC for each model-constraint combo
    aic.table %>% 
      group_by(ModelIdentifier,ERConstraint, LRConstraint) %>% 
      arrange(desc(RunDate)) %>% 
      slice(0:1) %>% 
      filter(!is.na(ModelIdentifier)) %>%
      write_csv(paste0(output_path, 'Tables/', 'Model_AIC.csv'))
  }
  
}

####****************************************************************************** END FUNCTION


####**************************************************
#### 9: Grid search to determine df for ER and lR #### 
####**************************************************

# Notes: For the main model, a linear ER constraint and a 4df LR constraint was identified
#        For the 48-hour lag, we assumed a linear ER constraint, since this should not
#          change because of adding additional lags. A 5df LR constraint was identified

#        The grid search may take 3-5 min to run for each model type

# 9a Set dlnm / case-crossover function inputs
# 9a.i Main model
ExpTerm <- 'daily_wfpm'
CaseType <- 'MIcountPrim'
Sensitivity <- 'Main' 
SaveModel <- 'StoreAIC'
dta <- data4casecross_exp_out_conf
ModelIdentifier <- paste0(ExpTerm, '_', CaseType, '_', Sensitivity)
# 9a.ii 48-hour lag model
ExpTerm <- 'daily_wfpm'
CaseType <- 'MIcountPrim'
Sensitivity <- '48Lags' 
SaveModel <- 'StoreAIC'
dta <- data4casecross_48lags
ModelIdentifier <- paste0(ExpTerm, '_', CaseType, '_', Sensitivity)

# 9b Build constraint grid
# 9b.i Main model
CandidateConstraintsGrid <- data.frame(
  ERConstraint = rep(c('lin', '3dfevenknots','4dfevenknots', '5dfevenknots'), 3), 
  LRConstraint = c(rep('4dfevenknots',4), rep('5dfevenknots',4), rep('6dfevenknots',4)))
# 9b.ii 48-hour lag model
CandidateConstraintsGrid <- data.frame(
  ERConstraint = rep(c('lin'), 4), 
  LRConstraint = c(rep('4dfevenknots',1), rep('5dfevenknots',1), rep('6dfevenknots',1),
                   rep('7dfevenknots',1)))

# 9c Initialize loop over grid cells
for(i in 1:nrow(CandidateConstraintsGrid)){
  # i <- 1
  # Fit model with candidate constraints
  analyze_dlnm_wfpm(ExpTerm, CaseType, Sensitivity, 
                    CandidateConstraintsGrid$ERConstraint[i], CandidateConstraintsGrid$LRConstraint[i], 
                    SaveModel, dta)
}

# 9d Read in table of model AICs
aic.table0 <- read_csv(paste0(output_path, 'Tables/', 'Model_AIC.csv'), 
                       col_types = 'cccdT')

# 9e Filter by model identifier 
aic.table <- aic.table0 %>% filter(ModelIdentifier == !!ModelIdentifier) 

# 9f Find minAIC
AIC.min <- min(aic.table$AIC)

# 9g Identify selected model
SelectedModel <- aic.table %>% 
  filter(AIC == AIC.min) 

# 9h Remove old terms from environment
rm(ExpTerm, CaseType, Sensitivity, SaveModel, dta, ModelIdentifier,
   aic.table, aic.table0, CandidateConstraintsGrid, AIC.min)


####*******************************************
#### 10: Run main and sensitivity analyses #### 
####*******************************************

# after best parameters determined, actually run the health model

# 10a Run main city-level analysis (~ 1min)
#    Notes: Model use -- analyze_dlnm_wfpm(ExpTerm, CaseType, Sensitivity, ERConstraint, 
#                                LRConstraint, SaveModel, dta)
#           Example arguments -- ExpTerm <- 'daily_wfpm'; CaseType <- 'MIcountPrim'; 
#                                Sensitivity <- 'Main'; 
#                                ERConstraint <- '4dfevenknots'; LRConstraint <- '3dfevenknots';
#                                SaveModel <- 'SaveModel'; dta <- data4casecross_exp_out_conf
analyze_dlnm_wfpm('daily_wfpm', 'MIcountPrim', 'Main', 'lin', '4dfevenknots', 
                  'SaveModel', data4casecross_exp_out_conf)

# 10b Run secondary NYC alone analysis
analyze_dlnm_wfpm('daily_wfpm', 'MIcountPrim', 'NYC', 'lin', '4dfevenknots',
                  'SaveModel', data4casecross_NYC)

# 10c Run sensitivity analysis: no relative humidity
analyze_dlnm_wfpm('daily_wfpm', 'MIcountPrim', 'NoRH', 'lin', '4dfevenknots',
                  'SaveModel', data4casecross_exp_out_conf)

# 10d Run sensitivity analysis: zip codes
analyze_dlnm_wfpm('daily_wfpm', 'MIcountPrim', 'Zips', 'lin', '4dfevenknots',
                  'SaveModel', data4casecross_zip)

# 10e Run sensitivity analysis: MI in the first diagnostic position only
analyze_dlnm_wfpm('daily_wfpm', 'MIcountDXA410X1', 'MI_count_DXA410X1', 'lin', '4dfevenknots',
                  'SaveModel', data4casecross_DXA410X1)

# 10f Run sensitivity analysis: 48 lags
analyze_dlnm_wfpm('daily_wfpm', 'MIcountPrim', '48Lags', 'lin', '5dfevenknots',
                  'SaveModel', data4casecross_48lags)

# 10g Run secondary all but NYC analysis
analyze_dlnm_wfpm('daily_wfpm', 'MIcountPrim', 'notNYC', 'lin', '4dfevenknots',
                  'SaveModel', data4casecross_NotNYC)

# 10h Run sensitivity analysis: 2014-2015 NYC not adjusted for hourly PM2.5
#     Note: this analysis is only run to compare with the model that is 
#           adjusted for hourly PM2.5
analyze_dlnm_wfpm('daily_wfpm', 'MIcountPrim', '1415NoPM', 'lin', '4dfevenknots',
                  'SaveModel', data4casecross_1415nyc)

# 10i Run sensitivity analysis: 2014-2015 NYC adjusted for hourly PM2.5
analyze_dlnm_wfpm('daily_wfpm', 'MIcountPrim', '1415PM', 'lin', '4dfevenknots',
                  'SaveModel', data4casecross_1415nyc)




