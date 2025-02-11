# Generate a subsample of the dataset we will use for gridsearch to obtain df in non linear meterological terms
# Vivian Do
# 7/14/23

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
output_path <- paste0(data_path, "output")

# 0d Load datas for case-crossover analyses
# year <- 2006
gen_data4casecross_year <- function(year){
  data_w_lagged_wfpm <- read_fst(paste0(data_path_data4casecross, "data_w_lagged_wfpm/data4casecross_", year, ".fst")) %>% 
    select(dayName, CaseDateTime, DayDateTime, id, starts_with(c("zip", "wfpm")))
  
  data_w_lagged_temp_rh <- read_fst(paste0(data_path_data4casecross, "data_w_lagged_temp_rh/data4casecross_", year, ".fst")) %>% 
    select(DayDateTime, contains(c("zip", "temp", "rh"))) 
  
  zip_count_hosp_outcomes <- read_fst(paste0(data_path_data4casecross, "zip_count_hosp_outcomes/by_adm_year/zip_count_hosp_outcomes_", year, ".fst")) %>% 
    rename(zip = zipcode_R,
           CaseDateTime = ADATE) %>% 
    select(CaseDateTime, starts_with(c("zip", "count"))) 
  
  samp_data_25pct <- left_join(data_w_lagged_wfpm, data_w_lagged_temp_rh, by = c("zip", "DayDateTime")) %>% 
    left_join(., zip_count_hosp_outcomes, by = c("zip",
                                                 "CaseDateTime" = "CaseDateTime")) %>% # merge by CaseDateTime (bc we want the weights to be the daily case numbers)
    select(DayDateTime, id, zip, starts_with("count"), everything()) %>% 
    # replace the missing outcome variables with 0 because there are no hospitalizations for those days
    mutate(across(contains("count"), ~replace(., is.na(.), 0))) 

}

keep_data_w_all_temp_rh <- function(data4casecross_year){
  # keep only id sets with full confounder data by excluding ids with missing confounder data
  ids_w_missing_temp_rh <- data4casecross_year %>% 
    select(DayDateTime, id, zip, contains(c("temp", "rh")))
  ids_w_missing_temp_rh$number_na <- rowSums(is.na(ids_w_missing_temp_rh)) 
  ids_w_missing_temp_rh <- ids_w_missing_temp_rh %>% 
    group_by(id) %>% 
    mutate(ind_missing = ifelse(sum(number_na) >= 1, 1, 0)) %>% 
    filter(row_number() == 1,
           ind_missing == 1) %>% 
    select(id)
  
  data4casecross_year <- data4casecross_year %>% 
    filter(!id %in% ids_w_missing_temp_rh$id)
}

keep_data_w_all_wfpm <- function(data4casecross_year){
  # keep only id sets with full exposure data by excluding ids with missing exposure data
  ids_w_missing_wfpm <- data4casecross_year %>% 
    select(DayDateTime, id, zip, contains(c("wfpm")))
  ids_w_missing_wfpm$number_na <- rowSums(is.na(ids_w_missing_wfpm)) 
  ids_w_missing_wfpm <- ids_w_missing_wfpm %>% 
    group_by(id) %>% 
    mutate(ind_missing = ifelse(sum(number_na) >= 1, 1, 0)) %>% 
    filter(row_number() == 1,
           ind_missing == 1) %>% 
    select(id)
  
  data4casecross_year <- data4casecross_year %>% 
    filter(!id %in% ids_w_missing_wfpm$id)
}

rand_select_25pct <- function(data4casecross_year){
  # randomly select 25% of unique ids from each year (so we get 25% of all data)
  set.seed(888)
  selected_ids <- sample(1:max(data4casecross_year$id),
                         size = 0.25*max(data4casecross_year$id),
                         replace = FALSE)
  
  samp_25pct <- data4casecross_year %>% 
    filter(id %in% selected_ids)
}

years <- c(2006:2016)
samp_data <- list()
for (year in 1:length(years)){
  
  print(years[[year]])
  data_year <- gen_data4casecross_year(years[[year]])
  data_year_no_missing_conf <- keep_data_w_all_temp_rh(data_year)
  data_year_no_missing_conf_exp <- keep_data_w_all_wfpm(data_year_no_missing_conf)
  data_year_no_missing_25pct <- rand_select_25pct(data_year_no_missing_conf_exp)

  samp_data[[year]] <- data_year_no_missing_25pct
  
}

samp_data_25pct <- bind_rows(samp_data) %>% 
  select(-CaseDateTime) %>% 
  write_fst(., '/n/dominici_nsaph_l3/Lab/projects/wfpm_adrd_hosp/data/data_process/samp_data/samp_data_25pct.fst')



# delete later -------------------------------------------------------------


####********************************************************
#### 3: Add case and time-to-event variables for models #### 
####********************************************************

# read in sample data
samp_data_25pct <- read_fst('/n/dominici_nsaph_l3/Lab/projects/wfpm_adrd_hosp/data/data_process/samp_data/samp_data_25pct.fst')

# 3a Add needed vars for  analysis
samp_data_25pct <- samp_data_25pct %>% 
  mutate(Case = ifelse(dayName == 'Caseday_0', 1, 0), # bivariate Case variable
         TimetoEvent = ifelse(dayName == 'Caseday_0', 1, 2)) # bivariate Case variable for coxph model (if using instead of clogit) 


####****************************************************************************** BEGIN FUNCTION
# Here I edit functions developed by Sebastian Rowland


####*******************************************************
#### 4: Create function to conduct dlnm case-crossover #### 
####*******************************************************

# 4a Name function
analyze_dlnm_wfpm <- function(ExpTerm, CaseType, Sensitivity,  
                              ERConstraint, LRConstraint, SaveModel, dta){
  # ExpTerm <- 'wfpm'; CaseType <- 'count_ADRD_any';
  # Sensitivity <- 'Main'; ERConstraint <- '4dfevenknots'; LRConstraint <- '3dfevenknots'; #ERConstraint <- '4dfevenknots'
  # SaveModel <- 'StoreAIC'; dta <- samp_data_25pct
  
  # 4b Create ModelName
  ModelIdentifier <- paste0(ExpTerm, '_', CaseType, '_', Sensitivity)
  ExpConstraints <- paste0('ER', ERConstraint, '_LR', LRConstraint)
  ModelName <- paste0(ModelIdentifier,'_', ExpConstraints)
  NumLag <- 6
  
  
  ####***************************
  #### 5: Create cross basis #### 
  ####***************************
  
  # 5a Set ER (exposure response) and LR (lagged response) constraints
  ERdf <- as.numeric(str_remove_all(ERConstraint, '[A-z]')) # Remove all letters, leaving only number of df (degrees of freedom)
  LRdf <- as.numeric(str_remove_all(LRConstraint, '[A-z]'))
  
  # 5b Create cross basis for temperature
  cb.hrlytemp <- crossbasis(
    as.matrix(dplyr::select(dta, contains('temp')))[,1:NumLag], 
    lag=c(0,(NumLag-1)),
    argvar=list(fun='ns', df = 3),
    arglag=list(fun='ns', df = LRdf))
  
  # 5c Create cross basis for relative humidity
  cb.hrlyrh <- crossbasis(
    as.matrix(dplyr::select(dta, contains('rh')))[,1:NumLag], 
    lag=c(0,(NumLag-1)),
    argvar=list(fun='ns', df = 3),
    arglag=list(fun='ns', df = LRdf))
  
  # 5d Create cross basis for wfpm 
  if(str_detect(ERConstraint, 'evenknots') & str_detect(LRConstraint, 'evenknots')){
    cb.wfpm <- crossbasis(
      as.matrix(dplyr::select(dta, contains('wfpm')))[,1:NumLag], 
      lag=c(0,(NumLag-1)),
      argvar=list(fun='ns', df = ERdf),
      arglag=list(fun='ns', df = LRdf))}
  
  # if(str_detect(ERConstraint, 'lin') & str_detect(LRConstraint, 'evenknots')){
  #   cb.wfpm <- crossbasis(
  #     as.matrix(dplyr::select(dta, contains('wfpm')))[,1:NumLag], 
  #     lag=c(0,(NumLag-1)),
  #     argvar=list(fun='lin'),
  #     arglag=list(fun='ns', df = LRdf))}
  # 
  # # 5e Create cross basis for nonwfpm (sensitivity analysis)
  # if(str_detect(Sensitivity, 'nonwfpm')){ #check to see if we need something here
  #   cb.nonwfpm <- crossbasis(
  #     as.matrix(dplyr::select(dta, contains('nonwfpm_lag')))[,1:NumLag], 
  #     lag=c(0,(NumLag-1)),
  #     argvar=list(fun='lin'),
  #     arglag=list(fun='ns', df = LRdf))}
  
  
  ####****************************
  #### 6: Create health model #### 
  ### 7/5/23 WORK ON THIS
  ####****************************
  
  # 6a Health model for main analysis ("Main") 
  # 7/17/23 --> are there other main analyses?
  # should loop through all the health outcomes
  if(str_detect(Sensitivity, 'Main')){
    
    # try clogit
    mod <- clogit(Case ~ cb.wfpm +
                    # ns(max_temp_kelvin_00, df = 3) +      
                    # ns(min_rh_00, df = 3) +               
                    cb.hrlytemp +                        # LR and ER built in
                    cb.hrlyrh +                          # LR and ER built in
                    strata(id),
                  weights = count_ADRD_any,        # num of daily events (could only use if exposure is same for all cases/controls in a given hour); set this to be our outcome of interest, which can change by input!
                  method = "efron",
                  data = dta)
    }
    # # try survival analysis
    # mod <- coxph(Surv(TimetoEvent, Case) ~ cb.wfpm +
    #                 ns(max_temp_kelvin_00, df = 3) +      # ER
    #                 ns(min_rh_00, df = 3) +               # ER
    #                 cb.hrlytemp +                         # LR
    #                 cb.hrlyrh +                          # LR
    #                 strata(id),
    #               weights = count_ADRD_any,        # num of daily events (could only use if exposure is same for all cases/controls in a given hour); set this to be our outcome of interest, which can change by input!
    #               method = "efron",
    #               data = dta)
    # an error from the above code
    # Error in coxph.fit(X, Y, istrat, offset, init, control, weights = weights,  : 
    # Invalid weights, must be >0
    # 
    # mod <- coxph(Surv(TimetoEvent, Case) ~ cb.wfpm +
    #                ns(temp, df = 3) +
    #                ns(rh, df = 3) +
    #                cb.hrlytemp +
    #                cb.hrlyrh +
    #                strata(id),              # each case id is a strata
    #              weights = CaseType,   # num of events in each hour (could only use if exposure is same for all cases/controls in a given hour)
    #              method = "efron",          # the method tells the model how to deal with ties
    #              data = dta)}
  # 
  # # 6b Health model for No Rel Hum sens analysis
  # if(str_detect(Sensitivity, 'NoRH')){
  #   mod <- coxph(Surv(TimetoEvent, Case) ~ cb.wfpm + 
  #                  cb.hrlytemp +
  #                  strata(id),              # each case id is a strata
  #                weights = CaseType,   # num of events in each hour (could only use if exposure is same for all cases/controls in a given hour)
  #                method = "efron",          # the method tells the model how to deal with ties
  #                data = dta)}
  # 
  # # 6c Health model for MI_count_DXA410X1 sens analysis
  # if(str_detect(Sensitivity, 'MI_count_DXA410X1')){
  #   mod <- coxph(Surv(TimetoEvent, Case) ~ cb.wfpm + 
  #                  cb.hrlytemp +
  #                  cb.hrlyrh +
  #                  strata(id),                  # each case id is a strata
  #                weights = MI_count_DXA410X1,   # num of events in each hour (could only use if exposure is same for all cases/controls in a given hour)
  #                method = "efron",              # the method tells the model how to deal with ties
  #                data = dta)}
  # 
  # # 6d Health model for 2014-2015 NYC PM2.5 sens analysis 
  # if(str_detect(Sensitivity, '1415PM')){
  #   mod <- coxph(Surv(TimetoEvent, Case) ~ cb.wfpm +
  #                  #ns(temp, df = 3) + 
  #                  #ns(rh, df = 3) +
  #                  cb.hrlytemp +
  #                  cb.hrlyrh +
  #                  cb.hrlyPM25 +
  #                  strata(id),              # each case id is a strata
  #                weights = CaseType,   # num of events in each hour (could only use if exposure is same for all cases/controls in a given hour)
  #                method = "efron",          # the method tells the model how to deal with ties
  #                data = dta)}
  # 
  # 
  # ####***********************************************
  # #### 7: Save model and create crosspredictions #### 
  # ####***********************************************
  # 
  # # 7a Begin option  
  # if(str_detect(SaveModel, 'SaveModel')){
  #   
  #   # 7b Save the model 
  #   mod %>% saveRDS(paste0(output_path, 'Models/', ModelName, '.RDS'))
  #   
  #   # 7c Begin option for nonlinear ER and LR 
  #   if(str_detect(ERConstraint, 'evenknots') & str_detect(LRConstraint, 'evenknots')){ 
  #     
  #     # 7d Generate estimates for nonlinear ER and LR   
  #     # the cen argument sets the reference exposure level for our effect estimates (set to the mean NO2 value)
  #     # crosspred() will yield estimates for 100 exposure levels plus those exposure levels we set
  #     est <- crosspred(cb.wfpm,                                   # exposure crossbasis
  #                      mod,                                          # health model
  #                      cen = daily_wfpm.mean,                         # center at mean NO2
  #                      at = expContrasts$Counterfactual_wfpm,          # compute estimated association for each integer value of NO2 in Counterfactual_wfpm vector
  #                      cumul = TRUE,                                 # also compute cumulative associations
  #                      bylag = 1)                                    # estimates association along each lag
  #     
  #     # 7e Extract coefficient fit and CI 
  #     fit.table <- as.data.frame(est$matRRfit)  
  #     colnames(fit.table) <- paste0('fit.or.', colnames(fit.table))
  #     fit.table <- fit.table %>%  
  #       mutate(Counterfactual_wfpm = as.numeric(row.names(fit.table)))
  #     
  #     lci.table <- as.data.frame(est$matRRlow)  
  #     colnames(lci.table) <- paste0('lci.or.', colnames(lci.table))
  #     
  #     uci.table <- as.data.frame(est$matRRhigh)  
  #     colnames(uci.table) <- paste0('uci.or.', colnames(uci.table))
  #     
  #     # 7f Combine fit and se for individual lags 
  #     #    Notes: All OR are relative to the mean NO2 
  #     est.table <- bind_cols(fit.table, lci.table, uci.table)
  #     
  #     # 7g Attach the labels of the exposure contrasts
  #     est.table <- est.table %>% full_join(expContrasts, by = 'Counterfactual_wfpm')
  #     
  #     # 7h Save estimate table for individual lags 
  #     est.table %>%
  #       write.csv(paste0(output_path, 'Estimates/', 
  #                        'EstInd_', ModelName, '.csv'))
  #     
  #     # 7i Extract cumulative coefficient fit and ci  
  #     fit.table <- as.data.frame(est$cumRRfit)  
  #     colnames(fit.table) <- paste0('fit.or.', colnames(fit.table))
  #     fit.table <- fit.table %>%  
  #       mutate(Counterfactual_wfpm = as.numeric(row.names(fit.table)))
  #     
  #     lci.table <- as.data.frame(est$cumRRlow)  
  #     colnames(lci.table) <- paste0('lci.or.', colnames(lci.table))
  #     
  #     uci.table <- as.data.frame(est$cumRRhigh)  
  #     colnames(uci.table) <- paste0('uci.or.', colnames(uci.table))
  #     
  #     # 7j Combine fit and se for individual lags 
  #     #    Notes: All OR are relative to the mean NO2  
  #     est.table <- bind_cols(fit.table, lci.table, uci.table)
  #     
  #     # 7k Attach the labels of the exposure contrasts
  #     est.table <- est.table %>% full_join(expContrasts, by = 'Counterfactual_wfpm')
  #     
  #     # 7l Save cumulative estimates table 
  #     est.table %>% 
  #       write.csv(paste0(output_path, 'Estimates/', 
  #                        'EstCumul_', ModelName, '.csv'))
  #   }
  #   
  #   # 7m Begin option for linear ER and nonlinear LR 
  #   if(str_detect(ERConstraint, 'lin') & str_detect(LRConstraint, 'evenknots')){ 
  #     
  #     # 7n Generate estimates for nonlinear ER and LR   
  #     est <- crosspred(cb.wfpm,                    # exposure crossbasis
  #                      mod,                           # health model
  #                      at = 0:daily_wfpm.max,          # compute estimated association for each integer value of NO2
  #                      cumul = TRUE,                  # also compute cumulative associations
  #                      bylag = 1)                     # estimates association along each lag
  #     
  #     # 7o Extract coefficient fit and CI 
  #     fit.table <- as.data.frame(est$matRRfit)  
  #     colnames(fit.table) <- paste0('fit.or.', colnames(fit.table))
  #     fit.table <- fit.table %>%  
  #       mutate(Counterfactual_wfpm = as.numeric(row.names(fit.table)))
  #     
  #     lci.table <- as.data.frame(est$matRRlow)  
  #     colnames(lci.table) <- paste0('lci.or.', colnames(lci.table))
  #     
  #     uci.table <- as.data.frame(est$matRRhigh)  
  #     colnames(uci.table) <- paste0('uci.or.', colnames(uci.table))
  #     
  #     # 7p Combine fit and se for individual lags 
  #     #    Notes: All OR are relative 0 ppb
  #     est.table <- bind_cols(fit.table, lci.table, uci.table)
  #     
  #     # 7q Save estimate table for individual lags 
  #     est.table %>%
  #       write.csv(paste0(output_path, 'Estimates/', 
  #                        'EstInd_', ModelName, '.csv'))
  #     
  #     # 7r Extract cumulative coefficient fit and ci  
  #     fit.table <- as.data.frame(est$cumRRfit)  
  #     colnames(fit.table) <- paste0('fit.or.', colnames(fit.table))
  #     fit.table <- fit.table %>%  
  #       mutate(Counterfactual_wfpm = as.numeric(row.names(fit.table)))
  #     
  #     lci.table <- as.data.frame(est$cumRRlow)  
  #     colnames(lci.table) <- paste0('lci.or.', colnames(lci.table))
  #     
  #     uci.table <- as.data.frame(est$cumRRhigh)  
  #     colnames(uci.table) <- paste0('uci.or.', colnames(uci.table))
  #     
  #     # 7s Combine fit and se for individual lags 
  #     #    Notes: All OR are relative to the mean NO2  
  #     est.table <- bind_cols(fit.table, lci.table, uci.table)
  #     
  #     # 7t Save cumulative estimates table 
  #     est.table %>% 
  #       write.csv(paste0(output_path, 'Estimates/', 
  #                        'EstCumul_', ModelName, '.csv'))
  #   }
  #   
  # } 
  
  ####***************************************
  #### 8: Save model AIC for grid search #### 
  ####***************************************
  
  if(str_detect(SaveModel, 'StoreAIC')){
    
    # 8a Readin the table of AIC's
    aic.table <- read_csv(paste0(output_path, '/tables/', 'Model_AIC.csv'), 
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
      write_csv(paste0(output_path, '/tables/', 'Model_AIC.csv'))
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
ExpTerm <- 'wfpm'
CaseType <- 'count_ADRD_any'
Sensitivity <- 'Main' 
SaveModel <- 'StoreAIC'
dta <- samp_data_25pct
ModelIdentifier <- paste0(ExpTerm, '_', CaseType, '_', Sensitivity)

# 9b Build constraint grid
# 9b.i Main model
CandidateConstraintsGrid <- data.frame(
  ERConstraint = rep(c('1dfevenknots','2dfevenknots', '3dfevenknots'), 3), 
  LRConstraint = c(rep('4dfevenknots', 3), rep('5dfevenknots', 3), rep('6dfevenknots', 3)))


# 9c Initialize loop over grid cells (~11 minutes)

start_time <- Sys.time()
for(i in 1:nrow(CandidateConstraintsGrid)){
  # i <- 1
  # Fit model with candidate constraints
  analyze_dlnm_wfpm(ExpTerm, CaseType, Sensitivity, 
                    CandidateConstraintsGrid$ERConstraint[i], CandidateConstraintsGrid$LRConstraint[i], 
                    SaveModel, dta)
}

end_time <- Sys.time()
end_time - start_time

# 9d Read in table of model AICs
aic.table0 <- read_csv(paste0(output_path, '/tables/', 'Model_AIC.csv'), 
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
#                                SaveModel <- 'SaveModel'; dta <- samp_data_25pct
analyze_dlnm_wfpm('daily_wfpm', 'MIcountPrim', 'Main', 'lin', '4dfevenknots', 
                  'SaveModel', samp_data_25pct)

# 10b Run secondary NYC alone analysis
analyze_dlnm_wfpm('daily_wfpm', 'MIcountPrim', 'NYC', 'lin', '4dfevenknots',
                  'SaveModel', data4casecross_NYC)

# 10c Run sensitivity analysis: no relative humidity
analyze_dlnm_wfpm('daily_wfpm', 'MIcountPrim', 'NoRH', 'lin', '4dfevenknots',
                  'SaveModel', samp_data_25pct)

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


