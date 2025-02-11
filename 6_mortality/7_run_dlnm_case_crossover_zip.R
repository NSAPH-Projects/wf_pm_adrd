# Conduct DLNM Case-crossover Analyses
# wf pm - hospitalizations
# primary hosp - adrd
# secondary hosp - cardiovascular, respiratory, mental health
# Vivian Do
# Updated 12/14/23

####********************
#### Preparation #### 
####********************
rm(list=ls())
source(here::here("0_set_up", "1_libraries.R"))

data_path <- '/n/dominici_nsaph_l3/Lab/projects/wfpm_adrd_hosp/data/'
data_path_data4casecross <- "/n/dominici_nsaph_l3/Lab/projects/wfpm_adrd_hosp/data/data_process_mortlity/data4casecross/"
data_path_stratification_vars <- '/n/dominici_nsaph_l3/Lab/projects/wfpm_adrd_hosp/data/data_process/stratification_vars/'
output_path <-"/n/dominici_nsaph_l3/Lab/projects/wfpm_adrd_hosp/data/output_mortality"

data4casecross_zip <- read_fst('/n/dominici_nsaph_l3/Lab/projects/wfpm_adrd_hosp/data/data_process_mortality/data4casecross/data_w_all_vars/data4casecross_zip.fst')
names(data4casecross_zip)

####************************************************************
#### Limit analyses to April - October  #### 
####************************************************************
data4casecross_zip <- data4casecross_zip %>% 
  filter(month(DayDateTime) %in% c(4:10))


####************************************************************
#### Calculate summary stats for use in DLNM crosspreds  #### 
####************************************************************

# Create vector of observed daily wf pm concentrations 
alldata <- data4casecross_zip %>%
  dplyr::select(contains('wfpm_lag'))
alldata <- as.matrix(alldata)[,1:dim(alldata)[2]]
obs_wfpm <- as.vector(alldata)

# Calculate summary statistics
daily_wfpm.mean <- mean(obs_wfpm, na.rm = TRUE)
daily_wfpm.sd <- sd(obs_wfpm, na.rm = TRUE)
daily_wfpm.min <- min(obs_wfpm, na.rm = TRUE)
daily_wfpm.max <- max(obs_wfpm, na.rm = TRUE)
daily_wfpm.per05 <- quantile(obs_wfpm, 0.05, type = 1, na.rm = TRUE)
daily_wfpm.per95 <- quantile(obs_wfpm, 0.95, type = 1, na.rm = TRUE)


####**************************************************************************
#### Create counterfactual exposure vector for use in DLNM crosspreds  #### 
####**************************************************************************

# Create counterfactual exposure vector
# Notes: We use this exposure vector for creating the estimates from the dlnm model. 
#        These are the counterfactual exposure levels, and we generate effect estimates
#        for a contrast between the reference level (the mean) and these levels. 
#        We likely won't need all of these, but making a big exposure contrast table 
#        makes it much easier to deal with any changes to analysis, e.g., reporting a different effect estimate


##### USER SPEC ----
lag_number <- 14 #7 or 14


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
  mutate(Counterfactual_wfpm = round(Counterfactual_wfpm, lag_number))

# Clean up 
rm(obs_wfpm, alldata)


####********************************************************
#### Add case and time-to-event variables for models #### 
####********************************************************

# Add needed vars for city analysis
data4casecross_zip <- data4casecross_zip %>% 
  mutate(Case = ifelse(dayName == 'Caseday_0', 1, 0)) # bivariate Case variable

####****************************************************************************** BEGIN FUNCTION
# Here I edit functions developed by Sebastian Rowland


####*******************************************************
#### Create function to conduct dlnm case-crossover #### 
####*******************************************************

# Name function to analyze dlnm
#* Note that we have already set the ER df to be 3 for temperature and relative humidity
#* The ER argument for this function is specifically for non-temperature/non-relative humidity ER df
analyze_dlnm_wfpm <- function(ExpTerm, CaseType, Sensitivity,  
                              ERConstraint, LRConstraint, dta, lag_number){
  
  print(CaseType)
  
  # filter to days with cases along with their controls (bc a case cannot be a day without 0 events)
  dta <- dta %>%
    filter(!!sym(CaseType) > 0)
  
  # Create ModelName
  ModelIdentifier <- paste0(ExpTerm, '_', CaseType, '_', Sensitivity)
  ExpConstraints <- paste0('ER', ERConstraint, '_LR', LRConstraint)
  LagName <- paste0("Lag", lag_number)
  ModelName <- paste0(ModelIdentifier,'_', ExpConstraints, "_", LagName)
  
  # Set lags
  NumLag <- as.numeric(lag_number)
  
  ####***************************
  #### Create cross basis #### 
  ####***************************
  
  # Set ER (exposure response) and LR (lagged response) constraints
  ERdf <- as.numeric(str_remove_all(ERConstraint, '[A-z]')) # Remove all letters, leaving only number of df (degrees of freedom)
  LRdf <- as.numeric(str_remove_all(LRConstraint, '[A-z]'))
  
  # Create cross basis for temperature
  cb.temp <- crossbasis(
    as.matrix(dplyr::select(dta, contains('max_temp')))[,1:NumLag], 
    lag=c(0,(NumLag-1)),
    argvar=list(fun='ns', df = 3),
    arglag=list(fun='ns', df = LRdf))
  
  # Create cross basis for relative humidity
  cb.rh <- crossbasis(
    as.matrix(dplyr::select(dta, contains('min_rh')))[,1:NumLag], 
    lag=c(0,(NumLag-1)),
    argvar=list(fun='ns', df = 3),
    arglag=list(fun='ns', df = LRdf))
  
  # Create cross basis for wfpm
  # a nonlinear wfpm term
  if(str_detect(ERConstraint, 'evenknots') & str_detect(LRConstraint, 'evenknots')){
    cb.wfpm <- crossbasis(
      as.matrix(dplyr::select(dta, starts_with('wfpm_lag')))[,1:NumLag], 
      lag=c(0,(NumLag-1)),
      argvar=list(fun='ns', df = ERdf),
      arglag=list(fun='ns', df = LRdf))
  }
  
  # a linear wfpm term
  if(str_detect(ERConstraint, 'lin') & str_detect(LRConstraint, 'evenknots')){
    cb.wfpm <- crossbasis(
      as.matrix(dplyr::select(dta, starts_with('wfpm_lag')))[,1:NumLag],
      lag=c(0,(NumLag-1)),
      argvar=list(fun='lin'),
      arglag=list(fun='ns', df = LRdf))
  }
  
  # Create cross basis for nonwfpm 
  if(str_detect(Sensitivity, 'nonwfpm')){ 
    cb.nonwfpm <- crossbasis(
      as.matrix(dplyr::select(dta, starts_with('nonwfpm_lag')))[,1:NumLag], 
      lag=c(0,(NumLag-1)),
      argvar=list(fun='lin'),
      arglag=list(fun='ns', df = LRdf))
  }
  
  
  ####****************************
  #### Create health model ####
  ####****************************
  
  if(str_detect(Sensitivity, 'Main')){
    
    # Health model for main analysis
    # prepare the weights variable
    dta_weights <- dta %>%
      select(!!sym(CaseType))
    
    # clogit
    mod <- clogit(Case ~ cb.wfpm +
                    cb.temp +                        # LR and ER temp
                    cb.rh +                          # LR and ER rh
                    strata(id),
                  weights = dta_weights[[1]],        # num of daily events (could only use if exposure is same for all cases/controls in a given hour); set this to be our outcome of interest, which can change by input!
                  method = "efron",
                  data = dta)
  }
  
  
  # Health model for sensitivity analysis: extend lag to 14 days
  # same as above - we dont change anything in model
  if(str_detect(Sensitivity, 'Sens_lag14')){
    
    # prepare the weights variable
    dta_weights <- dta %>%
      select(!!sym(CaseType))
    
    # clogit
    mod <- clogit(Case ~ cb.wfpm +
                    cb.temp +                        # LR and ER temp
                    cb.rh +                          # LR and ER rh
                    strata(id),
                  weights = dta_weights[[1]],        # num of daily events (could only use if exposure is same for all cases/controls in a given hour); set this to be our outcome of interest, which can change by input!
                  method = "efron",
                  data = dta)
  }
  
  
  # Health model for sensitivity analysis: include nonwfpm confounder
  if(str_detect(Sensitivity, 'nonwfpm')){
    
    # prepare the weights variable
    dta_weights <- dta %>%
      select(!!sym(CaseType))
    
    # clogit
    mod <- clogit(Case ~ cb.wfpm +
                    cb.temp +                        # LR and ER temp
                    cb.rh +                          # LR and ER rh
                    cb.nonwfpm +                     # include crossbasis for non wfpm 
                    strata(id),
                  weights = dta_weights[[1]],        # num of daily events (could only use if exposure is same for all cases/controls in a given hour); set this to be our outcome of interest, which can change by input!
                  method = "efron",
                  data = dta)
  }
  
  ####***********************************************
  #### Save model and create crosspredictions ####
  ####***********************************************
  
  # Save the model
  if(str_detect(Sensitivity, regex('(?i)Main'))){
    mod %>% saveRDS(paste0(output_path, '/Models/Main/', ModelName, '.RDS'))
  }
  
  if(!str_detect(Sensitivity, regex('(?i)Main'))){
    mod %>% saveRDS(paste0(output_path, '/Models/Sensitivity/', ModelName, '.RDS'))
  }
  
  
  ###### Crosspreds Nonlinear ER LR ---------------------------------------------------------
  # Begin option for nonlinear ER and LR
  if(str_detect(ERConstraint, 'evenknots') & str_detect(LRConstraint, 'evenknots')){
    
    # Generate estimates for nonlinear ER and LR
    # the cen argument sets the reference exposure level for our effect estimates (set to the mean NO2 value)
    # crosspred() will yield estimates for 100 exposure levels plus those exposure levels we set
    est <- crosspred(cb.wfpm,                                   # exposure crossbasis
                     mod,                                          # health model
                     cen = daily_wfpm.mean,                         # center at mean NO2
                     at = expContrasts$Counterfactual_wfpm,          # compute estimated association for each integer value of NO2 in Counterfactual_wfpm vector
                     cumul = TRUE,                                 # also compute cumulative associations
                     bylag = 1)                                    # estimates association along each lag
    
    
    # Extract coefficient fit and CI
    fit.table <- as.data.frame(est$matRRfit)
    colnames(fit.table) <- paste0('fit.or.', colnames(fit.table))
    fit.table <- fit.table %>%
      mutate(Counterfactual_wfpm = as.numeric(row.names(fit.table)))
    
    lci.table <- as.data.frame(est$matRRlow)
    colnames(lci.table) <- paste0('lci.or.', colnames(lci.table))
    
    uci.table <- as.data.frame(est$matRRhigh)
    colnames(uci.table) <- paste0('uci.or.', colnames(uci.table))
    
    # Combine fit and se for individual lags
    #    Notes: All OR are relative to the mean wfpm
    est.table <- bind_cols(fit.table, lci.table, uci.table)
    
    # Attach the labels of the exposure contrasts
    est.table <- est.table %>% full_join(expContrasts, by = 'Counterfactual_wfpm')
    
    # Save estimate table for individual lags
    
    if(str_detect(Sensitivity, regex('(?i)Main'))){
      est.table %>%
        write.csv(paste0(output_path, '/Estimates/Main/',
                         'EstInd_', ModelName, '.csv'))
    }
    
    if(!str_detect(Sensitivity, regex('(?i)Main'))){
      est.table %>%
        write.csv(paste0(output_path, '/Estimates/Sensitivity/',
                         'EstInd_', ModelName, '.csv'))
    }
    
    # Extract cumulative coefficient fit and ci
    fit.table <- as.data.frame(est$cumRRfit)
    colnames(fit.table) <- paste0('fit.or.', colnames(fit.table))
    fit.table <- fit.table %>%
      mutate(Counterfactual_wfpm = as.numeric(row.names(fit.table)))
    
    lci.table <- as.data.frame(est$cumRRlow)
    colnames(lci.table) <- paste0('lci.or.', colnames(lci.table))
    
    uci.table <- as.data.frame(est$cumRRhigh)
    colnames(uci.table) <- paste0('uci.or.', colnames(uci.table))
    
    # Combine fit and se for individual lags
    #    Notes: All OR are relative to the mean NO2
    est.table <- bind_cols(fit.table, lci.table, uci.table)
    
    # Attach the labels of the exposure contrasts
    est.table <- est.table %>% full_join(expContrasts, by = 'Counterfactual_wfpm')
    
    # Save cumulative estimates table
    if(str_detect(Sensitivity, regex('(?i)Main'))){
      est.table %>%
        write.csv(paste0(output_path, '/Estimates/Main/',
                         'EstCumul_', ModelName, '.csv'))
    }
    
    if(!str_detect(Sensitivity, regex('(?i)Main'))){
      est.table %>%
        write.csv(paste0(output_path, '/Estimates/Sensitivity/',
                         'EstCumul_', ModelName, '.csv'))
    }
    
  }
  
  
  ###### Crosspreds Linear ER LR ---------------------------------------------------------
  # Begin option for linear ER and nonlinear LR
  if(str_detect(ERConstraint, 'lin') & str_detect(LRConstraint, 'evenknots')){
    
    # Generate estimates for nonlinear ER and LR
    est <- crosspred(cb.wfpm,                    # exposure crossbasis
                     mod,                           # health model
                     at = 0:daily_wfpm.max,          # compute estimated association for each integer value of NO2
                     cumul = TRUE,                  # also compute cumulative associations
                     bylag = 1)                     # estimates association along each lag
    
    # Extract coefficient fit and CI
    # note that matRRfit is the exponentiated version of prediction vars
    fit.table <- as.data.frame(est$matRRfit)
    colnames(fit.table) <- paste0('fit.or.', colnames(fit.table))
    fit.table <- fit.table %>%
      mutate(Counterfactual_wfpm = as.numeric(row.names(fit.table)))
    
    lci.table <- as.data.frame(est$matRRlow)
    colnames(lci.table) <- paste0('lci.or.', colnames(lci.table))
    
    uci.table <- as.data.frame(est$matRRhigh)
    colnames(uci.table) <- paste0('uci.or.', colnames(uci.table))
    
    # Combine fit and se for individual lags
    #    Notes: All OR are relative 0 mg^m3
    est.table <- bind_cols(fit.table, lci.table, uci.table)
    
    # Save estimate table for individual lags
    if(str_detect(Sensitivity, regex('(?i)Main'))){
      est.table %>%
        write.csv(paste0(output_path, '/Estimates/Main/',
                         'EstInd_', ModelName, '.csv'))
    }
    
    if(!str_detect(Sensitivity, regex('(?i)Main'))){
      est.table %>%
        write.csv(paste0(output_path, '/Estimates/Sensitivity/',
                         'EstInd_', ModelName, '.csv'))
    }
    
    # Extract cumulative coefficient fit and ci
    fit.table <- as.data.frame(est$cumRRfit)
    colnames(fit.table) <- paste0('fit.or.', colnames(fit.table))
    fit.table <- fit.table %>%
      mutate(Counterfactual_wfpm = as.numeric(row.names(fit.table)))
    
    lci.table <- as.data.frame(est$cumRRlow)
    colnames(lci.table) <- paste0('lci.or.', colnames(lci.table))
    
    uci.table <- as.data.frame(est$cumRRhigh)
    colnames(uci.table) <- paste0('uci.or.', colnames(uci.table))
    
    # Combine fit and se for cumulative lags
    #    Notes: All OR are relative to the mean NO2
    est.table <- bind_cols(fit.table, lci.table, uci.table)
    
    # Save cumulative estimates table
    if(str_detect(Sensitivity, regex('(?i)Main'))){
      est.table %>%
        write.csv(paste0(output_path, '/Estimates/Main/',
                         'EstCumul_', ModelName, '.csv'))
    }
    
    if(!str_detect(Sensitivity, regex('(?i)Main'))){
      est.table %>%
        write.csv(paste0(output_path, '/Estimates/Sensitivity/',
                         'EstCumul_', ModelName, '.csv'))
    }
    
  }
  
}

####****************************************************************************** END FUNCTION


####*******************************************
#### Run main and sensitivity analyses #### 
####*******************************************
# ExpTerm <- 'zip_daily_wfpm'; CaseType <- 'count_ADRD_any'; Sensitivity <- 'Main';
# ERConstraint <- '4dfevenknots'; LRConstraint <- '3dfevenknots'; dta <- data4casecross_zip

# after best parameters determined, actually run the health model

##### Main models (mortality) -------------------------------------------------------------
analyze_dlnm_wfpm(ExpTerm = 'zip_daily_wfpm', dta = data4casecross_zip,
                  CaseType = "count_death", Sensitivity = "Main",
                  ERConstraint = "3dfevenknots", LRConstraint = "5dfevenknots",
                  lag_number = 7)

analyze_dlnm_wfpm(ExpTerm = 'zip_daily_wfpm', dta = data4casecross_zip,
                  CaseType = "count_death", Sensitivity = "Main",  
                  ERConstraint = "lin", LRConstraint = "5dfevenknots",
                  lag_number = 7)

