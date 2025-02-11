# Stratified analyses: conduct DLNM Case-crossover Analyses using INDIVIDUAL LEVEL DATA
# wf pm - hospitalizations
# primary hosp - adrd
# secondary hosp - cardiovascular, respiratory, mental health
# Vivian Do
# 12/13/23

####********************
#### Preparation #### 
####********************
rm(list=ls())
source(here::here("0_set_up", "1_libraries.R"))

data_path <- '/n/dominici_nsaph_l3/Lab/projects/wfpm_adrd_hosp/data/'
data_path_data4casecross <- "/n/dominici_nsaph_l3/Lab/projects/wfpm_adrd_hosp/data/data_process_mortality/data4casecross/"
data_path_stratification_vars <- '/n/dominici_nsaph_l3/Lab/projects/wfpm_adrd_hosp/data/data_process_mortality/stratification_vars/'
output_path <-"/n/dominici_nsaph_l3/Lab/projects/wfpm_adrd_hosp/data/output_mortality/"

# data4casecross_zip <- read_fst('/n/dominici_nsaph_l3/Lab/projects/wfpm_adrd_hosp/data/data_process_mortality/data4casecross/data_w_all_vars/data4casecross_zip.fst')
data4casecross_ind <- read_fst('/n/dominici_nsaph_l3/Lab/projects/wfpm_adrd_hosp/data/data_process_mortality/data4casecross/data_w_all_vars/data4casecross_ind.fst') %>% 
  rename(old_id = id,
         id = new_id_data4casecross)
names(data4casecross_ind)

# summary(data4casecross_ind$zip)

####************************************************************
#### Limit analyses to April - October  #### 
####************************************************************
data4casecross_ind <- data4casecross_ind %>% 
  filter(month(DayDateTime) %in% c(4:10)) %>% 
  mutate(count_death = 1) #set death = 1 because they all died

####************************************************************
#### Calculate summary stats for use in DLNM crosspreds  #### 
####************************************************************

# Create vector of observed daily wf pm concentrations 
alldata <- data4casecross_ind %>%
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

# Clean up 
rm(obs_wfpm, alldata)


####********************************************************
#### Add case and time-to-event variables for models #### 
####********************************************************

# Add needed vars for city analysis
data4casecross_ind <- data4casecross_ind %>% 
  mutate(Case = ifelse(dayName == 'Caseday_0', 1, 0)) # bivariate Case variable


# fix urbanicity variable
data4casecross_ind <- data4casecross_ind %>% 
  mutate(urbanicity = ifelse(urbanicity == "urban/suburban", "urbansuburban", urbanicity))

unique(data4casecross_ind$urbanicity)

# check age distribution
hist(data4casecross_ind$age)
nrow(data4casecross_ind %>% filter(age <= 75)) # 4 816 729 (22.2%)
nrow(data4casecross_ind %>% filter(age <= 80)) # 8 938 464 (41.1%)
nrow(data4casecross_ind)

# make age categories
data4casecross_ind <- data4casecross_ind %>% 
  mutate(age_category = ifelse(age <= 75, "<=75", ">75"))

# make poverty variable binary
pov_threshold <- quantile(data4casecross_ind$pct_pov_final, probs = 0.75, na.rm = TRUE)
data4casecross_ind <- data4casecross_ind %>% 
  mutate(pov_category = ifelse(pct_pov_final >= as.numeric(quantile(data4casecross_ind$pct_pov_final, probs = 0.75, na.rm = TRUE)), "pov_gtet20pct", "pov_lt20pct"))

# make census region categories
data4casecross_ind <- data4casecross_ind %>% 
  mutate(census_region = case_when(state %in% c("CT", "ME", "MA", "NH", "RI", "VT", "NJ", "NY", "PA") ~ "Northeast",
                                   state %in% c("IN", "IL", "MI", "OH", "WI", "IA", "KS", "MN", "MO", "NE", "ND", "SD") ~ "Midwest",
                                   state %in% c("AZ", "CO", "ID", "NM", "MT", "UT", "NV", "WY", "AK", "CA", "HI", "OR", "WA")~ "West",
                                   state %in% c("DE", "DC", "FL", "GA", "MD", "NC", "SC", "VA", "WV", "AL", "KY", "MS", "TN", "AR", "LA", "OK", "TX") ~ "South"))


####****************************************************************************** BEGIN FUNCTION
# Here I edit functions developed by Sebastian Rowland


####*******************************************************
#### Create function to conduct dlnm case-crossover #### 
####*******************************************************

# ## test dataframe
# first_10_unique <- head(distinct(data4casecross_ind, id), 1000)
# data4casecross_ind_smaller <- data4casecross_ind %>%
#   filter(id %in% first_10_unique$id)


# Name function to analyze dlnm
#* Note that we have already set the ER df to be 3 for temperature and relative humidity
#* The ER argument for this function is specifically for non-temperature/non-relative humidity ER df
analyze_dlnm_wfpm_stratification <- function(ExpTerm, CaseType, Stratify_Variable,  
                                             ERConstraint, LRConstraint, dta){
  
  print(paste0("Outcome: ", CaseType, ", stratification var: ", Stratify_Variable))
  
  # remove stratification groups of NA
  strat_unique_levels <- c(na.omit((unique(dta[[Stratify_Variable]]))))
  print(strat_unique_levels)
  
  for (strat_unique_level in 1:length(strat_unique_levels)){
    
    strat_unique_level <- 1
    print(strat_unique_levels[[strat_unique_level]])
    
    # filter to (1) days with cases along with their controls (bc a case cannot be a day without 0 events)
    # and (2) one group level in the stratification variable of interest (e.g., male vs female in sex_group)
    dta_stratified <- dta %>% 
      filter(!!sym(CaseType) > 0,
             !!sym(Stratify_Variable) == strat_unique_levels[[strat_unique_level]])
    
    # Create ModelName
    ModelIdentifier <- paste0(ExpTerm, '_', CaseType, '_', strat_unique_levels[[strat_unique_level]])
    ExpConstraints <- paste0('ER', ERConstraint, '_LR', LRConstraint)
    ModelName <- paste0(ModelIdentifier,'_', ExpConstraints)
    
    # Set lags
    NumLag <- 7
    
    ####***************************
    #### Create cross basis ####
    ####***************************
    
    # Set ER (exposure response) and LR (lagged response) constraints
    ERdf <- as.numeric(str_remove_all(ERConstraint, '[A-z]')) # Remove all letters, leaving only number of df (degrees of freedom)
    LRdf <- as.numeric(str_remove_all(LRConstraint, '[A-z]'))
    
    # Create cross basis for temperature
    cb.temp <- crossbasis(
      as.matrix(dplyr::select(dta_stratified, contains('max_temp')))[,1:NumLag],
      lag=c(0,(NumLag-1)),
      argvar=list(fun='ns', df = 3),
      arglag=list(fun='ns', df = LRdf))
    
    # Create cross basis for relative humidity
    cb.rh <- crossbasis(
      as.matrix(dplyr::select(dta_stratified, contains('min_rh')))[,1:NumLag],
      lag=c(0,(NumLag-1)),
      argvar=list(fun='ns', df = 3),
      arglag=list(fun='ns', df = LRdf))
    
    # Create cross basis for wfpm
    # a nonlinear wfpm term
    if(str_detect(ERConstraint, 'evenknots') & str_detect(LRConstraint, 'evenknots')){
      cb.wfpm <- crossbasis(
        as.matrix(dplyr::select(dta_stratified, starts_with('wfpm_lag')))[,1:NumLag],
        lag=c(0,(NumLag-1)),
        argvar=list(fun='ns', df = ERdf),
        arglag=list(fun='ns', df = LRdf))
    }
    
    # a linear wfpm term
    if(str_detect(ERConstraint, 'lin') & str_detect(LRConstraint, 'evenknots')){
      cb.wfpm <- crossbasis(
        as.matrix(dplyr::select(dta_stratified, starts_with('wfpm_lag')))[,1:NumLag],
        lag=c(0,(NumLag-1)),
        argvar=list(fun='lin'),
        arglag=list(fun='ns', df = LRdf))
    }
    
    # Create cross basis for nonwfpm
    if(str_detect(ExpTerm, 'nonwfpm')){
      cb.nonwfpm <- crossbasis(
        as.matrix(dplyr::select(dta_stratified, starts_with('nonwfpm_lag')))[,1:NumLag], 
        lag=c(0,(NumLag-1)),
        argvar=list(fun='lin'),
        arglag=list(fun='ns', df = LRdf))
    }
    
    ####****************************
    #### Create health model ####
    ####****************************
    
    if(!str_detect(ExpTerm, 'nonwfpm')){
      # clogit
      mod <- clogit(Case ~ cb.wfpm +
                      cb.temp +                        # LR and ER temp
                      cb.rh +                          # LR and ER rh
                      strata(id),
                    method = "efron",
                    data = dta_stratified)
    }
    
    # Health model for sensitivity analysis: include nonwfpm confounder
    if(str_detect(ExpTerm, 'nonwfpm')){
      
      # clogit
      mod <- clogit(Case ~ cb.wfpm +
                      cb.temp +                        # LR and ER temp
                      cb.rh +                          # LR and ER rh
                      cb.nonwfpm +                     # include crossbasis for non wfpm 
                      strata(id),
                    method = "efron",
                    data = dta_stratified)
    }
    
    ####***********************************************
    #### Save model and create crosspredictions ####
    ####***********************************************
    
    # Save the model
    mod %>% saveRDS(paste0(output_path, 'Models/Stratification/', ModelName, '.RDS'))
    
    # Begin option for nonlinear ER and LR
    if(str_detect(ERConstraint, 'evenknots') & str_detect(LRConstraint, 'evenknots')){
      
      # Generate estimates for nonlinear ER and LR
      # the cen argument sets the reference exposure level for our effect estimates (set to the mean wfpm value)
      # crosspred() will yield estimates for 100 exposure levels plus those exposure levels we set
      est <- crosspred(cb.wfpm,                                   # exposure crossbasis
                       mod,                                          # health model
                       cen = daily_wfpm.mean,                         # center at mean wfpm
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
      #    Notes: All OR are relative to the mean NO2
      est.table <- bind_cols(fit.table, lci.table, uci.table)
      
      # Attach the labels of the exposure contrasts
      est.table <- est.table %>% full_join(expContrasts, by = 'Counterfactual_wfpm')
      
      # Save estimate table for individual lags
      est.table %>%
        write.csv(paste0(output_path, 'Estimates/Stratification/',
                         'EstInd_', ModelName, '.csv'))
      
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
      est.table %>%
        write.csv(paste0(output_path, 'Estimates/Stratification/',
                         'EstCumul_', ModelName, '.csv'))
    }
    
    # Begin option for linear ER and nonlinear LR
    if(str_detect(ERConstraint, 'lin') & str_detect(LRConstraint, 'evenknots')){
      
      # Generate estimates for nonlinear ER and LR
      est <- crosspred(cb.wfpm,                    # exposure crossbasis
                       mod,                           # health model
                       at = 0:daily_wfpm.max,          # compute estimated association for each integer value of wfpm
                       cumul = TRUE,                  # also compute cumulative associations
                       bylag = 1)                     # estimates association along each lag
      
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
      #    Notes: All OR are relative 0 ppb
      est.table <- bind_cols(fit.table, lci.table, uci.table)
      
      # Save estimate table for individual lags
      est.table %>%
        write.csv(paste0(output_path, 'Estimates/Stratification/',
                         'EstInd_', ModelName, '.csv'))
      
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
      est.table %>%
        write.csv(paste0(output_path, 'Estimates/Stratification/',
                         'EstCumul_', ModelName, '.csv'))
    }
    
  }
  
}

####****************************************************************************** END FUNCTION


####*******************************************
#### Run main and Stratify_Variable analyses #### 
####*******************************************
# ExpTerm <- 'zip_daily_wfpm'; CaseType <- 'count_ADRD_any'; Stratify_Variable <- 'Main';
# ERConstraint <- '4dfevenknots'; LRConstraint <- '3dfevenknots'; dta <- data4casecross_ind

# after best parameters determined, actually run the health model

##### Secondary analysis models -------------------------------------------------------------
start_time <- Sys.time() 

# mortality/death; ER = linear; LR = 5
## sex 
analyze_dlnm_wfpm_stratification(ExpTerm = 'ind_daily_wfpm', dta = data4casecross_ind,
                                 CaseType = "count_death", Stratify_Variable = "Sex_gp",  
                                 ERConstraint = "lin", LRConstraint = "5dfevenknots")

## urbanicity 
analyze_dlnm_wfpm_stratification(ExpTerm = 'ind_daily_wfpm', dta = data4casecross_ind,
                                 CaseType = "count_death", Stratify_Variable = "urbanicity",  
                                 ERConstraint = "lin", LRConstraint = "5dfevenknots")

## age -
analyze_dlnm_wfpm_stratification(ExpTerm = 'ind_daily_wfpm', dta = data4casecross_ind,
                                 CaseType = "count_death", Stratify_Variable = "age_category",  
                                 ERConstraint = "lin", LRConstraint = "5dfevenknots")

## pov 
analyze_dlnm_wfpm_stratification(ExpTerm = 'ind_daily_wfpm', dta = data4casecross_ind,
                                 CaseType = "count_death", Stratify_Variable = "pov_category",  
                                 ERConstraint = "lin", LRConstraint = "5dfevenknots")









