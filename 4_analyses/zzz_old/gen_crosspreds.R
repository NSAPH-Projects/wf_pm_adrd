####***********************
#### Code Description ####
# Author: Vivian
# Date: 8/8/23
# Goal: Generate summary stats and cross predictions (counterfactual values) for the 
####**********************

output_path <-"/n/dominici_nsaph_l3/Lab/projects/wfpm_adrd_hosp/data/output"

source(here::here("0_set_up", "1_libraries.R"))

# read in sample data
samp_data_25pct <- read_fst('/n/dominici_nsaph_l3/Lab/projects/wfpm_adrd_hosp/data/data_process/samp_data/samp_data_25pct.fst')

####************************************************************
#### 1: Calculate summary stats for use in DLNM crosspreds  #### 
####************************************************************

# 1a Create vector of observed daily wfpm concentrations 
alldata <- samp_data_25pct %>%
  dplyr::select(contains('wfpm_lag'))
alldata <- as.matrix(alldata)[,1:6]
obsDaily_wfpm <- as.vector(alldata)

# 1b Calculate summary statistics
Daily_wfpm.mean <- mean(obsDaily_wfpm, na.rm = TRUE)
Daily_wfpm.sd <- sd(obsDaily_wfpm, na.rm = TRUE)
Daily_wfpm.min <- min(obsDaily_wfpm, na.rm = TRUE)
Daily_wfpm.max <- max(obsDaily_wfpm, na.rm = TRUE)
Daily_wfpm.per05 <- quantile(obsDaily_wfpm, 0.05, type = 1, na.rm = TRUE)
Daily_wfpm.per95 <- quantile(obsDaily_wfpm, 0.95, type = 1, na.rm = TRUE)


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
  Counterfactual_wfpm = c(seq(Daily_wfpm.min, Daily_wfpm.max, length.out = 100), 
                        quantile(obsDaily_wfpm, 0.01, type = 1, na.rm = T), quantile(obsDaily_wfpm, 0.99, type = 1, na.rm = T), 
                        quantile(obsDaily_wfpm, 0.05, type = 1, na.rm = T), quantile(obsDaily_wfpm, 0.95, type = 1, na.rm = T), 
                        quantile(obsDaily_wfpm, 0.10, type = 1, na.rm = T), quantile(obsDaily_wfpm, 0.90, type = 1, na.rm = T),  
                        quantile(obsDaily_wfpm, 0.15, type = 1, na.rm = T), quantile(obsDaily_wfpm, 0.85, type = 1, na.rm = T),
                        quantile(obsDaily_wfpm, 0.20, type = 1, na.rm = T), quantile(obsDaily_wfpm, 0.80, type = 1, na.rm = T), 
                        quantile(obsDaily_wfpm, 0.25, type = 1, na.rm = T), quantile(obsDaily_wfpm, 0.75, type = 1, na.rm = T), 
                        Daily_wfpm.mean - Daily_wfpm.sd,  Daily_wfpm.mean + Daily_wfpm.sd, 
                        Daily_wfpm.mean - 10,  Daily_wfpm.mean + 10),
  Label = c(rep('ERValues', 100), 'per01','per99', 'per05', 'per95', 'per10', 'per90',
            'per15', 'per85', 'per20', 'per80', 'per25', 'per75', 'MeanMinusSD', 'MeanPlusSD', 
            'MeanMinus10', 'MeanPlus10')) %>% 
  mutate(Counterfactual_wfpm = round(Counterfactual_wfpm, 7))

# 2b Clean up 
rm(obsDaily_wfpm, alldata)
