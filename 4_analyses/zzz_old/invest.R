# Investigation
# Goal: I have questions, and I would like empirical answers
# Vivian Do
# Updated 5/23/23

####********************
#### Preparation #### 
####********************
source(here::here("0_set_up", "1_libraries.R"))

data_path <- '/n/dominici_nsaph_l3/Lab/projects/wfpm_adrd_hosp/data/'
data_path_data4casecross <- "/n/dominici_nsaph_l3/Lab/projects/wfpm_adrd_hosp/data/data_process/data4casecross/"
data_path_stratification_vars <- '/n/dominici_nsaph_l3/Lab/projects/wfpm_adrd_hosp/data/data_process/stratification_vars/'
output_path <-"/n/dominici_nsaph_l3/Lab/projects/wfpm_adrd_hosp/data/output"

data4casecross_zip <- read_fst('/n/dominici_nsaph_l3/Lab/projects/wfpm_adrd_hosp/data/data_process/data4casecross/data_w_all_vars/data4casecross_zip.fst')


# Is nonlinear-1 df combo the same as linear? -----------------------------
#* In the crossbasis, you can set ER to be natural spline with 1 df OR linear
#* Mathematically they should be the same but do they produce the same output?

##### Structure data -------------------------------------------------------
# Add needed vars for city analysis
data4casecross_zip <- data4casecross_zip %>% 
  mutate(Case = ifelse(dayName == 'Caseday_0', 1, 0)) # bivariate Case variable

ERdf <- 1 #set to be 1 bc we are comparing df 1 vs linear
LRdf <- 3 #randomly set this (doesnt matter as much in this case)
NumLag <- 6
CaseType <- "count_ADRD_prmy_out"

dta <- data4casecross_zip
dta <- dta %>% 
  filter(!!sym(CaseType) > 0)

##### Create crossbases -------------------------------------------------------
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

# Create cross basis for wfpm ns with 1df
cb.wfpm_ns_1df <- crossbasis(
  as.matrix(dplyr::select(dta, contains('wfpm_lag')))[,1:NumLag], 
  lag=c(0,(NumLag-1)),
  argvar=list(fun='ns', df = ERdf),
  arglag=list(fun='ns', df = LRdf))

# Create cross basis for wfpm linear
cb.wfpm_linear <- crossbasis(
  as.matrix(dplyr::select(dta, contains('wfpm_lag')))[,1:NumLag], 
  lag=c(0,(NumLag-1)),
  argvar=list(fun='lin'),
  arglag=list(fun='ns', df = LRdf))


##### Compare models ----------------------------------------------------------
dta_weights <- dta %>% 
  select(!!sym(CaseType))

mod_wfpm_ns_1df <- clogit(Case ~ cb.wfpm_ns_1df +
                       cb.temp +                        # LR and ER temp
                       cb.rh +                          # LR and ER rh
                       strata(id),
                     weights = dta_weights[[1]],        # num of daily events (could only use if exposure is same for all cases/controls in a given hour); set this to be our outcome of interest, which can change by input!
                     method = "efron",
                     data = dta)


mod_wfpm_linear <- clogit(Case ~ cb.wfpm_linear +
                       cb.temp +                        # LR and ER temp
                       cb.rh +                          # LR and ER rh
                       strata(id),
                     weights = dta_weights[[1]],        # num of daily events (could only use if exposure is same for all cases/controls in a given hour); set this to be our outcome of interest, which can change by input!
                     method = "efron",
                     data = dta)

# Save the model 
mod_wfpm_ns_1df %>% saveRDS(paste0(output_path, '/Models/', "invest_mod_wfpm_ns_1df", '.RDS'))
mod_wfpm_linear %>% saveRDS(paste0(output_path, '/Models/', "invest_mod_wfpm_linear", '.RDS'))

# estimates - wf ns 1 df
est_wfpm_ns_1df <- crosspred(cb.wfpm_ns_1df,                                   # exposure crossbasis
                             mod_wfpm_ns_1df,                                          # health model
                 cen = daily_wfpm.mean,                         # center at mean NO2
                 at = expContrasts$Counterfactual_wfpm,          # compute estimated association for each integer value of NO2 in Counterfactual_wfpm vector
                 cumul = TRUE,                                 # also compute cumulative associations
                 bylag = 1)                                    # estimates association along each lag

fit.table <- as.data.frame(est_wfpm_ns_1df$matRRfit)  
colnames(fit.table) <- paste0('fit.or.', colnames(fit.table))
fit.table <- fit.table %>%  
  mutate(Counterfactual_wfpm = as.numeric(row.names(fit.table)))

lci.table <- as.data.frame(est_wfpm_ns_1df$matRRlow)  
colnames(lci.table) <- paste0('lci.or.', colnames(lci.table))

uci.table <- as.data.frame(est_wfpm_ns_1df$matRRhigh)  
colnames(uci.table) <- paste0('uci.or.', colnames(uci.table))

est.table_wfpm_ns_1df <- bind_cols(fit.table, lci.table, uci.table)

# estimates - wf linear
est_wfpm_linear <- crosspred(cb.wfpm_linear,                    # exposure crossbasis
                             mod_wfpm_linear,                           # health model
                 at = 0:daily_wfpm.max,          # compute estimated association for each integer value of NO2
                 cumul = TRUE,                  # also compute cumulative associations
                 bylag = 1)                     # estimates association along each lag

# Extract coefficient fit and CI 
fit.table <- as.data.frame(est_wfpm_linear$matRRfit)  
colnames(fit.table) <- paste0('fit.or.', colnames(fit.table))
fit.table <- fit.table %>%  
  mutate(Counterfactual_wfpm = as.numeric(row.names(fit.table)))

lci.table <- as.data.frame(est_wfpm_linear$matRRlow)  
colnames(lci.table) <- paste0('lci.or.', colnames(lci.table))

uci.table <- as.data.frame(est_wfpm_linear$matRRhigh)  
colnames(uci.table) <- paste0('uci.or.', colnames(uci.table))

est.table_wfpm_linear <- bind_cols(fit.table, lci.table, uci.table)

# generate plots




