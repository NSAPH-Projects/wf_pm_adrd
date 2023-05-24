####***********************
#### Code Description ####
# Author: Vivian 
# Date: 5/9/23
# Goal: assign icd 9/10 codes for other cause-specific outcomes
####**********************


# Read in data ------------------------------------------------------------

# note: this is only for 1 year but use as test (will need to expand to several years EDA)
# for just one year (2020), the data is 911,508, so 10 years ~9,115,080
adrd_hosp <- read_fst(paste0(adrd_dir, "ADRD_2000.fst")) 
adrd_hosp_tmp <- adrd_hosp[1:1000,]


# Define ICD 9/10 codes ---------------------------------------------------
# circulatory (but not cardiovascular)
icd_circ <- c((390:459), #icd 9
              paste0("I", "0", 0:9), #icd 10
              paste0("I", 10:99)) #icd 10

# respiratory
icd_resp <- c((460:519), #icd 9
              paste0("J", "0", 0:9), #icd 10
              paste0("J", 10:99)) #icd 10

# PTSD, "- Need both first date and all dates" <- what does this mean
icd_ptsd <- c(309.81, #icd 9
              "F43.10", "F43.11", "F43.12") #icd 10

# anxiety; if 2+ visits on different days have these codes
icd_anxty <- c(300.00, 300.01, 300.02, 300.11, #icd 9
               300.20, 300.21, 300.22, 300.23,
               300.7, 313.0, 
               
               "F40.00", "F40.01", "F40.02", "F40.10", #icd 10
               "F40.11", "F40.9", "F41.0", "F41.1",
               "F41.9", "F44.4", 'F44.5', "F44.6", "F44.7",
               "F45.20", "F45.21", "F45.22", "F45.29")

# depression; if 2+ visits on different days have these codes
icd_dep <- c(296.20, 296.21, 296.22, 296.23,
             296.25, 296.26, 296.30, 296.31,
             296.32, 296.33, 296.34, 296.35,
             296.36, 298.0, 311, 300.4, 309.1,
             
             "F32.3", "F33.3", "F32.0", "F32.1", "F32.2",
             "F32.4", "F32.5", "F32.9", "F33.0", "F33.1",
             "F33.2", "F33.40", "F33.41", "F33.42",
             "F33.9", "F32.89", "F33.8", "F43.21")

# identify diagnosis variables in mdcr data
diag_vars <- c(paste0("DIAG", 1:10))


# Assign icd codes --------------------------------------------------------
adrd_hosp <- adrd_hosp %>% 
  mutate(circ_any = as.numeric(if_any(diag_vars, ~. %in% icd_circ)),
         circ_prmy = ifelse(DIAG1 %in% icd_circ, 1, 0),
         resp_any = as.numeric(if_any(diag_vars, ~. %in% icd_resp)),
         resp_prmy = ifelse(DIAG1 %in% icd_resp, 1, 0),
         ptsd_any = as.numeric(if_any(diag_vars, ~. %in% icd_ptsd)),
         ptsd_prmy = ifelse(DIAG1 %in% icd_ptsd, 1, 0),
         anxty_any = as.numeric(if_any(diag_vars, ~. %in% icd_anxty)),
         anxty_prmy = ifelse(DIAG1 %in% icd_anxty, 1, 0),
         dep_any = as.numeric(if_any(diag_vars, ~. %in% icd_dep)),
         dep_prmy = ifelse(DIAG1 %in% icd_dep, 1, 0))

# vars to summarize
hosp_diags <- c(names(adrd_hosp)[endsWith(names(adrd_hosp), c("any"))],
                names(adrd_hosp)[endsWith(names(adrd_hosp), c("prmy"))])

# loop through each variable and use the table function to summarize
for (hosp_diag in hosp_diags) {
  cat(paste0("Summary for ", hosp_diag, ":\n"))
  print(table(adrd_hosp[[hosp_diag]]))
  cat("\n")
}



