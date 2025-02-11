# Create Tables and Plots to Show Model Results
# Goal: output tables and figures for wf pm - adrd hospitalization
# Vivian Do
# 12/16/23
#* In this script, plots that show the exposure-response relationship across lags
#* and wfpm concentrations are created for the manuscript and supplementary material.

####********************
#### Preparation #### 
####********************
rm(list=ls())
source(here::here("0_set_up", "1_libraries.R"))

# Set up filepath(s)
data_path <- '/n/dominici_nsaph_l3/Lab/projects/wfpm_adrd_hosp/data/'
output_path <-"/n/dominici_nsaph_l3/Lab/projects/wfpm_adrd_hosp/data/output_mortality"

list.files(paste0(output_path, "/Estimates/Main"))
list.files(paste0(output_path, "/Estimates/Stratification"))
list.files(paste0(output_path, "/Estimates/Sensitivity"))

####*********************************************************************************
#### Function to create estimate table and manuscript plots for model results #### 
####*********************************************************************************

####****************************************************************************** BEGIN FUNCTION

# Initialize function
table_plot <- function(hosp_outcome, dataset, CumulInd, Analysis, FigNum){
  # hosp_outcome = "All-cause hospitalizations"
  # dataset = cumul_all_cause_main
  # CumulInd = "Cumul"
  # Analysis = "Main_all_cause"
  # FigNum = "1"
  
  # For linear models (which have a "Label" variable in them that nonlinear data don't)
  if(!'Label' %in% dataset){
    
    # Add Label variable for linear models
    dataset <- dataset %>% mutate(Label = 'Linear')
    
    if(!'...1' %in% dataset & !str_detect(Analysis, "14")){
      # Pivot from wide to long 
      dataset <- dataset %>% 
        dplyr::select(Label, Counterfactual_wfpm, everything()) %>% 
        pivot_longer(fit.or.lag0:uci.or.lag6, names_to = "lag", values_to = "estimate")
    }
    
    if('...1' %in% dataset & !str_detect(Analysis, "14")){
      # Pivot from wide to long 
      dataset <- dataset %>% 
        dplyr::select(-...1) %>% 
        dplyr::select(Label, Counterfactual_wfpm, everything()) %>% 
        pivot_longer(fit.or.lag0:uci.or.lag6, names_to = "lag", values_to = "estimate")
    }
    
    if(!'...1' %in% dataset & str_detect(Analysis, "14")){
      # Pivot from wide to long 
      dataset <- dataset %>% 
        dplyr::select(Label, Counterfactual_wfpm, everything()) %>% 
        pivot_longer(fit.or.lag0:uci.or.lag13, names_to = "lag", values_to = "estimate")
    }
    
    if('...1' %in% dataset & str_detect(Analysis, "14")){
      # Pivot from wide to long 
      dataset <- dataset %>% 
        dplyr::select(-...1) %>% 
        dplyr::select(Label, Counterfactual_wfpm, everything()) %>% 
        pivot_longer(fit.or.lag0:uci.or.lag13, names_to = "lag", values_to = "estimate")
    }
    
  }
  
  # For nonlinear models
  if(!!'Label' %in% dataset & !str_detect(Analysis, "14")){
    # Pivot from wide to long 
    dataset <- dataset %>% 
      dplyr::select(-...1) %>% 
      dplyr::select(Label, Counterfactual_wfpm, everything()) %>% 
      pivot_longer(fit.or.lag0:uci.or.lag6, names_to = "lag", values_to = "estimate")
  }
  
  if(!!'Label' %in% dataset & str_detect(Analysis, "14")){
    # Pivot from wide to long 
    dataset <- dataset %>% 
      dplyr::select(-...1) %>% 
      dplyr::select(Label, Counterfactual_wfpm, everything()) %>% 
      pivot_longer(fit.or.lag0:uci.or.lag13, names_to = "lag", values_to = "estimate")
  }
  
  # Separate lag variable into lag and estimate type, then convert back to wide
  dataset <- dataset %>% 
    mutate(est_type = str_sub(lag, start = 1, end = 3),
           lag = str_replace(lag, "fit.or.lag", ""),
           lag = str_replace(lag, "lci.or.lag", ""),
           lag = str_replace(lag, "uci.or.lag", ""),
           lag = as.numeric(lag)) %>% 
    pivot_wider(names_from = est_type, values_from = estimate)
  
  
  ####*********************
  #### Create table #### 
  ####*********************
  
  # Table of effect estimates
  est_table <- dataset %>% 
    filter(Label != "ERValues") %>% 
    mutate(sig = case_when(
      lci < 1 & uci < 1 ~ "sig",
      lci > 1 & uci > 1 ~ "sig",
      lci < 1 & uci > 1 ~ "not sig"
    ))
  
  # rename table according to cumul/ind
  if(str_detect(CumulInd, 'Cumul')){
    .GlobalEnv$est_table_cumul <- est_table}
  if(str_detect(CumulInd, 'Ind')){
    .GlobalEnv$est_table_ind <- est_table}
  rm(est_table)
  
  
  ####*********************
  #### Create plots #### 
  ####*********************
  
  if(str_detect(CumulInd, 'Cumul')){
    
    # 4a Plot of exposure response relationship, across lags 
    expRespLags_cumul <- dataset %>% 
      #filter(Label == "per95") %>% 
      filter(Counterfactual_wfpm == 10) %>% # 10 unit increase
      ggplot(aes(x = lag, y = fit)) +
      geom_ribbon(aes(ymin = lci, ymax = uci), alpha = .5, fill = "gray75") +         
      geom_line(alpha = .9, color = "black", size = 1) +
      geom_hline(yintercept = 1, color = "black", linetype = "dashed") + 
      ylab(paste0("Rate ratio")) + xlab('Daily lag') +
      # scale_y_continuous(breaks = seq(0.995, 1.025, by = 0.005)) +
      scale_x_continuous(breaks = unique(dataset$lag)) +  # Ensure all x values are shown
      theme_classic() +
      theme(text = element_text(size = 16)) +
      ggtitle("Cumulative exposure response")
    
    if(grepl("Main", Analysis, ignore.case = TRUE)){
      tiff(paste0(output_path, '/Plots/Main/', 'fig', FigNum, '_expRespLags_cumul_',
                  Analysis, '.tif'),
           units = "in", width = 8, height = 6, res = 300)
      print(expRespLags_cumul)
      dev.off()
    }
    
    if(grepl("Strat", Analysis, ignore.case = TRUE)){
      expRespLags_cumul <- expRespLags_cumul + 
        ggtitle(paste0("Cumulative exposure response: ", Analysis))
      
      tiff(paste0(output_path, '/Plots/Stratification/', 'fig', FigNum, '_expRespLags_cumul_',
                  Analysis, '.tif'),
           units = "in", width = 8, height = 6, res = 300)
      print(expRespLags_cumul)
      dev.off()
    }
    
    if(grepl("Sens", Analysis, ignore.case = TRUE)){
      expRespLags_cumul <- expRespLags_cumul + 
        ggtitle(paste0("Cumulative exposure response: ", Analysis))
      
      tiff(paste0(output_path, '/Plots/Sensitivity/', 'fig', FigNum, '_expRespLags_cumul_',
                  Analysis, '.tif'),
           units = "in", width = 8, height = 6, res = 300)
      print(expRespLags_cumul)
      dev.off()
    }
    
  }
  
  if(str_detect(CumulInd, 'Ind')){
    
    FigNum1 = str_split(FigNum, '_')[[1]][1]
    FigNum2 = str_split(FigNum, '_')[[1]][2]
    FigNum3 = str_split(FigNum, '_')[[1]][3]
    
    # Plot of exposure response relationship, across lags 
    expRespLags_ind <- dataset %>%
      #filter(Label == "per95") %>%
      filter(Counterfactual_wfpm == 10) %>%
      ggplot(aes(x = lag, y = fit)) +
      geom_line(alpha = .9, color = "#800000FF", size = 1) +
      geom_ribbon(aes(ymin = lci, ymax = uci), alpha = .5, fill = "gray75") +
      geom_hline(yintercept = 1, color = "black", linetype = "dashed") +
      ylab(paste0("Rate Ratio")) + xlab('Daily Lag') +
      # scale_y_continuous(limits = c(0.995, 1.008),
      #                    breaks = seq(1, 1.02, by = 0.005)) +
      scale_x_continuous(breaks = unique(dataset$lag)) + # Ensure all x values are shown
      theme_classic() +
      theme(text = element_text(size = 16)) +
      ggtitle("Individual exposure response across lags")
    
    if(grepl("Main", Analysis, ignore.case = TRUE)){
      tiff(paste0(output_path, '/Plots/Main/', 'fig', FigNum1, '_expRespLags_ind_',
                  Analysis, '.tif'),
           units = "in", width = 11, height = 6, dpi = 300)
      print(expRespLags_ind)
      dev.off()
    }
    
    if(grepl("Strat", Analysis, ignore.case = TRUE)){
      expRespLags_ind <- expRespLags_ind +
        ggtitle(paste0("Individual exposure response across lags: ", Analysis))
      
      tiff(paste0(output_path, '/Plots/Stratification/', 'fig', FigNum1, '_expRespLags_ind_',
                  Analysis, '.tif'),
           units = "in", width = 11, height = 6, dpi = 300)
      print(expRespLags_ind)
      dev.off()
    }
    
    if(grepl("Sens", Analysis, ignore.case = TRUE)){
      expRespLags_ind <- expRespLags_ind +
        ggtitle(paste0("Individual exposure response across lags: ", Analysis))
      
      tiff(paste0(output_path, '/Plots/Sensitivity/', 'fig', FigNum1, '_expRespLags_ind_',
                  Analysis, '.tif'),
           units = "in", width = 11, height = 6, dpi = 300)
      print(expRespLags_ind)
      dev.off()
    }
  }
}


####****************************************************************************** END FUNCTION


####*********************
#### MAIN analyses #### 
####*********************

# read in data
# mortality/death
cumul_mortality <- read_csv((paste0(output_path, '/Estimates/Main/',
                                    "EstCumul_zip_daily_wfpm_count_death_Main_ERlin_LR5dfevenknots_Lag7.csv")))

# ind_mortality <- read_csv((paste0(output_path, '/Estimates/Main/',
#                                     "EstInd_zip_daily_wfpm_count_death_Main_ERlin_LR5dfevenknots_Lag7.csv")))

# create plots
# mortality; cumulative
table_plot(hosp_outcome = "Mortality", 
           dataset = cumul_mortality,
           CumulInd = "Cumul",
           Analysis = "Main",
           FigNum = "1")









