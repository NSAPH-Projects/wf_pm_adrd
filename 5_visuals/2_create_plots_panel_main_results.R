# Create Tables and Plots to Show Model Results
# Goal: output tables and figures for wf pm - adrd hospitalization
# Vivian Do
# 9/27/23; updated 2/21/24
#* Create plots together in a panel once we decide on which ones should be in the final paper

####********************
#### Preparation #### 
####********************
rm(list=ls())
source(here::here("0_set_up", "1_libraries.R"))

# Set up filepath(s)
data_path <- '/n/dominici_nsaph_l3/Lab/projects/wfpm_adrd_hosp/data/'
output_path <- "/n/netscratch/dominici_nsaph/Lab/wfpm_adrd/output" # use scratch

####*********************************************************************************
#### Function to create estimate table and manuscript plots for model results #### 
####*********************************************************************************



####****************************************************************************** BEGIN FUNCTION


# Initialize function
plots_for_panel <- function(hosp_outcome, dataset, Analysis) {
  # hosp_outcome = "Respiratory"
  # dataset = cumul_resp_main
  # Analysis = "Main_resp"
  
  # For linear models (which have a "Label" variable in them that nonlinear data don't)
  if (!'Label' %in% dataset) {
    # Add Label variable for linear models
    dataset <- dataset %>% mutate(Label = 'Linear')
    
    if (!'...1' %in% dataset & !str_detect(Analysis, "14")) {
      # Pivot from wide to long
      dataset <- dataset %>%
        dplyr::select(Label, Counterfactual_wfpm, everything()) %>%
        pivot_longer(fit.or.lag0:uci.or.lag6,
                     names_to = "lag",
                     values_to = "estimate")
    }
    
    if ('...1' %in% dataset & !str_detect(Analysis, "14")) {
      # Pivot from wide to long
      dataset <- dataset %>%
        dplyr::select(-...1) %>%
        dplyr::select(Label, Counterfactual_wfpm, everything()) %>%
        pivot_longer(fit.or.lag0:uci.or.lag6,
                     names_to = "lag",
                     values_to = "estimate")
    }
    
    if (!'...1' %in% dataset & str_detect(Analysis, "14")) {
      # Pivot from wide to long
      dataset <- dataset %>%
        dplyr::select(Label, Counterfactual_wfpm, everything()) %>%
        pivot_longer(fit.or.lag0:uci.or.lag13,
                     names_to = "lag",
                     values_to = "estimate")
    }
    
    if ('...1' %in% dataset & str_detect(Analysis, "14")) {
      # Pivot from wide to long
      dataset <- dataset %>%
        dplyr::select(-...1) %>%
        dplyr::select(Label, Counterfactual_wfpm, everything()) %>%
        pivot_longer(fit.or.lag0:uci.or.lag13,
                     names_to = "lag",
                     values_to = "estimate")
    }
    
  }
  
  # For nonlinear models
  if (!!'Label' %in% dataset & !str_detect(Analysis, "14")) {
    # Pivot from wide to long
    dataset <- dataset %>%
      dplyr::select(-...1) %>%
      dplyr::select(Label, Counterfactual_wfpm, everything()) %>%
      pivot_longer(fit.or.lag0:uci.or.lag6,
                   names_to = "lag",
                   values_to = "estimate")
  }
  
  if (!!'Label' %in% dataset & str_detect(Analysis, "14")) {
    # Pivot from wide to long
    dataset <- dataset %>%
      dplyr::select(-...1) %>%
      dplyr::select(Label, Counterfactual_wfpm, everything()) %>%
      pivot_longer(fit.or.lag0:uci.or.lag13,
                   names_to = "lag",
                   values_to = "estimate")
  }
  
  # Separate lag variable into lag and estimate type, then convert back to wide
  dataset <- dataset %>%
    mutate(
      est_type = str_sub(lag, start = 1, end = 3),
      lag = str_replace(lag, "fit.or.lag", ""),
      lag = str_replace(lag, "lci.or.lag", ""),
      lag = str_replace(lag, "uci.or.lag", ""),
      lag = as.numeric(lag)
    ) %>%
    pivot_wider(names_from = est_type, values_from = estimate)
  
  ####*********************
  #### Create plots ####
  ####*********************
  # create breaks in exposure for y axis
  fit_range <- dataset %>%
    filter(Counterfactual_wfpm == 10) %>% # 10 unit increase
    mutate(break1 = min(lci),
           break4 = max(uci)) %>%
    filter(row_number() == 1) %>%
    mutate(break2 = break1 + (break4 - break1) / 3,
           break3 = break4 - (break4 - break1) / 3) %>%
    select(starts_with("break")) %>%
    mutate(across(where(is.numeric), ~ round(., digits = 2)))
  
  
  # 4a Plot of exposure response relationship, across lags
  if (hosp_outcome == "Anxiety" | hosp_outcome == "Depression"){
    expRespLags_cumul <- dataset %>%
      #filter(Label == "per95") %>%
      filter(Counterfactual_wfpm == 10) %>% # 10 unit increase
      ggplot(aes(x = lag, y = fit)) +
      geom_ribbon(aes(ymin = lci, ymax = uci), alpha = .5, fill = "gray75") +
      geom_line(alpha = .9,
                color = "black",
                size = 0.75) +
      geom_hline(yintercept = 1,
                 color = "black",
                 linetype = "dashed") +
      ylab("Rate ratio") + xlab('Daily lag') +
      labs(title = paste0(hosp_outcome)) +
      # scale_y_continuous(breaks = seq(0.995, 1.025, by = 0.005)) +
      theme_classic() +
      theme(
        text = element_text(size = 16),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        plot.title = element_text(hjust = 0.5)
      ) +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(breaks = c(0.85, 1.00, 1.15, 1.30), expand = expansion(mult = c(0, 0.05))) +
      coord_cartesian(ylim = c(0.85, 1.30)) 
    # scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    # ggh4x::facetted_pos_scales(y = list(scale_y_continuous(
    #   breaks = c(
    #     fit_range$break1,
    #     fit_range$break2,
    #     fit_range$break3,
    #     fit_range$break4
    #   )
    # )))
  }
  
  if (hosp_outcome == "Circulatory" | hosp_outcome == "Respiratory"){
    expRespLags_cumul <- dataset %>%
      #filter(Label == "per95") %>%
      filter(Counterfactual_wfpm == 10) %>% # 10 unit increase
      ggplot(aes(x = lag, y = fit)) +
      geom_ribbon(aes(ymin = lci, ymax = uci), alpha = .5, fill = "gray75") +
      geom_line(alpha = .9,
                color = "black",
                size = 0.75) +
      geom_hline(yintercept = 1,
                 color = "black",
                 linetype = "dashed") +
      ylab("Rate ratio") + xlab('Daily lag') +
      labs(title = paste0(hosp_outcome)) +
      # scale_y_continuous(breaks = seq(0.995, 1.025, by = 0.005)) +
      theme_classic() +
      theme(
        text = element_text(size = 16),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        plot.title = element_text(hjust = 0.5)
      ) +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(breaks = c(0.96, 0.98, 1.00, 1.02), expand = expansion(mult = c(0, 0.05))) +
      coord_cartesian(ylim = c(0.96, 1.02)) 
    # scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    # ggh4x::facetted_pos_scales(y = list(scale_y_continuous(
    #   breaks = c(
    #     fit_range$break1,
    #     fit_range$break2,
    #     fit_range$break3,
    #     fit_range$break4
    #   )
    # )))
  }
  
  return(expRespLags_cumul)
}
 

####****************************************************************************** END FUNCTION


# Read in data ------------------------------------------------------------
# circulatory
cumul_circ_main <- read_csv((paste0(output_path, '/Estimates/Main/',
                                    "EstCumul_zip_daily_wfpm_count_circ_prmy_out_Main_ERlin_LR4dfevenknots_Lag7.csv")))

# respiratory
cumul_resp_main <- read_csv((paste0(output_path, '/Estimates/Main/',
                                    "EstCumul_zip_daily_wfpm_count_resp_prmy_out_Main_ERlin_LR5dfevenknots_Lag7.csv")))

# anxiety
cumul_anxty_main <- read_csv((paste0(output_path, '/Estimates/Main/',
                                     "EstCumul_zip_daily_wfpm_count_anxty_prmy_out_Main_ERlin_LR5dfevenknots_Lag7.csv")))
# depression
cumul_dep_main <- read_csv((paste0(output_path, '/Estimates/Main/',
                                   "EstCumul_zip_daily_wfpm_count_dep_prmy_out_Main_ERlin_LR5dfevenknots_Lag7.csv")))

# Figure 2 - 4 hospitalization outcomes -----------------------------------

# anxiety
plot_anxiety <- plots_for_panel(hosp_outcome = "Anxiety", 
                                  dataset = cumul_anxty_main,
                                  Analysis = "Main_anxiety")

# circulatory
plot_circ <- plots_for_panel(hosp_outcome = "Circulatory", 
                                  dataset = cumul_circ_main,
                                  Analysis = "Main_circ")

# depression
plot_dep <- plots_for_panel(hosp_outcome = "Depression", 
                                dataset = cumul_dep_main,
                                Analysis = "Main_dep")

# respiratory
plot_resp <- plots_for_panel(hosp_outcome = "Respiratory", 
                            dataset = cumul_resp_main,
                            Analysis = "Main_resp")

# combine plots into one panel
plot_main <- (plot_circ + plot_resp) / (plot_anxiety + plot_dep)
plot_main <- plot_main + plot_annotation(tag_levels = "A")
plot_main

ggsave("fig2_results_main.png", 
       plot = plot_main, 
       path = "/n/dominici_nsaph_l3/Lab/projects/wfpm_adrd_hosp/figures/", 
       width = 11, height = 6, dpi = 300)


