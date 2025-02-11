# Create Tables and Plots to Show Model Results
# Goal: output tables and figures for wf pm - adrd hospitalization
# Vivian Do
# 1/27/25
#* Create plots together in a panel of individual lags (not cumulative)

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
  # dataset = ind_resp_main
  # Analysis = "Ind_resp"

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
  
  # 4a Plot of exposure response relationship, across lags
  expRespLags_ind <- dataset %>%
    filter(lag == 0 | lag == 3 | lag == 6) %>%
    mutate(lag = case_when(lag == 0 ~ "Lag 0",
                           lag == 3 ~ "Lag 3",
                           lag == 6 ~ "Lag 6"),
           Lag = factor(lag)) %>% 
    ggplot(aes(x = Counterfactual_wfpm, y = fit)) +
    geom_ribbon(aes(ymin = lci, ymax = uci, fill = Lag), alpha = .5) + 
    
    # this section is where we manually add colors to each of the lags
    scale_fill_manual(values = met.brewer("Archambault", direction=-1, 3)) +
    scale_color_manual(values = met.brewer("Archambault", direction=-1, 3)) +
    # end section
    
    geom_line(aes(color = Lag), alpha = .9, size = 0.75) +
    geom_hline(yintercept = 1,
               color = "black",
               linetype = "dashed") +
    ylab("Rate ratio") + 
    xlab(expression("Average Wildfire PM"[2.5] ~ "(μg/m"^3*")")) +
    labs(title = paste0(hosp_outcome)) +
    theme_classic() +
    theme(
      text = element_text(size = 12),
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 12),
      plot.title = element_text(hjust = 0.5),
      legend.position = "none"
    ) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(breaks = c(0.5, 0.75, 1.0, 1.25, 1.5), expand = expansion(mult = c(0, 0.05))) +
    coord_cartesian(ylim = c(0.5, 1.5)) +  # Limit the visible range of the y-axis
    facet_grid(~Lag)
  
  # plot(expRespLags_ind)
  return(expRespLags_ind)
}


####****************************************************************************** END FUNCTION


# Read in data ------------------------------------------------------------
# circulatory
ind_circ_main <- read_csv((paste0(output_path, '/Estimates/Main/',
                                    "EstInd_zip_daily_wfpm_count_circ_prmy_out_Main_ERlin_LR4dfevenknots_Lag7.csv")))

# respiratory
ind_resp_main <- read_csv((paste0(output_path, '/Estimates/Main/',
                                    "EstInd_zip_daily_wfpm_count_resp_prmy_out_Main_ERlin_LR5dfevenknots_Lag7.csv")))

# anxiety
ind_anxty_main <- read_csv((paste0(output_path, '/Estimates/Main/',
                                     "EstInd_zip_daily_wfpm_count_anxty_prmy_out_Main_ERlin_LR5dfevenknots_Lag7.csv")))

# depression
ind_dep_main <- read_csv((paste0(output_path, '/Estimates/Main/',
                                   "EstInd_zip_daily_wfpm_count_dep_prmy_out_Main_ERlin_LR5dfevenknots_Lag7.csv")))

# Figure 2 - 4 hospitalization outcomes -----------------------------------

# anxiety
plot_anxiety <- plots_for_panel(hosp_outcome = "Anxiety", 
                                dataset = ind_anxty_main,
                                Analysis = "Ind_anxiety")

# circulatory
plot_circ <- plots_for_panel(hosp_outcome = "Circulatory", 
                             dataset = ind_circ_main,
                             Analysis = "Ind_circ")

# depression
plot_dep <- plots_for_panel(hosp_outcome = "Depression", 
                            dataset = ind_dep_main,
                            Analysis = "Ind_dep")

# respiratory
plot_resp <- plots_for_panel(hosp_outcome = "Respiratory", 
                             dataset = ind_resp_main,
                             Analysis = "Ind_resp")

# combine plots into one panel
plot_main <- (plot_circ + plot_resp) / (plot_anxiety + plot_dep)
plot_main <- plot_main + plot_annotation(tag_levels = "A")
plot_main

ggsave("supp_fig_results_ind.png", 
       plot = plot_main, 
       path = "/n/dominici_nsaph_l3/Lab/projects/wfpm_adrd_hosp/figures/", 
       width = 11, height = 6, dpi = 300)



