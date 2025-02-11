# Create Tables and Plots to Show Model Results
# Goal: output tables and figures for wf pm - adrd hospitalization
# Vivian Do
# 9/27/23; updated 2/21/24; updated 5/3/24
#* Create plots together in a panel once we decide on which ones should be in the final paper

####********************
#### Preparation #### 
####********************
rm(list=ls())
source(here::here("0_set_up", "1_libraries.R"))

# Set up filepath(s)
data_path <- '/n/holyscratch01/LABS/dominici_nsaph/Lab/projects/wfpm_adrd_hosp/data/'
# output_path <-"/n/holyscratch01/LABS/dominici_nsaph/Lab/projects/wfpm_adrd_hosp/data/data_process/output"
output_path <- "/n/netscratch/dominici_nsaph/Lab/wfpm_adrd/output" # use scratch

####*********************************************************************************
#### Function to create estimate table and manuscript plots for model results #### 
####*********************************************************************************


library(MetBrewer)

# Initialize function
f_clean_data <- function(dataset, Analysis, hosp_outcome, em_category, em_subcategory){
  # hosp_outcome = "All-cause hospitalizations"
  # dataset = cumul_all_cause_main
  # Analysis = "Main_all_cause"
  
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
    pivot_wider(names_from = est_type, values_from = estimate) %>% 
    filter(Counterfactual_wfpm == 10) %>% 
    mutate(hosp_outcome = paste0(hosp_outcome),
           em_category = paste0(em_category),
           em_subcategory = paste0(em_subcategory))
  
  return(dataset)
}

# Read in data (effect modification results) ------------------------------------------------------------

# create data file with all the names bc there are so many outputs (ie - 60)
list_ind_vs_cumul <- c(rep("EstInd", 5), rep("EstCumul", 5))
list_exposure <- c(rep("ind_daily_wfpm"))
list_hosp_causes <- c(rep(c("all_cause_hosp_out", "circ_prmy_out", "resp_prmy_out", "anxty_prmy_out", "dep_prmy_out"), 2))
list_er <- c(rep(c("ERlinear", "ERlinear", "ERlinear", "ERlinear", "ERlinear"), 2))
list_lr <- c(rep(c("LR3dfevenknots", "LR4dfevenknots", "LR5dfevenknots", "LR5dfevenknots", "LR5dfevenknots")))
list_strat_levels <- c(
  "Female",
  "Male",
  # sex
  "urban",
  "rural",
  # urbanicity
  ">75",
  "<=75",
  # age
  "pov_lt20pct",
  "pov_gtet20pct",
  "Non-ED visit",
  "ED visit"
)

dta_model_name <- data.frame(list_ind_vs_cumul, list_exposure, list_hosp_causes, list_er, list_lr)
dta_strat_var <- data.frame(list_strat_levels)


dta_full_model_name <- merge(dta_model_name, dta_strat_var, by = NULL) %>%
  relocate(list_strat_levels, .after = list_hosp_causes) %>%
  unite(boo, everything(), sep = "_", remove = FALSE) %>% 
  mutate(clean_causes = case_when(grepl("all_cause", list_hosp_causes) ~ "All-cause",
                                  grepl("circ", list_hosp_causes) ~ "Circulatory",
                                  grepl("resp", list_hosp_causes) ~ "Respiratory",
                                  grepl("anxty", list_hosp_causes) ~ "Anxiety",
                                  grepl("dep", list_hosp_causes) ~ "Depression")) %>% 
  filter(clean_causes != "All-cause")

dataset_names <- data.frame(model_name = character(0))
# read in all the model estimates per each hospitalization outcome-stratification combination
for (row in 1:nrow(dta_full_model_name)){
  print(dta_full_model_name[row, 1])
  
  filename <- paste0(dta_full_model_name[row, 1])
  
  # Save dataset names so we can read in when creating plots
  dataset_names <- rbind(dataset_names, as.character((filename)))
  
  # Read in the dataset and assign it to the dynamically named variable
  assign(filename, read.csv(paste0(output_path, "/Estimates/Stratification/", filename, ".csv")))
  
}

# limit to only cumulative response
dataset_names <- dataset_names %>% 
  rename(model_name = names(.)[1]) %>% 
  filter(grepl("EstCumul", model_name),
         !grepl("all_cause", model_name, ignore.case = TRUE))
nrow(dataset_names) == 32 #should have 32 (whew thats a lot)

# Age ---------------------------------------------------------------------
data_age <- dataset_names %>% 
  filter(grepl("75", model_name, ignore.case = TRUE))
data_age <- as.list(data_age$model_name)

hosp_outcomes <- rep(c("Circulatory", "Respiratory", "Anxiety", "Depression"), 2)
em_category <- c("age")
em_subcategory <- c(rep(">75 years", 4), rep("≤75 years", 4))

est_lag_age <- data.frame()
for (d in 1:length(data_age)){
  
  data <- get(data_age[[d]])
  data_cleaned <- f_clean_data(dataset = data,
                               Analysis = "Main",
                               hosp_outcome = hosp_outcomes[d],
                               em_category = em_category,
                               em_subcategory = em_subcategory[d])
  
  est_lag_age <- rbind(est_lag_age, data_cleaned)
}

est_lag_age$em_subcategory <- factor(est_lag_age$em_subcategory, levels = c("≤75 years", ">75 years"))

fit_range <- est_lag_age %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 2))) %>% 
  group_by(hosp_outcome) %>% 
  mutate(break1 = min(lci),
         break4 = max(uci)) %>% 
  filter(row_number() == 1) %>%
  mutate(break2 = break1 + (break4 - break1)/3,
         break3 = break4 - (break4 - break1)/3) %>% 
  select(hosp_outcome, starts_with("break")) %>%
  mutate(across(where(is.numeric), ~ round(., digits = 2)))

est_lag_age %>% 
  mutate(hosp_outcome = factor(hosp_outcome, levels = c("Circulatory", "Respiratory", "Anxiety", "Depression"))) %>% 
  ggplot(aes(x = lag, y = fit)) +
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = .1) +
  geom_line(size = 0.75) +
  geom_hline(yintercept = 1, color = "black", linetype = "dashed") +
  ylab("Rate ratio") + 
  xlab('Daily lag') +
  labs(title = paste0("Cumulative rate ratio across lags for hospitalization types")) +
  theme_classic() +
  facet_grid(hosp_outcome ~ em_subcategory, scales = "free_y") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  ggh4x::facetted_pos_scales(
    y = list(
      hosp_outcome == "Anxiety" ~ scale_y_continuous(breaks = c(fit_range[fit_range$hosp_outcome == "Anxiety", ]$break1,
                                                                fit_range[fit_range$hosp_outcome == "Anxiety", ]$break2,
                                                                fit_range[fit_range$hosp_outcome == "Anxiety", ]$break3,
                                                                fit_range[fit_range$hosp_outcome == "Anxiety", ]$break4)),
      hosp_outcome == "Circulatory" ~ scale_y_continuous(breaks = c(fit_range[fit_range$hosp_outcome == "Circulatory", ]$break1,
                                                                fit_range[fit_range$hosp_outcome == "Circulatory", ]$break2,
                                                                fit_range[fit_range$hosp_outcome == "Circulatory", ]$break3,
                                                                fit_range[fit_range$hosp_outcome == "Circulatory", ]$break4)),
      hosp_outcome == "Depression" ~ scale_y_continuous(breaks = c(fit_range[fit_range$hosp_outcome == "Depression", ]$break1,
                                                                fit_range[fit_range$hosp_outcome == "Depression", ]$break2,
                                                                fit_range[fit_range$hosp_outcome == "Depression", ]$break3,
                                                                fit_range[fit_range$hosp_outcome == "Depression", ]$break4)),
      hosp_outcome == "Respiratory" ~ scale_y_continuous(breaks = c(fit_range[fit_range$hosp_outcome == "Respiratory", ]$break1,
                                                                fit_range[fit_range$hosp_outcome == "Respiratory", ]$break2,
                                                                fit_range[fit_range$hosp_outcome == "Respiratory", ]$break3,
                                                                fit_range[fit_range$hosp_outcome == "Respiratory", ]$break4))
    )) +
  theme(
    axis.title = element_text(size = 16),  # Adjust the text size for axis titles
    axis.text = element_text(size = 14),   # Adjust the text size for axis labels
    strip.text = element_text(size = 14),  # Adjust the text size for facet labels
    plot.title = element_text(size = 20),   # Adjust the text size for the plot title
    panel.spacing = unit(0.75, "lines") 
  )

ggsave("fig_em_age.png", 
       path = "/n/dominici_nsaph_l3/Lab/projects/wfpm_adrd_hosp/figures/", 
       width = 11, height = 6, dpi = 300)

# Sex ---------------------------------------------------------------------
data_sex <- dataset_names %>% 
  filter(grepl("Female|Male", model_name, ignore.case = TRUE))
data_sex <- as.list(data_sex$model_name)

hosp_outcomes <- rep(c("Circulatory", "Respiratory", "Anxiety", "Depression"), 2)
em_category <- c("sex")
em_subcategory <- c(rep("Female", 4), rep("Male", 4))

est_lag_sex <- data.frame()
for (d in 1:length(data_sex)){
  
  data <- get(data_sex[[d]])
  data_cleaned <- f_clean_data(dataset = data,
                               Analysis = "Main",
                               hosp_outcome = hosp_outcomes[d],
                               em_category = em_category,
                               em_subcategory = em_subcategory[d])
  
  est_lag_sex <- rbind(est_lag_sex, data_cleaned)
}

fit_range <- est_lag_sex %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 5))) %>% 
  group_by(hosp_outcome) %>% 
  mutate(break1 = min(lci),
         break4 = max(uci)) %>% 
  filter(row_number() == 1) %>%
  mutate(break2 = break1 + (break4 - break1)/3,
         break3 = break4 - (break4 - break1)/3) %>% 
  select(hosp_outcome, starts_with("break")) %>%
  mutate(across(where(is.numeric), ~ round(., digits = 2)))

est_lag_sex %>% 
  mutate(hosp_outcome = factor(hosp_outcome, levels = c("Circulatory", "Respiratory", "Anxiety", "Depression"))) %>% 
  ggplot(aes(x = lag, y = fit)) +
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = .1) +
  geom_line(size = 0.75) +
  geom_hline(yintercept = 1, color = "black", linetype = "dashed") +
  ylab("Rate ratio") + 
  xlab('Daily lag') +
  labs(title = paste0("Cumulative rate ratio across lags for hospitalization types")) +
  theme_classic() +
  facet_grid(hosp_outcome ~ em_subcategory, scales = "free_y") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  ggh4x::facetted_pos_scales(
    y = list(
      hosp_outcome == "Anxiety" ~ scale_y_continuous(breaks = c(fit_range[fit_range$hosp_outcome == "Anxiety", ]$break1,
                                                                fit_range[fit_range$hosp_outcome == "Anxiety", ]$break2,
                                                                fit_range[fit_range$hosp_outcome == "Anxiety", ]$break3,
                                                                fit_range[fit_range$hosp_outcome == "Anxiety", ]$break4)),
      hosp_outcome == "Circulatory" ~ scale_y_continuous(breaks = c(0.97, #96 is out of range so there will be weird ticks with rounding at 2 decimals
                                                                    0.99,
                                                                    fit_range[fit_range$hosp_outcome == "Circulatory", ]$break3,
                                                                    fit_range[fit_range$hosp_outcome == "Circulatory", ]$break4)),
      hosp_outcome == "Depression" ~ scale_y_continuous(breaks = c(fit_range[fit_range$hosp_outcome == "Depression", ]$break1,
                                                                   fit_range[fit_range$hosp_outcome == "Depression", ]$break2,
                                                                   fit_range[fit_range$hosp_outcome == "Depression", ]$break3,
                                                                   fit_range[fit_range$hosp_outcome == "Depression", ]$break4)),
      hosp_outcome == "Respiratory" ~ scale_y_continuous(breaks = c(fit_range[fit_range$hosp_outcome == "Respiratory", ]$break1,
                                                                    fit_range[fit_range$hosp_outcome == "Respiratory", ]$break2,
                                                                    fit_range[fit_range$hosp_outcome == "Respiratory", ]$break3,
                                                                    fit_range[fit_range$hosp_outcome == "Respiratory", ]$break4))
    )) +
  theme(
    axis.title = element_text(size = 16),  # Adjust the text size for axis titles
    axis.text = element_text(size = 14),   # Adjust the text size for axis labels
    strip.text = element_text(size = 14),  # Adjust the text size for facet labels
    plot.title = element_text(size = 20),   # Adjust the text size for the plot title
    panel.spacing = unit(0.75, "lines") 
  )

ggsave("fig_em_sex.png", 
       path = "/n/dominici_nsaph_l3/Lab/projects/wfpm_adrd_hosp/figures/", 
       width = 11, height = 6, dpi = 300)

# Urbanicity ---------------------------------------------------------------------
data_urbanicity <- dataset_names %>% 
  filter(grepl("urban|rural", model_name, ignore.case = TRUE))
data_urbanicity <- as.list(data_urbanicity$model_name)

hosp_outcomes <- rep(c("Circulatory", "Respiratory", "Anxiety", "Depression"), 2)
em_category <- c("urbanicity")
em_subcategory <- c(rep("Urban", 4), rep("Rural", 4))

est_lag_urbanicity <- data.frame()
for (d in 1:length(data_urbanicity)){
  
  data <- get(data_urbanicity[[d]])
  data_cleaned <- f_clean_data(dataset = data,
                               Analysis = "Main",
                               hosp_outcome = hosp_outcomes[d],
                               em_category = em_category,
                               em_subcategory = em_subcategory[d])
  
  est_lag_urbanicity <- rbind(est_lag_urbanicity, data_cleaned)
}

est_lag_urbanicity$em_subcategory <- factor(est_lag_urbanicity$em_subcategory, levels = c("Urban", "Rural"))

fit_range <- est_lag_urbanicity %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 2))) %>% 
  group_by(hosp_outcome) %>% 
  mutate(break1 = min(lci),
         break4 = max(uci)) %>% 
  filter(row_number() == 1) %>%
  mutate(break2 = break1 + (break4 - break1)/3,
         break3 = break4 - (break4 - break1)/3) %>% 
  select(hosp_outcome, starts_with("break")) %>%
  mutate(across(where(is.numeric), ~ round(., digits = 2)))


est_lag_urbanicity %>% 
  mutate(hosp_outcome = factor(hosp_outcome, levels = c("Circulatory", "Respiratory", "Anxiety", "Depression"))) %>% 
  ggplot(aes(x = lag, y = fit)) +
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = .1) +
  geom_line(size = 0.75) +
  geom_hline(yintercept = 1, color = "black", linetype = "dashed") +
  ylab("Rate ratio") + 
  xlab('Daily lag') +
  labs(title = paste0("Cumulative rate ratio across lags for hospitalization types")) +
  theme_classic() +
  facet_grid(hosp_outcome ~ em_subcategory, scales = "free_y") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  ggh4x::facetted_pos_scales(
    y = list(
      hosp_outcome == "Anxiety" ~ scale_y_continuous(breaks = c(fit_range[fit_range$hosp_outcome == "Anxiety", ]$break1,
                                                                fit_range[fit_range$hosp_outcome == "Anxiety", ]$break2,
                                                                fit_range[fit_range$hosp_outcome == "Anxiety", ]$break3,
                                                                fit_range[fit_range$hosp_outcome == "Anxiety", ]$break4)),
      hosp_outcome == "Circulatory" ~ scale_y_continuous(breaks = c(fit_range[fit_range$hosp_outcome == "Circulatory", ]$break1, 
                                                                    fit_range[fit_range$hosp_outcome == "Circulatory", ]$break2,
                                                                    fit_range[fit_range$hosp_outcome == "Circulatory", ]$break3,
                                                                    fit_range[fit_range$hosp_outcome == "Circulatory", ]$break4)),
      hosp_outcome == "Depression" ~ scale_y_continuous(breaks = c(fit_range[fit_range$hosp_outcome == "Depression", ]$break1,
                                                                   fit_range[fit_range$hosp_outcome == "Depression", ]$break2,
                                                                   fit_range[fit_range$hosp_outcome == "Depression", ]$break3,
                                                                   fit_range[fit_range$hosp_outcome == "Depression", ]$break4)),
      hosp_outcome == "Respiratory" ~ scale_y_continuous(breaks = c(fit_range[fit_range$hosp_outcome == "Respiratory", ]$break1,
                                                                    fit_range[fit_range$hosp_outcome == "Respiratory", ]$break2,
                                                                    fit_range[fit_range$hosp_outcome == "Respiratory", ]$break3,
                                                                    fit_range[fit_range$hosp_outcome == "Respiratory", ]$break4))
    )) +
  theme(
    axis.title = element_text(size = 16),  # Adjust the text size for axis titles
    axis.text = element_text(size = 14),   # Adjust the text size for axis labels
    strip.text = element_text(size = 14),  # Adjust the text size for facet labels
    plot.title = element_text(size = 20),   # Adjust the text size for the plot title
    panel.spacing = unit(0.75, "lines") 
  )

ggsave("fig_em_urbanicity.png", 
       path = "/n/dominici_nsaph_l3/Lab/projects/wfpm_adrd_hosp/figures/", 
       width = 11, height = 6, dpi = 300)

# Poverty ---------------------------------------------------------------------
data_poverty <- dataset_names %>% 
  filter(grepl("pov", model_name, ignore.case = TRUE))
data_poverty <- as.list(data_poverty$model_name)

hosp_outcomes <- rep(c("Circulatory", "Respiratory", "Anxiety", "Depression"), 2)
em_category <- c("Poverty")
em_subcategory <- c(rep("Poverty <20%", 4), rep("Poverty ≥20%", 4))

est_lag_poverty <- data.frame()
for (d in 1:length(data_poverty)){
  
  data <- get(data_poverty[[d]])
  data_cleaned <- f_clean_data(dataset = data,
                               Analysis = "Main",
                               hosp_outcome = hosp_outcomes[d],
                               em_category = em_category,
                               em_subcategory = em_subcategory[d])
  
  est_lag_poverty <- rbind(est_lag_poverty, data_cleaned)
}

fit_range <- est_lag_poverty %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 2))) %>% 
  group_by(hosp_outcome) %>% 
  mutate(break1 = min(lci),
         break4 = max(uci)) %>% 
  filter(row_number() == 1) %>%
  mutate(break2 = break1 + (break4 - break1)/3,
         break3 = break4 - (break4 - break1)/3) %>% 
  select(hosp_outcome, starts_with("break")) %>%
  mutate(across(where(is.numeric), ~ round(., digits = 2)))

est_lag_poverty %>% 
  mutate(hosp_outcome = factor(hosp_outcome, levels = c("Circulatory", "Respiratory", "Anxiety", "Depression"))) %>% 
  ggplot(aes(x = lag, y = fit)) +
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = .1) +
  geom_line(size = 0.75) +
  geom_hline(yintercept = 1, color = "black", linetype = "dashed") +
  ylab("Rate ratio") + 
  xlab('Daily lag') +
  labs(title = paste0("Cumulative rate ratio across lags for hospitalization types")) +
  theme_classic() +
  facet_grid(hosp_outcome ~ em_subcategory, scales = "free_y") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  ggh4x::facetted_pos_scales(
    y = list(
      hosp_outcome == "Anxiety" ~ scale_y_continuous(breaks = c(fit_range[fit_range$hosp_outcome == "Anxiety", ]$break1,
                                                                fit_range[fit_range$hosp_outcome == "Anxiety", ]$break2,
                                                                fit_range[fit_range$hosp_outcome == "Anxiety", ]$break3,
                                                                fit_range[fit_range$hosp_outcome == "Anxiety", ]$break4)),
      hosp_outcome == "Circulatory" ~ scale_y_continuous(breaks = c(fit_range[fit_range$hosp_outcome == "Circulatory", ]$break1, #SOMETHING LOOKS WEIRD - SPACING
                                                                    fit_range[fit_range$hosp_outcome == "Circulatory", ]$break2,
                                                                    fit_range[fit_range$hosp_outcome == "Circulatory", ]$break3,
                                                                    fit_range[fit_range$hosp_outcome == "Circulatory", ]$break4)),
      hosp_outcome == "Depression" ~ scale_y_continuous(breaks = c(fit_range[fit_range$hosp_outcome == "Depression", ]$break1,
                                                                   fit_range[fit_range$hosp_outcome == "Depression", ]$break2,
                                                                   fit_range[fit_range$hosp_outcome == "Depression", ]$break3,
                                                                   fit_range[fit_range$hosp_outcome == "Depression", ]$break4)),
      hosp_outcome == "Respiratory" ~ scale_y_continuous(breaks = c(fit_range[fit_range$hosp_outcome == "Respiratory", ]$break1,
                                                                    fit_range[fit_range$hosp_outcome == "Respiratory", ]$break2,
                                                                    fit_range[fit_range$hosp_outcome == "Respiratory", ]$break3,
                                                                    fit_range[fit_range$hosp_outcome == "Respiratory", ]$break4))
    )) +
  theme(
    axis.title = element_text(size = 16),  # Adjust the text size for axis titles
    axis.text = element_text(size = 14),   # Adjust the text size for axis labels
    strip.text = element_text(size = 14),  # Adjust the text size for facet labels
    plot.title = element_text(size = 20),   # Adjust the text size for the plot title
    panel.spacing = unit(0.75, "lines") 
  )

ggsave("fig_em_pov.png", 
       path = "/n/dominici_nsaph_l3/Lab/projects/wfpm_adrd_hosp/figures/", 
       width = 11, height = 6, dpi = 300)


# ED visit ---------------------------------------------------------------------
data_edvisit <- dataset_names %>% 
  filter(grepl("visit", model_name, ignore.case = TRUE))
data_edvisit <- as.list(data_edvisit$model_name)

hosp_outcomes <- rep(c("Circulatory", "Respiratory", "Anxiety", "Depression"), 2)
em_category <- c("ED visit status")
em_subcategory <- c(rep("Non-emergency hospitalization", 4), rep("Emergency hospitalization", 4))

est_lag_edvisit <- data.frame()
for (d in 1:length(data_edvisit)){
  
  data <- get(data_edvisit[[d]])
  data_cleaned <- f_clean_data(dataset = data,
                               Analysis = "Main",
                               hosp_outcome = hosp_outcomes[d],
                               em_category = em_category,
                               em_subcategory = em_subcategory[d])
  
  est_lag_edvisit <- rbind(est_lag_edvisit, data_cleaned)
}

fit_range <- est_lag_edvisit %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 2))) %>% 
  group_by(hosp_outcome) %>% 
  mutate(break1 = min(lci),
         break4 = max(uci)) %>% 
  filter(row_number() == 1) %>%
  mutate(break2 = break1 + (break4 - break1)/3,
         break3 = break4 - (break4 - break1)/3) %>% 
  select(hosp_outcome, starts_with("break")) %>%
  mutate(across(where(is.numeric), ~ round(., digits = 2)))

est_lag_edvisit %>% 
  mutate(hosp_outcome = factor(hosp_outcome, levels = c("Circulatory", "Respiratory", "Anxiety", "Depression"))) %>% 
  ggplot(aes(x = lag, y = fit)) +
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = .1) +
  geom_line(size = 0.75) +
  geom_hline(yintercept = 1, color = "black", linetype = "dashed") +
  ylab("Rate ratio") + 
  xlab('Daily lag') +
  labs(title = paste0("Cumulative rate ratio across lags for hospitalization types")) +
  theme_classic() +
  facet_grid(hosp_outcome ~ em_subcategory, scales = "free_y") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  ggh4x::facetted_pos_scales(
    y = list(
      hosp_outcome == "Anxiety" ~ scale_y_continuous(breaks = c(fit_range[fit_range$hosp_outcome == "Anxiety", ]$break1,
                                                                fit_range[fit_range$hosp_outcome == "Anxiety", ]$break2,
                                                                fit_range[fit_range$hosp_outcome == "Anxiety", ]$break3,
                                                                fit_range[fit_range$hosp_outcome == "Anxiety", ]$break4)),
      hosp_outcome == "Circulatory" ~ scale_y_continuous(breaks = c(fit_range[fit_range$hosp_outcome == "Circulatory", ]$break1, #SOMETHING LOOKS WEIRD - SPACING
                                                                    fit_range[fit_range$hosp_outcome == "Circulatory", ]$break2,
                                                                    fit_range[fit_range$hosp_outcome == "Circulatory", ]$break3,
                                                                    fit_range[fit_range$hosp_outcome == "Circulatory", ]$break4)),
      hosp_outcome == "Depression" ~ scale_y_continuous(breaks = c(fit_range[fit_range$hosp_outcome == "Depression", ]$break1,
                                                                   fit_range[fit_range$hosp_outcome == "Depression", ]$break2,
                                                                   fit_range[fit_range$hosp_outcome == "Depression", ]$break3,
                                                                   fit_range[fit_range$hosp_outcome == "Depression", ]$break4)),
      hosp_outcome == "Respiratory" ~ scale_y_continuous(breaks = c(fit_range[fit_range$hosp_outcome == "Respiratory", ]$break1,
                                                                    fit_range[fit_range$hosp_outcome == "Respiratory", ]$break2,
                                                                    fit_range[fit_range$hosp_outcome == "Respiratory", ]$break3,
                                                                    fit_range[fit_range$hosp_outcome == "Respiratory", ]$break4))
    )) +
  theme(
    axis.title = element_text(size = 16),  # Adjust the text size for axis titles
    axis.text = element_text(size = 14),   # Adjust the text size for axis labels
    strip.text = element_text(size = 14),  # Adjust the text size for facet labels
    plot.title = element_text(size = 20),   # Adjust the text size for the plot title
    panel.spacing = unit(0.75, "lines") 
  )

ggsave("fig_em_edvisit.png", 
       path = "/n/dominici_nsaph_l3/Lab/projects/wfpm_adrd_hosp/figures/", 
       width = 11, height = 9, dpi = 300)

