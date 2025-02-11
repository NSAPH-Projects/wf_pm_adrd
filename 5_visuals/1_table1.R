# Generate a table 1 for study
# Vivian Do
# 7/19/23 ; updated 4/26/24

# Preparation -------------------------------------------------------------
# Load Packages
rm(list = ls())
source(here::here("0_set_up", "1_libraries.R"))

# Set up filepath(s)
data_path <-
  '/n/dominici_nsaph_l3/Lab/projects/wfpm_adrd_hosp/data/'
data_path_stratification_vars <-
  paste0(data_path, "data_process/stratification_vars/")
data_path_data4casecross <-
  "/n/dominici_nsaph_l3/Lab/projects/wfpm_adrd_hosp/data/data_process/data4casecross/"
output_path <- paste0(data_path, "output")
outpath_table1 <-
  "/n/dominici_nsaph_l3/Lab/projects/wfpm_adrd_hosp/results/table1/"


# read in data ------------------------------------------------------------
# hospitalizations/exposure data
data4casecross_ind_final <-
  read_fst(
    '/n/dominici_nsaph_l3/Lab/projects/wfpm_adrd_hosp/data/data_process/data4casecross/data_w_all_vars/data4casecross_ind.fst'
  )

# poverty and urbanicity variables for cohort
strat_pov_urbanicity <-
  read_csv(paste0(data_path_stratification_vars, "pov_ruca_zip.csv")) %>%
  mutate(urbanicity = ifelse(RUCA1 %in% c(4:10), "rural", "urban")) %>%
  # make ed visit variable
  data4casecross_ind <- data4casecross_ind %>% 
  mutate(edvisit_category = ifelse(adm_type %in% c(1, 2), "ED visit", "Non-ED visit"))

  select(-c(GEOID10, STATE, ZIP_TYPE, RUCA1, RUCA2)) %>%
  select(zip, everything()) %>%
  mutate(zip = as.integer(zip)) %>%
  rename(zipcode_R = zip)

# xwalk of zcta zip - use this to keep only valid zips for cohort
valid_zips <-
  read_csv(
    "/n/dominici_nsaph_l3/Lab/projects/wfpm_adrd_hosp/data/data_process/valid_zips_zctas.csv"
  ) %>%
  select(-zcta) %>%
  mutate(link_zipyear = paste0(zip, year))

# cohort
hosp_after_first_adrd_ind <-
  read_rds(paste0(
    data_path,
    "data_process/hosp_dta/hosp_after_first_adrd_ind.rds"
  ))

# hospitalizations --------------------------------------------------------
hosp <- data4casecross_ind_final %>%
  filter(month(DayDateTime) %in% c(4:10)) %>%
  filter(dayName == "Caseday_0") %>%
  mutate(edvisit_category = ifelse(adm_type %in% c(1, 2), "ED visit", "Non-ED visit")) %>% 
  select(
    DayDateTime,
    dayName,
    paste0("wfpm_lag0", 0:6),
    ends_with("out"),
    age,
    sex_group,
    urbanicity,
    pct_pov_final,
    edvisit_category
  )

# ed hosp
table(hosp$edvisit_category)
prop.table(table(hosp$edvisit_category))

nrow(hosp) # 2052611 hospitalizations
nrow(hosp %>% filter(circ_prmy_out == 1)) #1470297 circulatory
nrow(hosp %>% filter(resp_prmy_out == 1)) #897655 resp
nrow(hosp %>% filter(anxty_prmy_out == 1)) #13740 anxiety
nrow(hosp %>% filter(dep_prmy_out == 1)) #58753 depression
hist(month(hosp$DayDateTime))

#### figure hospitalizations ----------------------------------------------------------------
figure_hosp <- hosp %>%
  select(DayDateTime, ends_with("out")) %>%
  group_by(year = year(DayDateTime)) %>%
  mutate(
    tot_circ_prmy_out = sum(circ_prmy_out),
    tot_resp_prmy_out = sum(resp_prmy_out),
    tot_anxty_prmy_out = sum(anxty_prmy_out),
    tot_dep_prmy_out = sum(dep_prmy_out)
  ) %>%
  filter(row_number() == 1) %>%
  select(year, starts_with("tot"))

figure_hosp <- melt(
  figure_hosp,
  id.vars = "year",
  variable.name = "Category",
  value.name = "Value"
)

yearly_total_max <- figure_hosp %>% 
  group_by(year) %>% 
  mutate(yearly_total = sum(Value)) %>% 
  group_by(year) %>% 
  filter(row_number() == 1) %>% 
  ungroup() %>% 
  filter(yearly_total == max(yearly_total))
yearly_total_max$yearly_total

category_colors <- c(
  "tot_circ_prmy_out" = "#440154FF",  # Circulatory
  "tot_resp_prmy_out" = "#3B528BFF",  # Respiratory
  "tot_anxty_prmy_out" = "#FDE725FF",  # Anxiety (color for Depression)
  "tot_dep_prmy_out" = "#35B779FF"   # Depression (color for Anxiety)
)

ggplot(figure_hosp, aes(x = as.character(year), y = Value, fill = Category)) +
  geom_bar(stat = "identity", position = "stack") +
  theme_minimal() +
  xlab("") +
  ylab("Number of hospitalizations") +
  scale_fill_manual(
    name = "Hospitalization type",
    values = category_colors,
    labels = c(
      "tot_circ_prmy_out" = "Circulatory",
      "tot_resp_prmy_out" = "Respiratory",
      "tot_anxty_prmy_out" = "Anxiety",
      "tot_dep_prmy_out" = "Depression"
    )
  ) +
  scale_y_continuous(breaks = seq(0, max(yearly_total_max$yearly_total), by = 25000)) +
  theme_classic() +
  theme(
    text = element_text(size = 18),  # Increase text size
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 14),  # Rotate x-axis labels and increase size
    axis.text.y = element_text(size = 18)  # Increase y-axis text size
  ) 

ggsave("fig_hosp_year.png", 
       path = "/n/dominici_nsaph_l3/Lab/projects/wfpm_adrd_hosp/figures/", 
       width = 11, height = 6, dpi = 300)

# cohort demographics -----------------------------------------------------
cohort <- hosp_after_first_adrd_ind %>%
  # filter(month(adm_date_hosp) %in% c(4:10)) %>%
  group_by(QID) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  mutate(link_zipyear = paste0(zipcode_R, year(adm_date_hosp))) %>%
  left_join(., strat_pov_urbanicity, by = "zipcode_R") %>%
  left_join(., valid_zips, by = "link_zipyear") %>%
  filter(!is.na(zip)) %>%
  mutate(
    age_cat = ifelse(age >= 75, "gtet 75", "lt 75"),
    age_cat = ifelse(is.na(age), NA, age_cat),
    pct_pov_final = ifelse(year(adm_date_hosp) <= 2011, pct_pov_0711, pct_pov_1216),
    med_hh_inc_final = ifelse(year(adm_date_hosp) <= 2011, med_hh_inc_0711, med_hh_inc_1216),
    pov_category = ifelse(pct_pov_final >= 0.2, "pov_gtet20pct", "pov_lt20pct"),
    pct_pov_final = pct_pov_final * 100
  ) 


nrow(cohort) 
table(cohort$sex_group) 
table(cohort$age_cat) 
table(cohort$pov_category, useNA = "always")
table(cohort$urbanicity)
length(unique(cohort$zipcode_R))



# exposure ----------------------------------------------------------------
exp <- data4casecross_ind_final %>%
  filter(month(DayDateTime) %in% c(4:10)) %>%
  filter(dayName == "Caseday_0") %>%
  select(DayDateTime, dayName, zip, paste0("wfpm_lag0", 0:6)) %>% 
  group_by(DayDateTime, zip) %>% 
  filter(row_number() == 1)

summary(exp$wfpm_lag00)

glimpse(exp)

# get the mean daily wildfire pm2.5 and sd

#### figure exposure ----------------------------------------------------------------
# get the average wildfire smoke from months 4-10 across zctas
zcta_shape <- st_read('/n/dominici_nsaph_l3/Lab/projects/wfpm_adrd_hosp/data/shapefile_zip/USA_ZIP_Code_Boundaries/USA_ZIP_Code_Boundaries.shp') %>% 
  filter(!STATE %in% c("AK", "HI", "PR", "AS", "GU", "MP", "VI"))

# get all wildfire data months 4-10, years 2006-2016
wfpm_for_figure <- read_csv('/n/dominici_nsaph_l3/Lab/projects/wfpm_adrd_hosp/data/wildfire_smoke_pm25_per_zipcode/daily_zip_2006.csv')

wfpm_for_figure <- data.frame()

for (year in 2006:2016){
  print(year)
  
  # clean data
  wfpm_year <- read_csv(paste0("/n/dominici_nsaph_l3/Lab/projects/wfpm_adrd_hosp/data/wildfire_smoke_pm25_per_zipcode/daily_zip_", year, ".csv")) %>% 
    filter(month(date) %in% c(4:10),
           as.numeric(zip) %in% (unique(cohort$zipcode_R)))
  
  wfpm_for_figure <- rbind(wfpm_for_figure, wfpm_year)
  
}

# get the median of wfpm for each across years
wfpm_time_barplot <- wfpm_for_figure %>%
  mutate(month = factor(month(date, label = TRUE), levels = month.abb)) %>%
  group_by(month) %>%
  summarise(wfpm_med = median(smoke, na.rm = TRUE),
            wfpm_mean = mean(smoke, na.rm = TRUE)) %>%
  ungroup()

ggplot(wfpm_time_barplot, aes(x = month, y = wfpm_mean)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(x = "", y = expression("Average Wildfire PM"[2.5])) +
  theme(axis.text.x = element_text(hjust = 1)) +
  theme_classic() +
  theme(
    text = element_text(size = 18),  # Increase text size
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 14),  # Rotate x-axis labels and increase size
    axis.text.y = element_text(size = 18)  # Increase y-axis text size
  ) 

ggsave("fig_wfpm_month.png", 
       path = "/n/dominici_nsaph_l3/Lab/projects/wfpm_adrd_hosp/figures/", 
       width = 11, height = 6, dpi = 300)


# Wildfire pm distribution among cases and controls -----------------------
kelvin_to_fahrenheit <- function(kelvin) {
  fahrenheit <- (kelvin - 273.15) * 9/5 + 32
  return(fahrenheit)
}

wfpm_cases <- data4casecross_ind_final %>% 
  filter(dayName == "Caseday_0") %>%
  mutate(max_temp_f_00 = kelvin_to_fahrenheit(max_temp_kelvin_00),
         max_temp_f_01 = kelvin_to_fahrenheit(max_temp_kelvin_01),
         max_temp_f_02 = kelvin_to_fahrenheit(max_temp_kelvin_02),
         max_temp_f_03 = kelvin_to_fahrenheit(max_temp_kelvin_03),
         max_temp_f_04 = kelvin_to_fahrenheit(max_temp_kelvin_04),
         max_temp_f_05 = kelvin_to_fahrenheit(max_temp_kelvin_05),
         max_temp_f_06 = kelvin_to_fahrenheit(max_temp_kelvin_06)) %>% 
  select(id, wfpm_lag00:wfpm_lag06, max_temp_f_00:max_temp_f_06, min_rh_00:min_rh_06)
wfpm_cases <- na.omit(wfpm_cases)
summary_table_cases <- wfpm_cases %>%
  summarise(across(everything(), list(mean = ~mean(.), sd = ~sd(.)), .names = "{col}_{fn}"))
# wfpm lag 00 - 0.3490208 (1.76555)
# temp lag 00 - 64.73958 (20.51659)
# rh lag 00 - 42.40235 (14.88317)


wfpm_controls <- data4casecross_ind_final %>% 
  filter(dayName != "Caseday_0") %>% 
  mutate(max_temp_f_00 = kelvin_to_fahrenheit(max_temp_kelvin_00),
         max_temp_f_01 = kelvin_to_fahrenheit(max_temp_kelvin_01),
         max_temp_f_02 = kelvin_to_fahrenheit(max_temp_kelvin_02),
         max_temp_f_03 = kelvin_to_fahrenheit(max_temp_kelvin_03),
         max_temp_f_04 = kelvin_to_fahrenheit(max_temp_kelvin_04),
         max_temp_f_05 = kelvin_to_fahrenheit(max_temp_kelvin_05),
         max_temp_f_06 = kelvin_to_fahrenheit(max_temp_kelvin_06)) %>% 
  select(id, wfpm_lag00:wfpm_lag06, max_temp_f_00:max_temp_f_06, min_rh_00:min_rh_06)
wfpm_controls <- na.omit(wfpm_controls)
summary_table_controls <- wfpm_controls %>%
  summarise(across(everything(), list(mean = ~mean(.), sd = ~sd(.)), .names = "{col}_{fn}"))
# wfpm lag 00 - 0.3542841 (1.781714)
# temp lag 00 - 64.8951 (20.50507)
# rh lag 00 - 42.38348 (14.8735)


# effective sample information --------------------------------------------------------------------
glimpse(data4casecross_ind_final)

effective_sample <- data4casecross_ind_final %>% 
  select(-c(ends_with(paste0(07:13)))) %>% 
  filter(month(DayDateTime) %in% c(4:10)) %>% 
  mutate(tot_wfpm = rowSums(select(., starts_with("wfpm_lag")), na.rm = T)) %>%
  group_by(id) %>% 
  mutate(tot_wfpm_per_strata = sum(tot_wfpm, na.rm = T)) %>% 
  filter(tot_wfpm_per_strata > 0)

# controls days
effective_sample_case <- effective_sample %>% 
  filter(dayName == "Caseday_0")
length(unique(effective_sample_case$id)) 
length(unique(effective_sample_case$QID)) 
length(unique(effective_sample_case$zip)) 

prop.table(table(effective_sample_case$circ_prmy_out)) 
prop.table(table(effective_sample_case$resp_prmy_out)) 
prop.table(table(effective_sample_case$anxty_prmy_out))
prop.table(table(effective_sample_case$dep_prmy_out)) 

# characteristics of case-hosp; choose the first hospitalization to grab demographics from
effective_sample_char <- effective_sample %>% 
  group_by(QID) %>% 
  filter(row_number() == 1) %>% 
  mutate(link_zipyear = paste0(zip, year(DayDateTime))) %>%
  left_join(., strat_pov_urbanicity %>% rename(zip = zipcode_R), by = "zip") %>%
  mutate(
    age_cat = ifelse(age >= 75, "gtet 75", "lt 75"),
    age_cat = ifelse(is.na(age), NA, age_cat),
    pct_pov_final = ifelse(year(DayDateTime) <= 2011, pct_pov_0711, pct_pov_1216),
    med_hh_inc_final = ifelse(year(DayDateTime) <= 2011, med_hh_inc_0711, med_hh_inc_1216),
    pov_category = ifelse(pct_pov_final >= 0.2, "pov_gtet20pct", "pov_lt20pct"),
    pct_pov_final = pct_pov_final * 100
  ) 

table(effective_sample_char$age_cat, useNA = "always")
prop.table(table(effective_sample_char$age_cat, useNA = "always"))
table(effective_sample_char$sex_group, useNA = "always")
prop.table(table(effective_sample_char$sex_group, useNA = "always"))
table(effective_sample_char$pov_category, useNA = "always")
prop.table(table(effective_sample_char$pov_category, useNA = "always"))
table(effective_sample_char$urbanicity, useNA = "always")
prop.table(table(effective_sample_char$urbanicity, useNA = "always"))

effective_sample_char <- effective_sample_char %>% 
  mutate(edvisit_category = ifelse(adm_type %in% c(1, 2), "ED visit", "Non-ED visit"))
prop.table(table(effective_sample_char$edvisit_category, useNA = "always"))

var_hist_case <- effective_sample_case %>% 
  ungroup() %>% 
  mutate(max_temp_f_00 = kelvin_to_fahrenheit(max_temp_kelvin_00)) %>% 
  select(ends_with(c("_out", "00"))) %>% 
  select(-max_temp_kelvin_00)

var_hist_case_long <- var_hist_case %>%
  ungroup() %>% 
  pivot_longer(cols = c(circ_prmy_out, resp_prmy_out, anxty_prmy_out, dep_prmy_out), 
               names_to = "hosp_type", values_to = "hosp_value") %>%
  filter(hosp_value == 1) %>%  # Keep only rows where hosp_value is 1
  select(-hosp_value) %>%  # Remove the hosp_value column
  # pivot_longer(cols = c(wfpm_lag00, max_temp_f_00, min_rh_00), names_to = "variable", values_to = "value") %>% 
  mutate(hosp_type = case_when(hosp_type == "circ_prmy_out" ~ "Circulatory",
                               hosp_type == "resp_prmy_out" ~ "Respiratory",
                               hosp_type == "anxty_prmy_out" ~ "Anxiety",
                               hosp_type == "dep_prmy_out" ~ "Depression"))

var_hist_case_long_summary <- var_hist_case_long %>% 
  group_by(hosp_type) %>% 
  mutate(wfpm_mean = round(mean(wfpm_lag00, na.rm = T), 2),
         wfpm_sd = round(sd(wfpm_lag00, na.rm = T), 2),
         rh_mean = round(mean(min_rh_00, na.rm = T), 2),
         rh_sd = round(sd(min_rh_00, na.rm = T), 2),
         temp_mean = round(mean(max_temp_f_00, na.rm = T), 2),
         temp_sd = round(sd(max_temp_f_00, na.rm = T), 2)) %>% 
  filter(row_number() == 1)

# controls days
effective_sample_cntrl <- effective_sample %>% 
  filter(dayName != "Caseday_0") 

var_hist_cntrl <- effective_sample_cntrl %>% 
  ungroup() %>% 
  mutate(max_temp_f_00 = kelvin_to_fahrenheit(max_temp_kelvin_00)) %>% 
  select(ends_with(c("_out", "00"))) %>% 
  select(-max_temp_kelvin_00)

var_hist_cntrl_long <- var_hist_cntrl %>%
  ungroup() %>% 
  pivot_longer(cols = c(circ_prmy_out, resp_prmy_out, anxty_prmy_out, dep_prmy_out), 
               names_to = "hosp_type", values_to = "hosp_value") %>%
  filter(hosp_value == 1) %>%  # Keep only rows where hosp_value is 1
  select(-hosp_value) %>%  # Remove the hosp_value column
  # pivot_longer(cols = c(wfpm_lag00, max_temp_f_00, min_rh_00), names_to = "variable", values_to = "value") %>% 
  mutate(hosp_type = case_when(hosp_type == "circ_prmy_out" ~ "Circulatory",
                               hosp_type == "resp_prmy_out" ~ "Respiratory",
                               hosp_type == "anxty_prmy_out" ~ "Anxiety",
                               hosp_type == "dep_prmy_out" ~ "Depression"))

var_hist_cntrl_long_summary <- var_hist_cntrl_long %>% 
  group_by(hosp_type) %>% 
  mutate(wfpm_mean = round(mean(wfpm_lag00, na.rm = T), 2),
         wfpm_sd = round(sd(wfpm_lag00, na.rm = T), 2),
         rh_mean = round(mean(min_rh_00, na.rm = T), 2),
         rh_sd = round(sd(min_rh_00, na.rm = T), 2),
         temp_mean = round(mean(max_temp_f_00, na.rm = T), 2),
         temp_sd = round(sd(max_temp_f_00, na.rm = T), 2)) %>% 
  filter(row_number() == 1)


