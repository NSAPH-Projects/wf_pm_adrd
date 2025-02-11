# Create Tables and Plots to Show Model Results
# Goal: output tables and figures for wf pm - adrd hospitalization
# Vivian Do
# 1/27/25
#* Create map of medicare population
#* Create map of wildfire pm exposure

####********************
#### Preparation #### 
####********************
rm(list=ls())
source(here::here("0_set_up", "1_libraries.R"))

# Set up filepath(s)
data_path <- '/n/dominici_nsaph_l3/Lab/projects/wfpm_adrd_hosp/data/'
shapefile_path <- '/n/dominici_nsaph_l3/Lab/projects/wfpm_adrd_hosp/data/shapefile_zip/USA_ZIP_Code_Boundaries/'
output_figures_path <- paste0(data_path, "output/figures/")

# shapefile
zip_shapefile <- st_read(paste0(shapefile_path, "USA_ZIP_Code_Boundaries.shp")) %>% 
  filter(!STATE %in% c("AK", "HI", "AS", "GU", "MP", "PR", "VI"))
st_crs(zip_shapefile)

# xwalk of zcta zip - use this to keep only valid zips
valid_zips <-
  read_csv(
    "/n/dominici_nsaph_l3/Lab/projects/wfpm_adrd_hosp/data/data_process/valid_zips_zctas.csv"
  ) %>%
  select(-zcta) %>%
  mutate(link_zipyear = paste0(zip, year))

xwalk_zcta_zip <-
  read_csv(
    "/n/dominici_nsaph_l3/Lab/projects/wfpm_adrd_hosp/data/data_process/valid_zips_zctas.csv"
  )  

# cohort
hosp_after_first_adrd_ind <-
  read_rds(paste0(
    data_path,
    "data_process/hosp_dta/hosp_after_first_adrd_ind.rds"
  ))

cohort <- hosp_after_first_adrd_ind %>%
  # filter(month(adm_date_hosp) %in% c(4:10)) %>%
  group_by(QID) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  mutate(link_zipyear = paste0(zipcode_R, year(adm_date_hosp))) %>%
  left_join(., valid_zips, by = "link_zipyear") %>%
  filter(!is.na(zip)) %>% 
  mutate(ZIP_CODE = as.character(zipcode_R))

# hospitalizations
data4casecross_zip <- read_fst('/n/dominici_nsaph_l3/Lab/projects/wfpm_adrd_hosp/data/data_process/data4casecross/data_w_all_vars/data4casecross_zip.fst')

# total medicare benes by zip
data_path <- "/n/dominici_nsaph_l3/Lab/lego/medicare/mbsf_medpar_denom/"
years <- 2006:2016

read_and_process <- function(year) {
  file_path <- paste0(data_path, "denom_", year, ".parquet")
  tryCatch({
    denom <- read_parquet(file_path)
    denom %>%
      group_by(zcta, year) %>%
      summarize(tot_zcta = n(), .groups = 'drop')
  }, error = function(e) {
    message(paste("Year", year, "not found"))
    return(NULL)
  })
}

# Read and combine all Parquet files into a single dataset
combined_data <- map_dfr(years, read_and_process)

# wfpm data
years <- 2006:2016
data_path <- '/n/dominici_nsaph_l3/Lab/projects/wfpm_adrd_hosp/data/'
read_and_process <- function(year) {
  read_csv(paste0(data_path, "wildfire_smoke_pm25_per_zipcode/daily_zip_", year, ".csv")) %>%
    mutate(zip = as.numeric(zip)) %>%
    filter(month(date) %in% c(4:10))
}

# Read and combine all CSV files into a single dataset
wfpm_preds <- map_dfr(years, read_and_process)

# tidy cohort -------------------------------------------------------------
# get total counts
zip_cohort <- cohort %>%
  group_by(ZIP_CODE) %>%
  mutate(tot_bene_per_zip = n()) %>%
  filter(row_number() == 1) %>%
  select(ZIP_CODE, tot_bene_per_zip) %>% 
  mutate(zip_numeric = as.numeric(ZIP_CODE))

# get total hospitalization counts
zip_hosp <- data4casecross_zip %>% 
  filter(dayName == "Caseday_0") %>% 
  select(zip, dayName, contains("count_")) %>% 
  group_by(zip) %>% 
  summarize(across(starts_with("count_"), sum, na.rm = TRUE))

# get average zcta population from 2006-2016
avg_zip_pop <- combined_data %>% 
  left_join(., xwalk_zcta_zip) %>% 
  # filter(tot_zcta > 100) %>% 
  group_by(zip) %>% 
  summarize(avg_zip_pop = mean(tot_zcta, na.rm = T)) 

# join with outcome data
zip_hosp_rate_per_1000 <- zip_hosp %>%
  left_join(., avg_zip_pop %>%
              mutate(zip = as.numeric(zip))) %>% 
  mutate(rate_circ = (count_circ_prmy_out/avg_zip_pop * 1000),
         rate_resp = (count_resp_prmy_out/avg_zip_pop * 1000),
         rate_anxty = (count_anxty_prmy_out/avg_zip_pop * 1000),
         rate_dep = (count_dep_prmy_out/avg_zip_pop * 1000)) %>% 
  select(zip, starts_with("rate"))


# map cohort ---------------------------------------------------------------------
test <-
  left_join(zip_shapefile %>% mutate(zip = as.numeric(ZIP_CODE)),
            zip_hosp_rate_per_1000 %>% mutate(in_study = 1),
            by = ("zip"))

# Reshape the data for faceting
test_long <- test %>%
  pivot_longer(cols = starts_with("rate_"), names_to = "rate_type", values_to = "rate") %>% 
  filter(in_study == 1)

map_zips_all <- zip_shapefile

albers_proj <- 5070
map_zips_all <- st_transform(map_zips_all, crs = albers_proj)
test_long <- st_transform(test_long, crs = albers_proj)

# Replace rate_type values with desired labels
test_long <- test_long %>%
  mutate(rate_type = recode(rate_type,
                            rate_anxty = "Anxiety",
                            rate_dep = "Depression",
                            rate_circ = "Circulatory",
                            rate_resp = "Respiratory"))

# Create plots
map_plot_circ <- ggplot() +
  geom_sf(data = map_zips_all, fill = "grey80", color = "grey70", size = 0.1) +
  geom_sf(data = test_long %>% filter(rate_type == "Circulatory"), aes(fill = log(rate)), size = 0.2, color = NA) +
  scale_fill_viridis_c(option = "plasma", name = "Rate per 1000") +
  ggtitle("Circulatory") +
  theme(
    plot.title = element_text(hjust = 0.5),
    text = element_text(size = 12),  # Increase text size
    legend.text = element_text(size = 6),  # Set legend text size
    legend.title = element_text(size = 6), 
    panel.background = element_rect(fill = "white", color = NA),  # Set panel background to white
    plot.background = element_rect(fill = "white", color = NA)    # Set plot background to white
  ) +
  theme_void() 

map_plot_resp <- ggplot() +
  geom_sf(data = map_zips_all, fill = "grey80", color = "grey70", size = 0.1) +
  geom_sf(data = test_long %>% filter(rate_type == "Respiratory"), aes(fill = log(rate)), size = 0.2, color = NA) +
  scale_fill_viridis_c(option = "plasma", name = "Rate per 1000") +
  ggtitle("Respiratory") +
  theme(
    plot.title = element_text(hjust = 0.5),
    text = element_text(size = 12),  # Increase text size
    legend.text = element_text(size = 6),  # Set legend text size
    legend.title = element_text(size = 6), 
    panel.background = element_rect(fill = "white", color = NA),  # Set panel background to white
    plot.background = element_rect(fill = "white", color = NA)    # Set plot background to white
  ) +
  theme_void() 

map_plot_anxty <- ggplot() +
  geom_sf(data = map_zips_all, fill = "grey80", color = "grey70", size = 0.1) +
  geom_sf(data = test_long %>% filter(rate_type == "Anxiety"), aes(fill = log(rate)), size = 0.2, color = NA) +
  scale_fill_viridis_c(option = "plasma", name = "Rate per 1000") +
  ggtitle("Anxiety") +
  theme(
    plot.title = element_text(hjust = 0.5),
    text = element_text(size = 12),  # Increase text size
    legend.text = element_text(size = 6),  # Set legend text size
    legend.title = element_text(size = 6), 
    panel.background = element_rect(fill = "white", color = NA),  # Set panel background to white
    plot.background = element_rect(fill = "white", color = NA)    # Set plot background to white
  ) +
  theme_void() 

map_plot_dep <- ggplot() +
  geom_sf(data = map_zips_all, fill = "grey80", color = "grey70", size = 0.1) +
  geom_sf(data = test_long %>% filter(rate_type == "Depression"), aes(fill = log(rate)), size = 0.2, color = NA) +
  scale_fill_viridis_c(option = "plasma", name = "Rate per 1000") +
  ggtitle("Depression") +
  theme(
    plot.title = element_text(hjust = 0.5),
    text = element_text(size = 12),  # Increase text size
    legend.text = element_text(size = 6),  # Set legend text size
    legend.title = element_text(size = 6), 
    panel.background = element_rect(fill = "white", color = NA),  # Set panel background to white
    plot.background = element_rect(fill = "white", color = NA)    # Set plot background to white
  ) +
  theme_void() 

plot_main <- (map_plot_circ + map_plot_resp) / (map_plot_anxty + map_plot_dep)
# plot_main <- plot_main + plot_annotation(tag_levels = "A")
# plot_main

ggsave("supp_hosp_out.png", 
       plot = plot_main, 
       path = "/n/dominici_nsaph_l3/Lab/projects/wfpm_adrd_hosp/figures/", 
       width = 11, height = 6, dpi = 300)

# tidy wfpm ---------------------------------------------------------------
summary(wfpm_preds$zip)

zip_wfpm_preds <- wfpm_preds %>% 
  group_by(zip) %>% 
  mutate(avg_wfpm = mean(smoke, na.rm = T),
         med_wfpm = median(smoke, na.rm = T)) %>% 
  filter(row_number() == 1)

map_zip_wfpm_preds <- zip_shapefile %>% 
  mutate(zip = as.numeric(ZIP_CODE)) %>% 
  full_join(., zip_wfpm_preds) %>% 
  st_transform(., crs = 5070)

map_zip_wfpm_preds_study <- zip_shapefile %>% 
  mutate(zip = as.numeric(ZIP_CODE)) %>% 
  full_join(., zip_wfpm_preds) %>% 
  st_transform(., crs = 5070) %>% 
  filter(zip %in% data4casecross_zip$zip)

map_plot <- ggplot() +
  # geom_sf(data = map_zips_all, fill = "grey80", color = "grey70", size = 0.1) +
  geom_sf(data = map_zip_wfpm_preds_study, aes(fill = avg_wfpm), size = 0.1) +
  scale_fill_viridis_c(option = "plasma", name = expression("Average Wildfire PM"[2.5] ~ "(μg/m"^3*")")) +
  theme(
    text = element_text(size = 11),  # Increase text size
    panel.background = element_rect(fill = "white", color = NA),  # Set panel background to white
    plot.background = element_rect(fill = "white", color = NA)    # Set plot background to white
  )

# map wfpm ---------------------------------------------------------------------
glimpse(zip_wfpm_preds)
map_zips_all <- left_join(
  zip_shapefile %>%
    mutate(zip_numeric = as.numeric(ZIP_CODE)),
  zip_wfpm_preds,
  by = c("zip_numeric" = "zip")
)

# keep only wfpm observations in if they appear in study
map_zip_wfpm_preds_study <- zip_shapefile %>% 
  mutate(zip = as.numeric(ZIP_CODE)) %>% 
  full_join(., zip_wfpm_preds) %>% 
  st_transform(., crs = 5070) %>% 
  filter(zip %in% data4casecross_zip$zip)

albers_proj <- 5070
map_zips_all <- st_transform(map_zips_all, crs = albers_proj)
map_zips_study <- st_transform(map_zips_study, crs = albers_proj)

map_plot <- ggplot() +
  geom_sf(data = map_zips_all, fill = "grey80", color = "grey70", size = 0.1) +
  geom_sf(data = map_zip_wfpm_preds_study, aes(fill = avg_wfpm), size = 0.2, color = NA) +
  scale_fill_viridis_c(option = "plasma", name = expression("Average Wildfire PM"[2.5] ~ "(μg/m"^3*")")) +
  theme(
    text = element_text(size = 11),  # Increase text size
    panel.background = element_rect(fill = "white", color = NA),  # Set panel background to white
    plot.background = element_rect(fill = "white", color = NA)    # Set plot background to white
  )

plot(map_plot)

ggsave(map_plot,
       "supp_wfpm.png", 
       path = "/n/dominici_nsaph_l3/Lab/projects/wfpm_adrd_hosp/figures/", 
       width = 11, height = 6, dpi = 300)

