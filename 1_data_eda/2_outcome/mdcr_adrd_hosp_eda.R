####***********************
#### Code Description ####
# Author: Vivian 
# Date: 4/13/23
# Goal: medicare adrd hospitalizations eda
####**********************

# Read in libraries
path_libraries <- "/n/dominici_nsaph_l3/Lab/projects/power_outages-adrd_hosp-cond_log/code/0_set_up/"
source(paste0(path_libraries, "1_libraries.R"))

# Read in data ------------------------------------------------------------

# note: this is only for 1 year but use as test (will need to expand to several years EDA)
# for just one year (2020), the data is 911,508, so 10 years ~9,115,080
adrd_hosp <- read_fst(paste0(adrd_dir, "ADRD_2000.fst")) 

nrow(adrd_hosp) 
names(adrd_hosp)

# filter to only NYS zip codes (ie - first 3 digits are within 100-149)
# data question: there are zip codes that are only "113"
# adrd_hosp <- adrd_hosp %>% 
#   filter(as.numeric(substr(as.character(zipcode_R), 1, 3)) %in% 100:149)


# check distribution and missingness of variables
# follow ups:
#     -age - max is 123; is this true? such a healthy person
summary(adrd_hosp)
hist(adrd_hosp$AGE)
nrow(adrd_hosp %>% filter(AGE > 110))

# AD explore --------------------------------------------------------------

# Q: how many cases have AD as the primary billing code?
# A: very few, 3.8% (4193 hospitalizations)
sum(adrd_hosp$AD_primary == 1)/nrow(adrd_hosp)

# Q: how many cases have AD in any primary billing code?
# A: more but still not a lot, 35.7% (39065 hospitalizations)
sum(adrd_hosp$AD_any == 1)/nrow(adrd_hosp)

# Q: What are the socio-demographic characteristics/geographic dist?
# A: regardless of race, women were more freq hos
#    mean of hosp count typically 85 age across all groups
#.   white women had highest ct
eda_ad <- adrd_hosp %>% 
  filter(AD_any == 1) %>% 
  select(AGE, Sex_gp, Race_gp, SSA_STATE_CD)

hist(eda_ad$AGE)

ggplot(eda_ad, aes(x = AGE)) +
  facet_grid(~Sex_gp) +
  geom_histogram() 

ggplot(eda_ad, aes(x = AGE)) +
  facet_grid(~Race_gp) +
  geom_histogram() 

ggplot(eda_ad, aes(x = AGE)) +
  facet_grid(~Race_gp + Sex_gp) +
  geom_histogram() 

# look into ad/adrd trends
eda_adrd <- adrd_hosp %>% 
  filter(ADRD_any == 1) %>% 
  select(AGE, Sex_gp, Race_gp, SSA_STATE_CD)

hist(eda_adrd$AGE)

ggplot(eda_adrd, aes(x = AGE)) +
  facet_grid(~Sex_gp) +
  geom_histogram() 

ggplot(eda_adrd, aes(x = AGE)) +
  facet_grid(~Race_gp) +
  geom_histogram() 

ggplot(eda_adrd, aes(x = AGE)) +
  facet_grid(~Race_gp + Sex_gp) +
  geom_histogram() 


# Q: Are adate and ddate usually/always the same?
# follow ups:
#       - wait a minute...some hospital durations are >200 days - is this believable?
#       - what does adm_type 9 mean?
hosp_date <- adrd_hosp %>% 
  mutate(dur_hosp_stay = as.numeric(DDATE - ADATE))
hist(hosp_date$dur_hosp_stay)
summary(hosp_date$dur_hosp_stay)
nrow(hosp_date %>% filter(dur_hosp_stay > 30))


hosp_date %>%
  ungroup() %>% 
  arrange(desc(dur_hosp_stay)) %>% 
  slice(1:10)

# ADRD explore --------------------------------------------------------------

# Q: how many cases have ADRD as the primary billing code?
# A: very few, 9.6% (10487 hospitalizations)
sum(adrd_hosp$ADRD_primary == 1)/nrow(adrd_hosp)

# Q: how many cases have ADRD in any primary billing code?
# A: everyone (by design) 100% (109141 hospitalizations)
sum(adrd_hosp$ADRD_any == 1)/nrow(adrd_hosp)

# Q: What are the socio-demographic characteristics/geographic dist?
eda_rd <- adrd_hosp %>% 
  filter(ADRD_any == 1) %>% 
  select(AGE, Sex_gp, Race_gp, SSA_STATE_CD)

hist(eda_rd$AGE)
table(eda_rd$Sex_gp, useNA = "ifany")
table(eda_rd$Race_gp, useNA = "ifany")
table(eda_rd$SSA_STATE_CD, useNA = "ifany")


# When and where do they occur --------------------------------------------
head(adrd_hosp)
adrd_hosp <- adrd_hosp %>% 
  mutate(adate_dow = as.numeric(wday(ADATE, label = TRUE))) #1 = Monday, 7 = Sunday

# weekday trends for both ad and adrd 
ggplot(adrd_hosp, aes(x = (adate_dow))) +
  geom_histogram() +
  scale_x_continuous(breaks = 1:7) +
  labs(x = "", y = "Count", title = "Histogram of Weekdays with ad + adrd hosp")

# weekday trends for ad
ggplot(adrd_hosp %>% filter(AD_any == 1), aes(x = (adate_dow))) +
  geom_histogram() +
  scale_x_continuous(breaks = 1:7) +
  labs(x = "", y = "Count", title = "Histogram of Weekdays with ad hosp")

# weekday trends for adrd
ggplot(adrd_hosp %>% filter(ADRD_any == 1), aes(x = (adate_dow))) +
  geom_histogram() +
  scale_x_continuous(breaks = 1:7) +
  labs(x = "", y = "Count", title = "Histogram of Weekdays with adrd hosp")

# weekday trends for both ad and adrd 
ggplot(adrd_hosp, aes(x = month(ADATE))) +
  geom_histogram() +
  scale_x_continuous(breaks = 1:12, labels = month.name[1:12]) +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
  labs(x = "", y = "Count", title = "Histogram of Months with ad/adrd")

# weekday trends for ad
ggplot(adrd_hosp %>% filter(AD_any == 1), aes(x = month(ADATE))) +
  geom_histogram() +
  scale_x_continuous(breaks = 1:12, labels = month.name[1:12]) +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
  labs(x = "", y = "Count", title = "Histogram of Months with ad")

# weekday trends for adrd
ggplot(adrd_hosp %>% filter(ADRD_any == 1), aes(x = month(ADATE))) +
  geom_histogram() +
  scale_x_continuous(breaks = 1:12, labels = month.name[1:12]) +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
  labs(x = "", y = "Count", title = "Histogram of Months with adrd")

# zip codes with greatest hosp 
tmp <- adrd_hosp %>% 
  group_by(zipcode_R) %>% 
  mutate(ct_ad_adrd_hosp = n(),
         ct_ad_hosp = sum(ifelse(AD_any == 1, 1, 0)),
         ct_adrd_hosp = sum(ifelse(ADRD_any == 1, 1, 0))) %>% 
  filter(row_number() == 1)
  





