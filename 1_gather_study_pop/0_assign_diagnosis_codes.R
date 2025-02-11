####***********************
#### Code Description ####
# Author: Vivian 
# Date: 7/20/23
# Goal: Identify ICD9/ICD10 codes for
# ADRD, cardiorespiratory, respiratory, anxiety, and depression outcomes
####**********************

# Load libraries ----------------------------------------------------------
source(here::here("0_set_up", "1_libraries.R"))

# Define ICD 9/10 codes ---------------------------------------------------
# adrd
icd_adrd <- c(3310, 33111, 33119, 3312, 3317, 2900, 29010, 29011, #icd 9
              29012, 29013, 29020, 29021, 2903, 29040, 29041, 29042, 
              29043, 2940, 29410, 29411, 29420, 29421, 2948, 797,
              
              
              "F0150", "F0151", "F0280", "F0281", "F0390", "F0391", "F04", "G138", #icd 10
              "F05", "F061", "F068", "G300", "G301", "G308", "G309", "G311", "G312",
              "G3101", "G3109", "G94", "R4181", "R54")

# circulatory 
icd_circ <- c((390:459), #icd 9
              paste0("I", "0", 0:9), #icd 10
              paste0("I", 10:99)) #icd 10

# respiratory
icd_resp <- c((460:519), #icd 9
              paste0("J", "0", 0:9), #icd 10
              paste0("J", 10:99)) #icd 10

# as of 7/3/23, we have included H.E.'s suggestions for ICD codes related to mental health
# anxiety; if 2+ visits on different days have these codes
icd_anxty <- c(30001, 30002, 30009, 3000,  # icd 9
               "F419", "F410", "F411", "F418") #icd 10


# depression
icd_dep <- c(29620, 29621, 29622, 29623, #icd 9
             29625, 29626, 29630, 29631,
             29632, 29633, 29634, 29635,
             29636, 2980, 3004, 309, 3091, 311, 
             2962, 2963, # these are general families for depression cases 
             
             "F323", "F333", "F320", "F321", "F322", #icd 10
             "F324", "F325", "F329", "F330", "F331",
             "F332", "F3340", "F3341", "F3342",
             "F339", "F3289", "F338", "F4321", "F32A")

