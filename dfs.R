#A script that will be sourced from the APP in order to call the cleaned individual
#DF, dyad DF, and pairwise DF

options(warn=-1)

#Packages
library(haven)
library(dplyr)
library(tidyr)
library(mosaic)
library(psych)
library(nlme)

# --- loading in the data

#Adolescent demographic data
teen_demog <- read_sav("Teen-Demographic-SPSS.sav")

#Adolescent diet data
teen_diet <- read_sav("teen_diet_public_updated.sav")

#Adolescent physical activity data
teen_pa <- read_sav("teenpa_public_updated.sav")

#Parent demographic data
parent_demog <- read_sav("Parent-Demographic-SPSS.sav")

#Parents diet data
parent_diet <- read_sav("parent_diet_public_updated.sav")

#Parent physical activity data
parent_pa <- read_sav("parent_pa_public_updated.sav")


#### --- CLEANING PARENT DATA


# --- Cleaning parent diet

#getting rid of the p at the beginning
names(parent_diet)[8:110] <- substring(names(parent_diet)[8:110],2)
#getting rid of x and p for the computed variables at the end
names(parent_diet)[116:147] <- substring(names(parent_diet)[116:147],3)

# --- Cleaning parent physical activity

#getting rid of the p at the beginning
names(parent_pa)[8:119] <- substring(names(parent_pa)[8:119],2)
#getting rid of x and p for computed variables at the end
names(parent_pa)[125:130] <- substring(names(parent_pa)[125:130],3)
#Individual cleaning
parent_pa <- parent_pa %>%
  rename(PVIGMINS_X = XPPVIGMINS, PMODMINS_X = XPPMODMINS, PWLKMINS_X = XPPWLKMINS, PSITMINS_X = XPPSITMINS)

# --- Cleaning parent demographic

#Getting rid of the p at the beginning
names(parent_demog)[3:23] <- substring(names(parent_demog)[3:23], 2)
#getting rid of x and p
names(parent_demog)[25:32] <- substring(names(parent_demog)[25:32], 3)
#individual cleaning
parent_demog <- parent_demog %>%
  rename(ETHRAC_RC = PETHRAC_RC, CANCER_RC = XPCANCER_RC, AGE_RC = PAGE_RC, KIDSINHOME_RC =PKIDSINHOME_RC, 
         TIMEADDRMM_RC = XPTIMEADDRMM_RC, WORKHRS_RC = PWORKHRS_RC, HSEHLDINCM_RC = PHSEHLDINCM_RC, 
         NATIVAGE_RC = PNATIVAGE_RC, LITERACY_RC = PLITERACY_RC, LANGHOME_RC = XPLANGHOME_RC, MARITAL_RC = PMARITAL_RC, 
         HOMEOWN_RC = PHOMEOWN_RC, HEIGHTCM_RC = XPHEIGHTCM_RC, WEIGHTKG_RC = XPWEIGHTKG_RC, 
         THEIGHTCM_RC = XPTHEIGHTCM_RC, TWEIGHTKG_RC = XPTWEIGHTKG_RC)


#### --- CLEANING TEEN DATA


# --- Cleaning teen diet

#getting rid of the t at the beginning
names(teen_diet)[8:110] <- substring(names(teen_diet)[8:110],2)
#getting rid of x and t for the computed variables at the end
names(teen_diet)[116:147] <- substring(names(teen_diet)[116:147],3)

# --- Cleaning teen physical activity

#getting rid of the t at the beginning
names(teen_pa)[8:156] <- substring(names(teen_pa)[8:156],2)
#getting rid of x and t for the computed variables at the end
names(teen_pa)[158:165] <- substring(names(teen_pa)[158:165],3)
names(teen_pa)[170:185] <- substring(names(teen_pa)[170:185],3)

# --- Cleaning teen demographic

#getting rid of the t at the beginning
names(teen_demog)[3:18] <- substring(names(teen_demog)[3:18],2)

#getting rid of x and t for the computed variables at the end
names(teen_demog)[20:23] <- substring(names(teen_demog)[20:23],3)
names(teen_demog)[26:28] <- substring(names(teen_demog)[26:28],3)

teen_demog <- teen_demog %>%
  rename(ETHRAC_RC = TETHRAC_RC, WORKHRS_RC = TWORKHRS_RC)


#### --- MERGING DATA


# --- merging parent datasets

parent_full <- parent_demog %>%
  full_join(parent_diet, by = "PID") %>%
  full_join(parent_pa, by = "PID")

parent_full <- parent_full %>%
  select(- c(DYADID, DYADID.y)) %>%
  rename(DYADID = DYADID.x, FASTCMPFL_DEMOG = FASTCMPFL.x, DATE_STARTED_DIET = DATE_STARTED.x, DATE_SUBMITTED_DIET = DATE_SUBMITTED.x, TASKID_DIET = TASKID.x, SEQ_DIET = SEQ.x, SURVEY_TYPE_DIET = SURVEY_TYPE.x, FASTCMPFL_DIET = FASTCMPFL.y, WT_P_BOTH_DIET = WT_P_BOTH.x, QUOTA_P_DIET = QUOTA_P.x, QUOTA_T_DIET = QUOTA_T.x, DATE_STARTED_PA = DATE_STARTED.y, DATE_SUBMITTED_PA = DATE_SUBMITTED.y, TASKID_PA = TASKID.y, SEQ_PA = SEQ.y, SURVEY_TYPE_PA = SURVEY_TYPE.y, WT_P_BOTH_PA = WT_P_BOTH.y, QUOTA_P_PA = QUOTA_P.y, QUOTA_T_PA = QUOTA_T.y) %>%
  mutate(distinguish = "P")

# --- Merging teen datasets

teen_full <- teen_demog %>%
  full_join(teen_diet, by = "PID") %>%
  full_join(teen_pa, by = "PID")

teen_full <- teen_full %>%
  select(- c(DYADID, DYADID.y)) %>%
  rename(DYADID = DYADID.x, FASTCMPFL_DEMOG = FASTCMPFL.x, DATE_STARTED_DIET = DATE_STARTED.x, DATE_SUBMITTED_DIET = DATE_SUBMITTED.x, TASKID_DIET = TASKID.x, SEQ_DIET = SEQ.x, SURVEY_TYPE_DIET = SURVEY_TYPE.x, FASTCMPFL_DIET = FASTCMPFL.y, WT_T_BOTH_DIET = WT_T_BOTH.x, QUOTA_P_DIET = QUOTA_P.x, QUOTA_T_DIET = QUOTA_T.x, DATE_STARTED_PA = DATE_STARTED.y, DATE_SUBMITTED_PA = DATE_SUBMITTED.y, TASKID_PA = TASKID.y, SEQ_PA = SEQ.y, SURVEY_TYPE_PA = SURVEY_TYPE.y, WT_T_BOTH_PA = WT_T_BOTH.y, QUOTA_P_PA = QUOTA_P.y, QUOTA_T_PA = QUOTA_T.y) %>%
  mutate(distinguish = "T")


#### --- ROW BIND TO CREATE INDIVIDUAL DF


#creating the big dataset
full_data <- bind_rows(teen_full, parent_full)

df <- subset(full_data, select=c(PID, DYADID, distinguish, FOLFAMRUL:PREDWEEKSED, TCURRCOV:WT_P_BOTH_PA))

df_indiv <- df %>%
  mutate(distinguish = ifelse(distinguish == "P", 1, 2)) %>%
  arrange(DYADID) %>%
  select(-PID)

#necessary to get rid of individual identifier for next step!


#### --- FROM INDIVIDUAL -> DYAD


df_dyad <- df_indiv %>%
  mutate(distinguish = ifelse(distinguish == 1, "P", "T")) %>%
  gather(variable, value, FOLFAMRUL:WT_P_BOTH_PA) %>%
  unite(var_distinguish, variable, distinguish) %>%
  spread(var_distinguish, value)


#### --- FROM INDIVIDUAL -> PAIRWISE


tempA <- df_indiv %>%
  mutate(distinguishE = distinguish, partnum = 1, distinguish = ifelse(distinguish == 1, "A", "P")) %>%
  gather(variable, value, FOLFAMRUL:distinguishE) %>%
  unite(var_distinguish, variable, distinguish) %>%
  spread(var_distinguish, value)

tempB <- df_indiv %>%
  mutate(distinguishE = distinguish, partnum = 2, distinguish = ifelse(distinguish == 1, "P", "A")) %>%
  gather(variable, value, FOLFAMRUL:distinguishE) %>%
  unite(var_distinguish, variable, distinguish) %>%
  spread(var_distinguish, value)

#This is the pairwise df
df_pair <- bind_rows(tempA, tempB) %>%
  arrange(DYADID)
