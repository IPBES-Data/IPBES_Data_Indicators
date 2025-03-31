# clean you environment
rm(list=ls())

########################################
# Extract indicators from multiple MEAs
########################################
# Created by Yanina Sica in January 2023
# Updated Feb 2025

### Settings----

## Your working directory (will be set using function in setting.R)
your_dir <- dirname(rstudioapi::getSourceEditorContext()$path) # works only in RStudio
#your_dir <- "path_to_where_code_is" # complete accordingly

## Source useful functions from folder downloaded from GitHub
source(paste0(your_dir,"/useful_functions_indic.R"))
source(paste0(your_dir,"/settings.R"))

## Set working directory and install required libraries
settings()

## Read installed libraries
library(stringr)
library(gtools)
library(dplyr)
library(tidyr)
library(readr)
library(data.table)
library(purrr)
library(googlesheets4)
library(ggplot2)
library(circlize)
library(ggalluvial)

# Load indicators from conventions (policy)----
## Indicators from KM_GBF----

complementary =  read_csv("../input/meas/GBF/complementary_indicators_2025-01-15.csv") %>% 
  dplyr::select("Goal/target","Indicator name") %>% 
  mutate(indicator_type = 'complementary') %>% 
  # create ID
  mutate(indic_id = paste0('GBF_',`Goal/target`,'_CY_',row_number())) %>% 
  mutate(indic_id = gsub('Goal ', 'G', indic_id)) %>% 
  mutate(indic_id = gsub('Target ', 'T', indic_id)) 

component =  read_csv("../input/meas/GBF/component_indicators_2025-01-15.csv") %>% 
  dplyr::select("Goal/target","Indicator name") %>% 
  mutate(indicator_type = 'component') %>% 
  # create ID
  mutate(indic_id = paste0('GBF_',`Goal/target`,'_C_',row_number())) %>% 
  mutate(indic_id = gsub('Goal ', 'G', indic_id)) %>% 
  mutate(indic_id = gsub('Target ', 'T', indic_id)) 

headline =  read_csv("../input/meas/GBF/headline_indicators_2025-01-15.csv") %>% 
  dplyr::select("Goal/target","Indicator name", "Group") %>% 
  rename(indicator_type = Group) %>% 
  # remove the goals in indicator name
  dplyr::mutate(`Indicator name` = word(`Indicator name`, start = 2,end = -1, sep = ' ')) %>% 
  # create ID
  mutate(indic_id = paste0('GBF_',`Goal/target`,'_H_',row_number())) %>% 
  mutate(indic_id = gsub('Goal ', 'G', indic_id)) %>% 
  mutate(indic_id = gsub('Target ', 'T', indic_id)) 

# Join all indicators
gbf = headline %>% 
  # Join all types of indicators proposed
  rbind(component,complementary) %>% 
  mutate(indicator_name = gsub('None adopted','',`Indicator name`))
write_csv(gbf, '../input/meas/GBF/gbf_indicators.csv')

# Harmonize indicators
gbf = gbf %>% 
  # Filter out indicators that do not exist
  filter(!is.na(indicator_name)) %>% 
  filter(indicator_name != '') %>% 
  # harmonize indicators
  dplyr::mutate(indicators_h = tolower(indicator_name)) %>%
  dplyr::mutate(indicators_h = gsub('  ', '',indicators_h)) %>% 
  dplyr::mutate(indicators_h = str_trim(indicators_h)) %>% 
  dplyr::mutate(indicators_h = gsub("[.]$","",indicators_h)) %>% 
  harmonize_indic(indicators_h) %>% 
  dplyr::rename(indicator_harmonized = field_harm) %>% 
  dplyr::mutate(indicator_harmonized = str_trim(indicator_harmonized)) %>% 
  # keep original name
  dplyr::rename(indicator_orig = indicator_name) %>% 
  # clean columns
  dplyr::select("goal_target"="Goal/target","indicator_orig","indic_id","indicator_harmonized","indicator_type") %>% 
  # save input data
  write_csv('../input/meas/GBF/gbf_indic_harm.csv')

gbf %>% distinct(indicator_harmonized) %>% count() # 315
gbf %>% distinct(indicator_orig) %>% count() # 331

# Prepare indicators for analisys
indic_gbf = gbf %>% 
  dplyr::group_by(indicator_harmonized) %>% 
  dplyr::summarise(indic_ids = paste0(indic_id, collapse = ";")) %>% 
  # add source
  dplyr::mutate(gbf = 1)

rm(headline, component, complementary)

## Indicators from SDGs----

sdg = readxl::read_excel("../input/meas/SDG/sdg.xlsx",
                         sheet = "indicators")

sdg = sdg %>% 
  # extract indicators
  dplyr::mutate(indic_name = word(indicators, start = 2,end = -1, sep = ' ')) %>% 
  # get indic_id
  dplyr::mutate(indic_id = word(indicators, start = 1,end = 1, sep = ' ')) %>% 
  dplyr::mutate(indic_id = paste0("SDG_G",indic_id, "_",row_number())) %>% 
  # harmonize indicators
  dplyr::mutate(indicators_h = gsub('  ', '',indic_name)) %>% 
  dplyr::mutate(indicators_h = tolower(indicators_h)) %>%
  dplyr::mutate(indicators_h = str_trim(indicators_h)) %>% 
  dplyr::mutate(indicators_h = gsub("[.]$","",indicators_h)) %>% 
  harmonize_indic(indicators_h) %>% 
  dplyr::rename(indicator_harmonized = field_harm) %>% 
  dplyr::mutate(indicator_harmonized = str_trim(indicator_harmonized)) %>% 
  dplyr::select(-indicators_h) %>% 
  # Remove 11c which is in development
  dplyr::filter(!is.na(indicator_harmonized)) %>% 
  # keep original name
  dplyr::rename(indicator_orig = indic_name) %>% 
  # clean columns
  dplyr::select("goal"="goals", "target"="targets","indicator_orig","indic_id","indicator_harmonized") %>% 
  # save input data
  write_csv('../input/meas/SDG/sdg_indic_harm.csv')
  

# Prepare indicators for analisys
indic_sdg = sdg %>% 
  dplyr::group_by(indicator_harmonized) %>% 
  dplyr::summarise(indic_ids = paste0(indic_id, collapse = ";")) %>% 
  # add source
  dplyr::mutate(sdg = 1)

rm(sdg)

## Indicators from CITES----

cites = readxl::read_excel("../input/meas/CITES/cites.xlsx",
                           sheet = "cites") %>% 
  filter(!is.na(id))

cites = cites %>% 
  # extract indicators
  dplyr::mutate(indic = strsplit(as.character(indicators), "\n")) %>% 
  unnest(indic) %>% 
  dplyr::mutate(indic = gsub('Indicator ','',indic)) %>%
  dplyr::mutate(indic = gsub('[:]','',indic)) %>%
  dplyr::mutate(indic_name = word(indic, start = 2,end = -1, sep = ' ')) %>% 
  # remove objectives without indicators
  dplyr::filter(!is.na(indicators)) %>% 
  # get indic_id
  dplyr::mutate(indic_id = word(indic, start = 1,end = 1, sep = ' ')) %>% 
  dplyr::mutate(indic_id = paste0("CITES_G",indic_id, "_",row_number())) %>% 
  dplyr::select(-indicators, -indic) %>% 
  # harmonize indicators
  dplyr::mutate(indicators_h = gsub('  ', '',indic_name)) %>% 
  dplyr::mutate(indicators_h = tolower(indicators_h)) %>%
  dplyr::mutate(indicators_h = str_trim(indicators_h)) %>% 
  dplyr::mutate(indicators_h = gsub("[.]$","",indicators_h)) %>% 
  harmonize_indic(indicators_h) %>% 
  dplyr::rename(indicator_harmonized = field_harm) %>% 
  dplyr::mutate(indicator_harmonized = str_trim(indicator_harmonized)) %>% 
  dplyr::select(-indicators_h) %>% 
  # keep original name
  dplyr::rename(indicator_orig = indic_name) %>% 
  # clean columns
  dplyr::select("goal", "objective","indicator_orig","indic_id","indicator_harmonized") %>% 
  # save input data
  write_csv('../input/meas/CITES/cites_indicators.csv')


# Prepare indicators for analisys
indic_cites = cites %>% 
  dplyr::group_by(indicator_harmonized) %>% 
  dplyr::summarise(indic_ids = paste0(indic_id, collapse = ";")) %>% 
  # add source
  dplyr::mutate(cites = 1)

rm(cites)

## Indicators from  RAMSAR----

ramsar = readxl::read_excel("../input/meas/RAMSAR/ramsar.xlsx",
                            sheet = "ramsar") %>% 
  filter(!is.na(id))

ramsar = ramsar %>% 
  # extract indicators
  dplyr::mutate(indic = strsplit(as.character(indicators), "\n")) %>% 
  unnest(indic) %>% 
  dplyr::mutate(indic = str_trim(indic)) %>% 
  # add future indicator
  dplyr::mutate(note_indic = if_else(grepl("[{]",indic),
                                     true= "for future development",
                                     false=NA)) %>% 
  dplyr::mutate(indic = gsub('[{]','',indic)) %>% 
  dplyr::mutate(indic = gsub('[}]','',indic)) %>%
  # get indic_id
  dplyr::mutate(indic_id = paste0(id,"_",row_number())) %>% 
  dplyr::mutate(indic_id = gsub("_T",".",indic_id)) %>% 
  # harmonize indicators
  dplyr::mutate(indicators_h = gsub("[(]Data source[:] National Reports[)][.]","",indic)) %>% 
  dplyr::mutate(indicators_h = gsub("[(]Data source[:] National Reports[)]","",indicators_h)) %>% 
  dplyr::mutate(indicators_h = gsub('  ', '',indicators_h)) %>% 
  dplyr::mutate(indicators_h = tolower(indicators_h)) %>%
  dplyr::mutate(indicators_h = str_trim(indicators_h)) %>% 
  dplyr::mutate(indicators_h = gsub("[.]$","",indicators_h)) %>% 
  harmonize_indic(indicators_h) %>% 
  dplyr::rename(indicator_harmonized = field_harm) %>% 
  dplyr::mutate(indicator_harmonized = str_trim(indicator_harmonized)) %>% 
  dplyr::select(-indicators_h) %>% 
  # keep original indicator name
  dplyr::rename(indicator_orig = indic) %>% 
  # clean columns
  dplyr::select("goal","target", "indicator_orig","indic_id","indicator_harmonized","note_indic") %>% 
  # save input data
  write_csv('../input/meas/RAMSAR/ramsar_indic_harm.csv')


# Prepare indicators for analysis
indic_ramsar = ramsar %>% 
  dplyr::group_by(indicator_harmonized) %>% 
  dplyr::summarise(indic_ids = paste0(indic_id, collapse = ";")) %>% 
  # add source
  dplyr::mutate(ramsar = 1)

## Indicators from tables and appendices in UNCCD----

unccd = readxl::read_excel("../input/meas/UNCCD/UNCCD.xlsx",
                           sheet = "indicators")

unccd = unccd %>% 
  # harmonize indicators
  dplyr::mutate(indicators_h = gsub('  ', '',indicator)) %>% 
  dplyr::mutate(indicators_h = tolower(indicators_h)) %>%
  dplyr::mutate(indicators_h = str_trim(indicators_h)) %>% 
  dplyr::mutate(indicators_h = gsub("[.]$","",indicators_h)) %>% 
  harmonize_indic(indicators_h) %>% 
  dplyr::rename(indicator_harmonized = field_harm) %>% 
  dplyr::mutate(indicator_harmonized = str_trim(indicator_harmonized)) %>% 
  dplyr::select(-indicators_h) %>% 
  # remove qualitative indicators
  dplyr::filter(!is.na(verbatim_indic_id)) %>% 
  # save input data
  write_csv('../input/meas/UNCCD/unccd_indic_harm.csv') %>% 
  # keep original indicator name
  dplyr::rename(indicator_orig = indicator) %>% 
  # clean columns
  dplyr::select("strategic_objective", "indicator_orig","indic_id","indicator_harmonized") %>% 
  # save input data
  write_csv('../input/meas/UNCCD/unccd_indic_harm.csv')

# Prepare indicators for analisys
indic_unccd = unccd %>% 
  dplyr::group_by(indicator_harmonized) %>% 
  dplyr::summarise(indic_ids = paste0(indic_id, collapse = ";")) %>% 
  # add source
  dplyr::mutate(unccd = 1)

## Indicators from tables and appendices in CMS----

cms = readxl::read_excel("../input/meas/CMS/cms.xlsx",
                         sheet = "cms") %>% 
  filter(!is.na(id))

cms = cms %>% 
  # extract indicators
  dplyr::mutate(indic = strsplit(as.character(indicators), ";")) %>% 
  unnest(indic) %>% 
  dplyr::mutate(indic = str_trim(indic)) %>% 
  # add future indicator
  dplyr::mutate(note_indic = if_else(grepl("[{]",indic),
                        true= "for future development",
                        false=NA)) %>% 
  dplyr::mutate(indic = gsub('[{]','',indic)) %>% 
  dplyr::mutate(indic = gsub('[}]','',indic)) %>%
  # get indic_id
  dplyr::mutate(indic_id = paste0(id,"_",row_number())) %>% 
  dplyr::mutate(indic_id = gsub("_T",".",indic_id)) %>% 
  # harmonize indicators
  dplyr::mutate(indicators_h = gsub('  ', '',indic)) %>% 
  dplyr::mutate(indicators_h = tolower(indicators_h)) %>%
  dplyr::mutate(indicators_h = str_trim(indicators_h)) %>% 
  dplyr::mutate(indicators_h = gsub("[.]$","",indicators_h)) %>% 
  harmonize_indic(indicators_h) %>% 
  dplyr::rename(indicator_harmonized = field_harm) %>% 
  dplyr::mutate(indicator_harmonized = str_trim(indicator_harmonized)) %>% 
  dplyr::select(-indicators_h) %>% 
  # keep original indicator name
  dplyr::rename(indicator_orig = indic) %>% 
  # clean columns
  dplyr::select("goal", "target","indicator_orig","indic_id","indicator_harmonized","note_indic") %>% 
  # save input data
  write_csv('../input/meas/CMS/cms_indic_harm.csv')

# Prepare indicators for analisys
indic_cms = cms %>% 
  dplyr::group_by(indicator_harmonized) %>% 
  dplyr::summarise(indic_ids = paste0(indic_id, collapse = ";")) %>% 
  # add source
  dplyr::mutate(cms = 1)


# ## Indicators from tables and appendices in ICCWC----
# 
# iccwc = readxl::read_excel(paste0(git_dir, "input/tables_extraction/ICCWC/ICCWC.xlsx"),
#                            sheet = "indicators")
# 
# indic_iccwc = iccwc %>% 
#   # harmonize indicators
#   dplyr::mutate(indicators_h = tolower(indicators)) %>%
#   dplyr::mutate(indicators_h = str_trim(indicators_h)) %>% 
#   harmonize_indic(indicators_h) %>% 
#   dplyr::rename(indicator_harmonized = field_harm) %>% 
#   dplyr::mutate(indicator_harmonized = str_trim(indicator_harmonized)) %>% 
#   dplyr::select(-indicators_h) %>% 
#   # save input data
#   write_csv(paste0(git_dir,'input/tables_extraction/iccwc_indicators.csv')) %>% 
#   # add source
#   dplyr::mutate(iccwc = 1) %>% 
#   dplyr::distinct(indicator_harmonized, .keep_all = TRUE)  %>% 
#   dplyr::select(indic_id, indicator_harmonized, iccwc)
# 
# rm(iccwc)


# Join
indic_policy = indic_gbf %>%
  # join indicators
  full_join(indic_sdg, by = 'indicator_harmonized') %>%
  full_join(indic_cites, by = 'indicator_harmonized') %>%
  full_join(indic_cms, by = 'indicator_harmonized') %>%
  full_join(indic_ramsar, by = 'indicator_harmonized') %>%
  full_join(indic_unccd, by = 'indicator_harmonized') %>% 
  dplyr::mutate(indic_ids = paste(indic_ids.x,indic_ids.y,indic_ids.x.x,indic_ids.y.y,indic_ids.x.x.x,indic_ids.y.y.y, sep=';',collapse = NULL)) %>% 
  dplyr::mutate(indic_ids = gsub('NA[;]', '',indic_ids)) %>% 
  dplyr::mutate(indic_ids = gsub('[;]NA', '',indic_ids)) %>% 
  dplyr::select("indicator_harmonized","indic_ids","gbf","sdg", "cites","cms","ramsar","unccd") %>% 
  # save data
  write_csv('../input/meas/policy_indicators.csv')
