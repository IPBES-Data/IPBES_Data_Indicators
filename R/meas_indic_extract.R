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

headline =  read_csv("../input/tables_extraction/GBF/2025/headline_indicators_2025-01-15.csv") %>% 
  dplyr::select("Goal/target","Indicator name", "Group") %>% 
  rename(indicator_type = Group) %>% 
  # remove the goals in indicator name
  dplyr::mutate(`Indicator name` = word(`Indicator name`, start = 2,end = -1, sep = ' ')) %>% 
  # create ID
  mutate(indic_id = paste0('GBF_',`Goal/target`,'_H_',row_number())) %>% 
  mutate(indic_id = gsub('Goal ', 'G', indic_id)) %>% 
  mutate(indic_id = gsub('Target ', 'T', indic_id)) 

#names(headline)

# Join all indicators
gbf_2025 = headline %>% 
  # Join all types of indicators proposed
  rbind(component,complementary) %>% 
  mutate(indicator_name = gsub('None adopted','',`Indicator name`))
write_csv(gbf_2025, '../input/meas/GBF/gbf_indicators.csv')

# Harmonize indicators
gbf_2025 = gbf_2025 %>% 
  # Filter out indicators that do not exist
  filter(`Indicator name` != 'None adopted') %>% 
  # harmonize indicators
  dplyr::mutate(indicators_h = tolower(`Indicator name`)) %>%
  dplyr::mutate(indicators_h = gsub('  ', '',indicators_h)) %>% 
  dplyr::mutate(indicators_h = str_trim(indicators_h)) %>% 
  dplyr::mutate(indicators_h = gsub(".$","",indicators_h)) %>% 
  harmonize_indic(indicators_h) %>% 
  dplyr::rename(indicator_harmonized = field_harm) %>% 
  dplyr::mutate(indicator_harmonized = str_trim(indicator_harmonized)) %>% 
  dplyr::select(-indicators_h, -indicator_name) %>% 
  # keep original name
  dplyr::rename(indicator_orig = `Indicator name`)
write_csv(gbf_2025, '../input/meas/GBF/gbf_indic_harm.csv')
#gbf_2025 = read_csv('../input/meas/GBF/gbf_indic_harm.csv')

gbf_2025 %>% distinct(indicator_harmonized) %>% count() # 323
gbf_2025 %>% distinct(indicator_orig) %>% count() # 331

# Prepare indicators for analisys
indic_gbf = gbf_2025 %>% 
  dplyr::group_by(indicator_harmonized) %>% 
  dplyr::summarise(indic_ids = paste0(indic_id, collapse = ";")) %>% 
  # add source
  dplyr::mutate(gbf = 1)

rm(gbf_2025, headline, component, complementary)

## Indicators from SDGs----

sdg = readxl::read_excel("../input/meas/SDGs/SDG_indicators.xlsx",
                         sheet = "sdg_indicators")

sdg = sdg %>% 
  # harmonize indicators
  dplyr::mutate(indicators_h = gsub('  ', '',indicators)) %>% 
  dplyr::mutate(indicators_h = tolower(indicators_h)) %>%
  dplyr::mutate(indicators_h = str_trim(indicators_h)) %>% 
  dplyr::mutate(indicators_h = gsub(".$","",indicators_h)) %>% 
  harmonize_indic(indicators_h) %>% 
  dplyr::rename(indicator_harmonized = field_harm) %>% 
  dplyr::mutate(indicator_harmonized = str_trim(indicator_harmonized)) %>% 
  dplyr::select(-indicators_h) %>% 
  # Remove 11c which is in development
  dplyr::filter(!is.na(indicator_harmonized)) %>% 
  # keep original name
  dplyr::rename(indicator_orig = indicators)

write_csv(sdg, '../input/meas/SDGs/sdg_indic_harm.csv')
  

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
  #extract indicators
  dplyr::mutate(indic = strsplit(as.character(indicators), "\n")) %>% 
  unnest(indic) %>% 
  dplyr::mutate(indic = gsub('Indicator ','',indic)) %>%
  dplyr::mutate(indic = gsub('[:]','',indic)) %>%
  # get indic_id
  dplyr::mutate(indic_id = word(indic, start = 1,end = 1, sep = ' ')) %>% 
  dplyr::mutate(indic_name = word(indic, start = 2,end = -1, sep = ' ')) %>% 
  dplyr::mutate(indic_id = paste0(id,"_",indic_id)) %>% 
  dplyr::select(-indicators, -indic) %>% 
  # harmonize indicators
  dplyr::mutate(indicators_h = tolower(indic_name)) %>%
  dplyr::mutate(indicators_h = str_trim(indicators_h)) %>% 
  dplyr::mutate(indicators_h = gsub('  ', '',indicators_h)) %>% 
  dplyr::mutate(indicators_h = gsub(".$","",indicators_h)) %>% 
  harmonize_indic(indicators_h) %>% 
  dplyr::rename(indicator_harmonized = field_harm) %>% 
  dplyr::mutate(indicator_harmonized = str_trim(indicator_harmonized)) %>% 
  dplyr::select(-indicators_h) %>% 
  # keep original name
  dplyr::rename(indicator_orig = indic_name)
write_csv(cites,'../input/meas/CITES/cites_indicators.csv')


# Prepare indicators for analisys
indic_cites = cites %>% 
  dplyr::group_by(indicator_harmonized) %>% 
  dplyr::summarise(indic_ids = paste0(indic_id, collapse = ";")) %>% 
  # add source
  dplyr::mutate(cites = 1)

rm(cites)

## Indicators from  RAMSAR----

ramsar = readxl::read_excel(paste0(git_dir, "input/tables_extraction/RAMSAR/ramsar.xlsx"),
                            sheet = "indicators")

indic_ramsar = ramsar %>% 
  # harmonize indicators
  dplyr::mutate(indicators_h = tolower(indicators)) %>%
  dplyr::mutate(indicators_h = str_trim(indicators_h)) %>% 
  harmonize_indic(indicators_h) %>% 
  dplyr::rename(indicator_harmonized = field_harm) %>% 
  dplyr::mutate(indicator_harmonized = str_trim(indicator_harmonized)) %>% 
  dplyr::select(-indicators_h) %>% 
  # save input data
  write_csv(paste0(git_dir,'input/tables_extraction/ramsar_indicators.csv')) %>% 
  # add source
  dplyr::mutate(ramsar = 1) %>% 
  dplyr::distinct(indicator_harmonized, .keep_all = TRUE)  %>% 
  dplyr::select(indic_id, indicator_harmonized, ramsar)

rm(ramsar)

## Indicators from tables and appendices in UNCCD----

unccd = readxl::read_excel(paste0(git_dir, "input/tables_extraction/UNCCD/UNCCD.xlsx"),
                           sheet = "indicators")

indic_unccd = unccd %>% 
  # harmonize indicators
  dplyr::mutate(indicators_h = tolower(indicators)) %>%
  dplyr::mutate(indicators_h = str_trim(indicators_h)) %>% 
  harmonize_indic(indicators_h) %>% 
  dplyr::rename(indicator_harmonized = field_harm) %>% 
  dplyr::mutate(indicator_harmonized = str_trim(indicator_harmonized)) %>% 
  dplyr::select(-indicators_h) %>% 
  # save input data
  write_csv(paste0(git_dir,'input/tables_extraction/unccd_indicators.csv')) %>% 
  # add source
  dplyr::mutate(unccd = 1) %>% 
  dplyr::distinct(indicator_harmonized, .keep_all = TRUE)  %>% 
  dplyr::select(indic_id, indicator_harmonized, unccd)

rm(unccd)

## Indicators from tables and appendices in CMS----

cms = readxl::read_excel(paste0(git_dir, "input/tables_extraction/CMS/CMS.xlsx"),
                         sheet = "indicators")

indic_cms = cms %>% 
  #rename
  dplyr::rename(indicators = indicators_matched) %>% 
  # harmonize indicators
  dplyr::mutate(indicators_h = tolower(indicators)) %>%
  dplyr::mutate(indicators_h = str_trim(indicators_h)) %>% 
  harmonize_indic(indicators_h) %>% 
  dplyr::rename(indicator_harmonized = field_harm) %>% 
  dplyr::mutate(indicator_harmonized = str_trim(indicator_harmonized)) %>% 
  dplyr::select(-indicators_h) %>% 
  # save input data
  write_csv(paste0(git_dir,'input/tables_extraction/cms_indicators.csv')) %>% 
  # add source
  dplyr::mutate(cms = 1) %>% 
  dplyr::distinct(indicator_harmonized, .keep_all = TRUE)  %>% 
  dplyr::select(indic_id, indicator_harmonized, cms)

rm(cms)

## Indicators from tables and appendices in ICCWC----

iccwc = readxl::read_excel(paste0(git_dir, "input/tables_extraction/ICCWC/ICCWC.xlsx"),
                           sheet = "indicators")

indic_iccwc = iccwc %>% 
  # harmonize indicators
  dplyr::mutate(indicators_h = tolower(indicators)) %>%
  dplyr::mutate(indicators_h = str_trim(indicators_h)) %>% 
  harmonize_indic(indicators_h) %>% 
  dplyr::rename(indicator_harmonized = field_harm) %>% 
  dplyr::mutate(indicator_harmonized = str_trim(indicator_harmonized)) %>% 
  dplyr::select(-indicators_h) %>% 
  # save input data
  write_csv(paste0(git_dir,'input/tables_extraction/iccwc_indicators.csv')) %>% 
  # add source
  dplyr::mutate(iccwc = 1) %>% 
  dplyr::distinct(indicator_harmonized, .keep_all = TRUE)  %>% 
  dplyr::select(indic_id, indicator_harmonized, iccwc)

rm(iccwc)
