# clean you environment
rm(list=ls())

############################################################################
# Match MEA indicators to GBF
# Work in progress for MTA
#
# Created by Yanina Sica in January 2025
############################################################################


### Settings----

## Your working directory (will be set using function in setting.R)
your_dir <- dirname(rstudioapi::getSourceEditorContext()$path) # works only in RStudio
#your_dir <- "path_to_your_chosen_working_directory" # complete accordingly

## Source useful functions from folder downloaded from GitHub
source(paste0(your_dir,"/useful_functions_indic.R"))
source(paste0(your_dir,"/settings.R"))

## Choose directories where you want to read/store data 
#input_dir='set_your_own_input_dir'
#output_dir='set_your_own_output_dir'

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

# Set GBF as the main framework----

# Load GBF indicators (2024)
gbf_2024 =  read_csv('../input/tables_extraction/km_gbf_indicators.csv')
gbf_2024 %>%  group_by(indicator_type) %>% count()

# Get 2025 data and compare
complementary =  read_csv("../input/tables_extraction/GBF/2025/complementary_indicators_2025-01-15.csv") %>% 
  dplyr::select("Goal/target","Indicator name") %>% 
  mutate(indicator_type = 'complementary') %>% 
  # create ID
  mutate(indic_id = paste0('GBF_CY_',`Goal/target`,"_",row_number())) %>% 
  mutate(indic_id = gsub('Goal ', 'G', indic_id)) %>% 
  mutate(indic_id = gsub('Target ', 'T', indic_id)) 
  
component =  read_csv("../input/tables_extraction/GBF/2025/component_indicators_2025-01-15.csv") %>% 
  dplyr::select("Goal/target","Indicator name") %>% 
  mutate(indicator_type = 'component') %>% 
  # create ID
  mutate(indic_id = paste0('GBF_C_',`Goal/target`,"_",row_number())) %>% 
  mutate(indic_id = gsub('Goal ', 'G', indic_id)) %>% 
  mutate(indic_id = gsub('Target ', 'T', indic_id)) 

headline =  read_csv("../input/tables_extraction/GBF/2025/headline_indicators_2025-01-15.csv") %>% 
  dplyr::select("Goal/target","Indicator name", "Group") %>% 
  rename(indicator_type = Group) %>% 
  # remove the goals in indicator name
  dplyr::mutate(`Indicator name` = word(`Indicator name`, start = 2,end = -1, sep = ' ')) %>% 
  # create ID
  mutate(indic_id = paste0('GBF_H_',`Goal/target`,"_",row_number())) %>% 
  mutate(indic_id = gsub('Goal ', 'G', indic_id)) %>% 
  mutate(indic_id = gsub('Target ', 'T', indic_id)) 

names(headline)

gbf_2025 = headline %>% 
  # Join all types of indicators proposed
  rbind(component,complementary) %>% 
  # Filter out indicators that do not exist
  filter(`Indicator name` != 'None adopted') %>% 
  # harmonize indicators
  dplyr::mutate(indicators_h = tolower(`Indicator name`)) %>%
  dplyr::mutate(indicators_h = str_trim(indicators_h)) %>% 
  harmonize_indic(indicators_h) %>% 
  dplyr::rename(indicators_harmonized = field_harm) %>% 
  dplyr::mutate(indicators_harmonized = str_trim(indicators_harmonized)) %>% 
  dplyr::select(-indicators_h) %>% 
  dplyr::rename(indicators_orig = `Indicator name`)
write_csv(gbf_2025, '../input/tables_extraction/km_gbf_indicators2025.csv')
gbf_2025 = read_csv('../input/tables_extraction/km_gbf_indicators2025.csv')


gbf_2025 %>%  group_by(indicator_type) %>% count()
gbf_2024 %>%  group_by(indicator_type) %>% count()

mismatch1 = anti_join(gbf_2024, gbf_2025, by = 'indicators_harmonized')
mismatch2 = anti_join(gbf_2025,gbf_2024, by = 'indicators_harmonized')
# SOME NEW INDICATORS BUT MOSTLY DIFFERENT NAMING --> use 2025 data

# Load other MEAs----

# Add CMS
cms =  read_csv('../input/tables_extraction/cms_indicators.csv') %>% 
  dplyr::select(indic_id, indicators_orig = indicator, indicators_harmonized)

# Add CITES
cites =  read_csv('../input/tables_extraction/cites_indicators.csv') %>% 
  dplyr::select(indic_id, indicators_orig = indicators, indicators_harmonized)

# Add RAMSAR
ramsar =  read_csv('../input/tables_extraction/ramsar_indicators.csv') %>% 
  dplyr::select(indic_id, indicators_orig = indicators, indicators_harmonized)


# Join by indicators_harmonized

meas = gbf_2025 %>%
  dplyr::select(-indicator_type) %>% 
  # add cms
  full_join(cms, by = 'indicators_harmonized') %>%
  mutate(indic_id = coalesce(indic_id.x, indic_id.y),
            indicators_orig = coalesce(indicators_orig.x, indicators_orig.y)) %>% 
  dplyr::select(-indic_id.x, -indic_id.y,-indicators_orig.x, -indicators_orig.y) %>% 
  # add cites
  full_join(cites, by = 'indicators_harmonized') %>%
  mutate(indic_id = coalesce(indic_id.x, indic_id.y),
         indicators_orig = coalesce(indicators_orig.x, indicators_orig.y)) %>% 
  dplyr::select(-indic_id.x, -indic_id.y,-indicators_orig.x, -indicators_orig.y) %>% 
  # add ramsar
  full_join(ramsar, by = 'indicators_harmonized') %>%
  mutate(indic_id = coalesce(indic_id.x, indic_id.y),
         indicators_orig = coalesce(indicators_orig.x, indicators_orig.y)) %>% 
  dplyr::select(-indic_id.x, -indic_id.y,-indicators_orig.x, -indicators_orig.y)

# Join with classified indicators
policy_indic_class = read_csv('../output/policy_indicatorsMay24_orignames.csv')

meas_classified =  meas %>%
  left_join(policy_indic_class, by = 'indicators_harmonized') %>% 
  rename(indicators_orig = indicators_orig.x) %>% 
  dplyr::select(-ids, -indicators_orig.y)
write_csv(meas_classified, '../input/gbf_framework/meas_classified_gbf.csv')
meas = read_csv('../input/gbf_framework/meas_gbf.csv')