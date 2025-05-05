# clean you environment
rm(list=ls())

##########################################################################
# Extract indicators from tables and supp material from global assessments
##########################################################################
# Created by Yanina Sica in January 2023
# Updated May 2024

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

library(tidyverse)
library(dplyr)
library(stringr)
library(readxl)
library(readr)


# Load indicators from supplementary material (tables)----
## Indicators from tables and appendices in IPBES----

# SUA
ipbes_sua = readxl::read_excel("../input/assessments/tables_extraction/IPBES/IPBES_suppMat_indicators.xlsx",
                   sheet = "sua_dmr_ch2")

ipbes_sua =  ipbes_sua %>% 
  # extract indicators
  dplyr::filter(!is.na(indicators)) %>% 
  dplyr::mutate(indic_name = word(indicators, start = 2,end = -1, sep = ' ')) %>% 
  # get indic_id
  dplyr::mutate(indic_id = word(indicators, start = 1,end = 1, sep = ' ')) %>% 
  dplyr::mutate(indic_id = paste0("IPBES_SUA_sup_",indic_id, "_",row_number())) %>% 
  # harmonize indicators
  dplyr::mutate(indicators_h = gsub('  ', '',indic_name)) %>% 
  dplyr::mutate(indicators_h = tolower(indicators_h)) %>%
  dplyr::mutate(indicators_h = str_trim(indicators_h)) %>% 
  dplyr::mutate(indicators_h = gsub("[.]$","",indicators_h)) %>% 
  harmonize_indic(indicators_h) %>% 
  dplyr::rename(indicator_harmonized = field_harm) %>% 
  dplyr::mutate(indicator_harmonized = str_trim(indicator_harmonized)) %>% 
  # keep original name
  dplyr::rename(indicator_orig = indic_name) %>% 
  # clean columns
  dplyr::select("indicator_orig","indic_id","indicator_harmonized") %>% 
  # save input data
  write_csv('../input/assessments/tables_extraction/IPBES/sua_sup_indicators.csv')


# Prepare indicators for analysis
indic_sua = ipbes_sua %>% 
  dplyr::group_by(indicator_harmonized) %>% 
  dplyr::summarise(indic_ids = paste0(indic_id, collapse = ";"))

rm(ipbes_sua)

# GA 2.1, GA 2.2, GA 2.3
# ipbes_ga21
ipbes_ga21 = readxl::read_excel("../input/assessments/tables_extraction/IPBES/IPBES_suppMat_indicators.xlsx",
                       sheet = "ga_suppMat_ch2.1_drivers")

ipbes_ga21 = ipbes_ga21 %>% 
  # extract indicators
  dplyr::filter(!is.na(indicators)) %>% 
  # get indic_id
  dplyr::mutate(indic_id = paste0("IPBES_GA2.1_sup_",row_number())) %>% 
  # harmonize indicators
  dplyr::mutate(indicators_h = gsub('  ', '',indicators)) %>% 
  dplyr::mutate(indicators_h = tolower(indicators_h)) %>%
  dplyr::mutate(indicators_h = str_trim(indicators_h)) %>% 
  dplyr::mutate(indicators_h = gsub("[.]$","",indicators_h)) %>% 
  harmonize_indic(indicators_h) %>% 
  dplyr::rename(indicator_harmonized = field_harm) %>% 
  dplyr::mutate(indicator_harmonized = str_trim(indicator_harmonized)) %>% 
  # keep original name
  dplyr::rename(indicator_orig = indicators) %>% 
  # clean columns
  dplyr::select("indicator_orig","indic_id","indicator_harmonized") %>% 
  # save input data
  write_csv('../input/assessments/tables_extraction/IPBES/ga2.1_indicators.csv')

# Prepare indicators for analysis
indic_ga21 = ipbes_ga21 %>% 
  dplyr::group_by(indicator_harmonized) %>% 
  dplyr::summarise(indic_ids = paste0(indic_id, collapse = ";")) 

rm(ipbes_ga21)

# ipbes_ga22
ipbes_ga22 = readxl::read_excel("../input/assessments/tables_extraction/IPBES/IPBES_suppMat_indicators.xlsx",
                        sheet = "ga_suppMat_ch2.2_nature")

ipbes_ga22 = ipbes_ga22 %>% 
  # extract indicators
  dplyr::filter(!is.na(indicators)) %>% 
  # get indic_id
  dplyr::mutate(indic_id = paste0("IPBES_GA2.2_sup_",row_number())) %>% 
  # harmonize indicators
  dplyr::mutate(indicators_h = gsub('  ', '',indicators)) %>% 
  dplyr::mutate(indicators_h = tolower(indicators_h)) %>%
  dplyr::mutate(indicators_h = str_trim(indicators_h)) %>% 
  dplyr::mutate(indicators_h = gsub("[.]$","",indicators_h)) %>% 
  harmonize_indic(indicators_h) %>% 
  dplyr::rename(indicator_harmonized = field_harm) %>% 
  dplyr::mutate(indicator_harmonized = str_trim(indicator_harmonized)) %>% 
  # keep original name
  dplyr::rename(indicator_orig = indicators) %>% 
  # clean columns
  dplyr::select("indicator_orig","indic_id","indicator_harmonized") %>% 
  # save input data
  write_csv('../input/assessments/tables_extraction/IPBES/ga2.2_indicators.csv')

# Prepare indicators for analysis
indic_ga22 = ipbes_ga22 %>% 
  dplyr::group_by(indicator_harmonized) %>% 
  dplyr::summarise(indic_ids = paste0(indic_id, collapse = ";"))

rm(ipbes_ga22)

# ipbes_ga23
ipbes_ga23 = readxl::read_excel("../input/assessments/tables_extraction/IPBES/IPBES_suppMat_indicators.xlsx",
                        sheet = "ga_suppMat_ch2.3_ncp")


ipbes_ga23 = ipbes_ga23 %>% 
  # extract indicators
  dplyr::filter(!is.na(indicators)) %>% 
  dplyr::filter(notes != 'not indicator') %>% 
  # get indic_id
  dplyr::mutate(indic_id = paste0("IPBES_GA2.3_sup_",row_number())) %>% 
  # harmonize indicators
  dplyr::mutate(indicators_h = gsub('  ', '',indicators)) %>% 
  dplyr::mutate(indicators_h = tolower(indicators_h)) %>%
  dplyr::mutate(indicators_h = str_trim(indicators_h)) %>% 
  dplyr::mutate(indicators_h = gsub("[.]$","",indicators_h)) %>% 
  harmonize_indic(indicators_h) %>% 
  dplyr::rename(indicator_harmonized = field_harm) %>% 
  dplyr::mutate(indicator_harmonized = str_trim(indicator_harmonized)) %>% 
  # keep original name
  dplyr::rename(indicator_orig = indicators) %>% 
  # clean columns
  dplyr::select("indicator_orig","indic_id","indicator_harmonized") %>% 
  # save input data
  write_csv('../input/assessments/tables_extraction/IPBES/ga2.3_indicators.csv')

# Prepare indicators for analysis
indic_ga23 = ipbes_ga23 %>% 
  dplyr::group_by(indicator_harmonized) %>% 
  dplyr::summarise(indic_ids = paste0(indic_id, collapse = ";")) 

rm(ipbes_ga23)

# ipbes_ga3 

ipbes_ga3 = readxl::read_excel("../input/assessments/tables_extraction/IPBES/IPBES_suppMat_indicators.xlsx",
                        sheet = "ga_suppMat_ch3")

ipbes_ga3 = ipbes_ga3 %>% 
  # extract indicators
  dplyr::filter(!is.na(indicators)) %>% 
  # get indic_id
  dplyr::mutate(indic_id = paste0("IPBES_GA3_sup_",row_number())) %>% 
  # harmonize indicators
  dplyr::mutate(indicators_h = gsub('  ', '',indicators)) %>% 
  dplyr::mutate(indicators_h = tolower(indicators_h)) %>%
  dplyr::mutate(indicators_h = str_trim(indicators_h)) %>% 
  dplyr::mutate(indicators_h = gsub("[.]$","",indicators_h)) %>% 
  harmonize_indic(indicators_h) %>% 
  dplyr::rename(indicator_harmonized = field_harm) %>% 
  dplyr::mutate(indicator_harmonized = str_trim(indicator_harmonized)) %>% 
  # keep original name
  dplyr::rename(indicator_orig = indicators) %>% 
  # clean columns
  dplyr::select("indicator_orig","indic_id","indicator_harmonized") %>% 
  # save input data
  write_csv('../input/assessments/tables_extraction/IPBES/ga3_indicators.csv')

# Prepare indicators for analysis
indic_ga3 = ipbes_ga3 %>% 
  dplyr::group_by(indicator_harmonized) %>% 
  dplyr::summarise(indic_ids = paste0(indic_id, collapse = ";")) 

rm(ipbes_ga3)

# combine all IPBES indicators from supplementary material
indic_ipbes_sup = indic_ga21 %>% 
  rbind(indic_ga22) %>% 
  rbind(indic_ga23) %>% 
  rbind(indic_ga3) %>% 
  rbind(indic_sua) %>% 
  dplyr::group_by(indicator_harmonized) %>% 
  dplyr::summarise(indic_ids = paste0(indic_ids, collapse = ";")) %>% 
  #create source
  dplyr::mutate(ipbes_sup = 1) %>% 
  # clean columns
  dplyr::select(indicator_harmonized,indic_ids,ipbes_sup) %>% 
  # save input data
  write_csv('../input/assessments/tables_extraction/ipbes_sup_indicators.csv')

rm(indic_ga21, indic_ga22,indic_ga23,indic_ga3,indic_sua)  
  
## Indicators from tables and appendices in IPCC----

ipccTAVI1 = readxl::read_excel("../input/assessments/tables_extraction/IPCC/AR6_WG1.xlsx",
                  sheet = "ANEX VI-Table AVI.1")%>% 
  dplyr::select(indicators = `Index Name`)
ipccTAVI2 = readxl::read_excel("../input/assessments/tables_extraction/IPCC/AR6_WG1.xlsx",
                               sheet = "ANEX VI-Table AVI.2")%>% 
  dplyr::select(indicators = Index)

ipcc = ipccTAVI1 %>% 
  rbind(ipccTAVI2) %>% 
  # extract indicators
  dplyr::distinct(indicators) %>% 
  dplyr::filter(!is.na(indicators)) %>% 
  # get indic_id
  dplyr::mutate(indic_id = paste0("IPCC_sup_",row_number())) %>% 
  # harmonize indicators
  dplyr::mutate(indicators_h = gsub('  ', '',indicators)) %>% 
  dplyr::mutate(indicators_h = tolower(indicators_h)) %>%
  dplyr::mutate(indicators_h = str_trim(indicators_h)) %>% 
  dplyr::mutate(indicators_h = gsub("[.]$","",indicators_h)) %>% 
  harmonize_indic(indicators_h) %>% 
  dplyr::rename(indicator_harmonized = field_harm) %>% 
  dplyr::mutate(indicator_harmonized = str_trim(indicator_harmonized)) %>% 
  # keep original name
  dplyr::rename(indicator_orig = indicators) %>% 
  # clean columns
  dplyr::select("indicator_orig","indic_id","indicator_harmonized") %>% 
  # save input data
  write_csv('../input/assessments/tables_extraction/IPCC/IPCC_sup_indicators.csv')

# Prepare indicators for analysis
indic_ipcc_sup = ipcc %>% 
  dplyr::group_by(indicator_harmonized) %>% 
  dplyr::summarise(indic_ids = paste0(indic_id, collapse = ";")) %>% 
  #create source
  dplyr::mutate(ipcc_sup = 1) %>% 
  # save input data
  write_csv('../input/assessments/tables_extraction/ipcc_sup_indicators.csv')


rm(ipccTAVI1, ipccTAVI2, ipcc)

# ## Append indicators from tables and appendices----
# 
# indic_ipcc = read_csv(paste0(git_dir,'input/tables_extraction/ipcc_indicators.csv')) %>% 
#   # add source
#   dplyr::mutate(ipcc_sup = 1) %>% 
#   dplyr::distinct(indicators_harmonized, .keep_all = TRUE)  %>% 
#   dplyr::select(indicators_harmonized, ipcc_sup)
# 
# indic_ga = read_csv(paste0(git_dir,'input/tables_extraction/ga_sup_indicators.csv')) %>% 
#   # add source
#   dplyr::mutate(ga_sup = 1) %>% 
#   dplyr::distinct(indicators_harmonized, .keep_all = TRUE)  %>% 
#   dplyr::select(indicators_harmonized, ga_sup)
# 
# indic_sua = read_csv(paste0(git_dir,'input/tables_extraction/sua_sup_indicators.csv')) %>% 
#   # add source
#   dplyr::mutate(sua_sup = 1) %>% 
#   dplyr::distinct(indicators_harmonized, .keep_all = TRUE)  %>% 
#   dplyr::select(indicators_harmonized, sua_sup)
# 
# # Check independent files
# dup = check_dup(indic_ipcc, indicators_harmonized) # no exact duplicates
# dup = check_dup(indic_ga, indicators_harmonized) # no exact duplicates
# dup = check_dup(indic_sua, indicators_harmonized) # no exact duplicates
# rm(dup)
# 
# # Join
# indic_supp_tables = indic_ipcc %>% 
#   full_join(indic_ga, by = 'indicators_harmonized') %>% 
#   full_join(indic_sua, by = 'indicators_harmonized')
# 
# # checks joined indicators
# dup = check_dup(indic_supp_tables, indicators_harmonized) # no exact duplicates
# 
# indic_supp_tables = indic_supp_tables %>% 
#   # harmonize indicators
#   dplyr::mutate(indicators_h = tolower(indicators_harmonized)) %>%
#   dplyr::mutate(indicators_h = str_trim(indicators_h)) %>% 
#   harmonize_indic(indicators_h)
# dup = check_dup(indic_supp_tables, indicators_h) # no exact duplicates
# 
# # save
# indic_supp_tables = indic_supp_tables %>% 
#   # clean table
#   dplyr::select(-indicators_h, -field_harm) %>%
#   # save data
#   write_csv(paste0(git_dir,'input/tables_extraction/IPBES-IPCC_supp_tables_indicators.csv'))

### NOT INCLUDED IN MANUSCRIPT ###
### IPBES core and highlighted indicators (D&K TF 2017-2018)----
# core_high = readxl::read_excel(paste0(git_dir, "input/tables_extraction/IPBES/IPBES_2018.xlsx"),
#                        sheet = "core-high")
# 
# core_high %>%  group_by(`type of indicator`) %>% distinct() %>% count()
# # core              30
# # highlighted       42
# 
# indic_core_high = core_high %>% 
#   mutate(indicators = tolower(`Specific Indicator (matched)`)) %>% 
#   mutate(type_indicators = tolower(`type of indicator`)) %>% 
#   # harmonize indicators
#   dplyr::mutate(indicators_h = tolower(indicators)) %>%
#   dplyr::mutate(indicators_h = str_trim(indicators_h)) %>% 
#   harmonize_indic(indicators_h) %>% 
#   dplyr::rename(indicators_harmonized = field_harm) %>% 
#   dplyr::mutate(indicators_harmonized = str_trim(indicators_harmonized)) %>% 
#   dplyr::select(-indicators_h) %>% 
#   # save input data
#   write_csv(paste0(git_dir,'input/tables_extraction/ipbes_core_high_indicators.csv')) %>% 
#   # add source
#   dplyr::mutate(core_high = 1) %>% 
#   dplyr::distinct(indicators_harmonized, .keep_all = TRUE)  %>% 
#   dplyr::select(indic_id, indicators_harmonized, core_high, type_indicators)
# 
# indic_core_high %>%  distinct(indicators_harmonized) %>% count() # 72



#################################### END


