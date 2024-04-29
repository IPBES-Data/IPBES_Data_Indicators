 rm(list=ls())

##################################################################################
# Extract indicators from tables and supp material from global assessments and MEA
##################################################################################
# Created by Yanina Sica in January 2023
# Updated Sep 2023

### Settings----

## Source useful functions from folders downloaded from GitHub
source("C:/Users/yanis/Documents/scripts/IPBES-Data/IPBES_Data_Indicators/code/settings.R")
source("C:/Users/yanis/Documents/scripts/IPBES-Data/IPBES_Data_Indicators/code/useful_functions_indic.R")

## Set working directory
your_user <- Sys.info()["user"]
your_node <- Sys.info()["nodename"]

your_workdir <- "C:/Users/yanis/Documents/scripts/IPBES-Data/IPBES_Data_Indicators" 

set_working_dir(your_user,your_node)

## Packages

library(tidyverse)
library(dplyr)

library(stringr)
library(readxl)
library(readr)

#install.packages("googlesheets4")
library(googlesheets4)

# Set data directories
gd_dir = 'G:/.shortcut-targets-by-id/1H9meyomoJK2QW531mPnEcynUYZOl3Itm/_TSU Internal/_ Weekly reports/Files - TSU Data/indicators/'
git_dir = "C:/Users/yanis/Documents/scripts/IPBES-Data/IPBES_Data_Indicators/"

# Load indicators from supplementary material (tables)----
## Indicators from tables and appendices in IPBES----
# Accessible in the shared drive https://drive.google.com/drive/u/0/folders/17hxQ5IelJkz5CaMlSthCo1sl22V2sEmS
# or locally in github input data  folder: 'C:\Users\yanis\Documents\scripts\IPBES-Data\IPBES_Data_Indicators\input\tables_extraction'

# SUA
ipbes_sua = read_sheet("https://docs.google.com/spreadsheets/d/1meGdQdz7g5xeRGm7mD8NPgDv4ugTdutrdVmZTiaZgo0/edit?usp=sharing",
                   sheet = "sua_dmr_ch2")

indic_sua =  ipbes_sua %>% 
  # harmonize indicators
  dplyr::mutate(indicators_h = tolower(indicators)) %>% 
  dplyr::mutate(indicators_h = str_trim(indicators_h)) %>% 
  harmonize_indic(indicators_h) %>% 
  dplyr::rename(indicators_harmonized = field_harm) %>% 
  dplyr::mutate(indicators_harmonized = str_trim(indicators_harmonized)) %>% 
  dplyr::select(-indicators_h) %>%
  # save input data
  write_csv(paste0(git_dir,'input/tables_extraction/sua_sup_indicators.csv')) %>% 
  write_csv(paste0(gd_dir,'input_data2/tables_extraction/sua_sup_indicators.csv')) %>% 
  # add source
  dplyr::mutate(sua_sup = 1) %>% 
  dplyr::distinct(indicators_harmonized, .keep_all = TRUE)  %>% 
  dplyr::select(indic_id, indicators_harmonized, sua_sup)
 
rm(ipbes_sua)

# GA 2.1, GA 2.2, GA 2.3
ipbes_ga21 = read_sheet("https://docs.google.com/spreadsheets/d/1meGdQdz7g5xeRGm7mD8NPgDv4ugTdutrdVmZTiaZgo0/edit?usp=sharing",
                       sheet = "ga_suppMat_ch2.1_drivers")

indic_ga21 = ipbes_ga21 %>% 
  dplyr::rename(indicators = Indicator) %>% 
  # clean indicators
  dplyr::mutate(indicators_h = tolower(indicators)) %>%
  dplyr::mutate(indicators_h = str_trim(indicators_h)) %>% 
  dplyr::distinct(indicators_h, .keep_all = TRUE)  %>% 
  dplyr::select(indic_id, indicators,indicators_h)


ipbes_ga22 = read_sheet("https://docs.google.com/spreadsheets/d/1meGdQdz7g5xeRGm7mD8NPgDv4ugTdutrdVmZTiaZgo0/edit?usp=sharing",
                        sheet = "ga_suppMat_ch2.2_nature")

indic_ga22 = ipbes_ga22 %>% 
  dplyr::rename(indicators = `Indicator (matched)`) %>% 
  # clean indicators
  dplyr::mutate(indicators_h = tolower(indicators)) %>%
  dplyr::mutate(indicators_h = str_trim(indicators_h)) %>% 
  dplyr::distinct(indicators_h, .keep_all = TRUE)  %>% 
  dplyr::select(indic_id, indicators,indicators_h)


ipbes_ga23 = read_sheet("https://docs.google.com/spreadsheets/d/1meGdQdz7g5xeRGm7mD8NPgDv4ugTdutrdVmZTiaZgo0/edit?usp=sharing",
                        sheet = "ga_suppMat_ch2.3_ncp")

indic_ga23 = ipbes_ga23 %>% 
  dplyr::rename(indicators = `Indicator (matched)`) %>% 
  # clean indicators
  dplyr::mutate(indicators_h = tolower(indicators)) %>%
  dplyr::mutate(indicators_h = str_trim(indicators_h)) %>% 
  dplyr::distinct(indicators_h, .keep_all = TRUE)  %>% 
  dplyr::select(indic_id, indicators,indicators_h)

ipbes_ga3 = read_sheet("https://docs.google.com/spreadsheets/d/1meGdQdz7g5xeRGm7mD8NPgDv4ugTdutrdVmZTiaZgo0/edit?usp=sharing",
                        sheet = "ga_suppMat_ch3")

indic_ga3 = ipbes_ga3 %>% 
  dplyr::rename(indicators = `Indicator name`) %>% 
  # clean indicators
  dplyr::mutate(indicators_h = tolower(indicators)) %>%
  dplyr::mutate(indicators_h = str_trim(indicators_h)) %>% 
  dplyr::distinct(indicators_h, .keep_all = TRUE)  %>% 
  dplyr::select(indic_id, indicators, indicators_h)

rm(ipbes_ga21, ipbes_ga22,ipbes_ga23,ipbes_ga3)  

# combine all GA indicators from supplementary material
indic_ga = indic_ga21 %>% 
  # harmonize indicators and bind
  harmonize_indic(indicators_h) %>% 
  rbind(harmonize_indic(indic_ga22,indicators_h)) %>% 
  rbind(harmonize_indic(indic_ga23,indicators_h)) %>% 
  rbind(harmonize_indic(indic_ga3,indicators_h)) %>% 
  dplyr::rename(indicators_harmonized = field_harm) %>% 
  dplyr::mutate(indicators_harmonized = str_trim(indicators_harmonized)) %>% 
  dplyr::select(-indicators_h) %>% 
  # save input data
  write_csv(paste0(git_dir,'input/tables_extraction/ga_sup_indicators.csv')) %>% 
  write_csv(paste0(gd_dir,'input_data2/tables_extraction/ga_sup_indicators.csv')) %>% 
  # add source
  dplyr::mutate(ga_sup = 1) %>% 
  dplyr::distinct(indicators_harmonized, .keep_all = TRUE) %>% 
  dplyr::select(indic_id,indicators_harmonized,ga_sup)

rm(indic_ga21, indic_ga22,indic_ga23,indic_ga3)  
  
## Indicators from tables and appendices in IPCC----

ipcc = read_sheet("https://docs.google.com/spreadsheets/d/1shVDLErTSXMQts4_edJaId37jYgb2XpHfKA1flhdOfU/edit#gid=1983992462",
                  sheet = "ANEX VI-Table AVI.1")

indic_ipcc = ipcc %>% 
  dplyr::rename(indicators = `Index Name`) %>% 
  # harmonize indicators
  dplyr::mutate(indicators_h = tolower(indicators)) %>%
  dplyr::mutate(indicators_h = str_trim(indicators_h)) %>% 
  harmonize_indic(indicators_h) %>% 
  dplyr::rename(indicators_harmonized = field_harm) %>% 
  dplyr::mutate(indicators_harmonized = str_trim(indicators_harmonized)) %>% 
  dplyr::select(-indicators_h) %>% 
  # save input data
  write_csv(paste0(git_dir,'input/tables_extraction/ipcc_indicators.csv')) %>% 
  write_csv(paste0(gd_dir,'input_data2/tables_extraction/ipcc_indicators.csv')) %>% 
  # add source
  dplyr::mutate(ipcc_sup = 1) %>% 
  dplyr::distinct(indicators_harmonized, .keep_all = TRUE)  %>% 
  dplyr::select(indic_id, indicators_harmonized, ipcc_sup)

rm(ipcc)
  

## Append indicators from tables and appendices----

indic_ipcc = read_csv(paste0(git_dir,'input/tables_extraction/ipcc_indicators.csv')) %>% 
  # add source
  dplyr::mutate(ipcc_sup = 1) %>% 
  dplyr::distinct(indicators_harmonized, .keep_all = TRUE)  %>% 
  dplyr::select(indicators_harmonized, ipcc_sup)

indic_ga = read_csv(paste0(git_dir,'input/tables_extraction/ga_sup_indicators.csv')) %>% 
  # add source
  dplyr::mutate(ga_sup = 1) %>% 
  dplyr::distinct(indicators_harmonized, .keep_all = TRUE)  %>% 
  dplyr::select(indicators_harmonized, ga_sup)

indic_sua = read_csv(paste0(git_dir,'input/tables_extraction/sua_sup_indicators.csv')) %>% 
  # add source
  dplyr::mutate(sua_sup = 1) %>% 
  dplyr::distinct(indicators_harmonized, .keep_all = TRUE)  %>% 
  dplyr::select(indicators_harmonized, sua_sup)

# Check independent files
dup = check_dup(indic_ipcc, indicators_harmonized) # no exact duplicates
dup = check_dup(indic_ga, indicators_harmonized) # no exact duplicates
dup = check_dup(indic_sua, indicators_harmonized) # no exact duplicates
rm(dup)

# Join
indic_supp_tables = indic_ipcc %>% 
  full_join(indic_ga, by = 'indicators_harmonized') %>% 
  full_join(indic_sua, by = 'indicators_harmonized')

# checks joined indicators
dup = check_dup(indic_supp_tables, indicators_harmonized) # no exact duplicates

indic_supp_tables = indic_supp_tables %>% 
  # harmonize indicators
  dplyr::mutate(indicators_h = tolower(indicators_harmonized)) %>%
  dplyr::mutate(indicators_h = str_trim(indicators_h)) %>% 
  harmonize_indic(indicators_h)
dup = check_dup(indic_supp_tables, indicators_h) # no exact duplicates

# save
indic_supp_tables = indic_supp_tables %>% 
  # clean table
  dplyr::select(-indicators_h, -field_harm) %>%
  # save data
  write_csv(paste0(git_dir,'input/tables_extraction/IPBES-IPCC_supp_tables_indicators.csv')) %>% 
  write_csv(paste0(gd_dir,'input_data2/tables_extraction/IPBES-IPCC_supp_tables_indicators.csv'))

#indic_supp_tables = read_csv(paste0(gd_dir,'input_data2/tables_extraction/IPBES-IPCC_supp_tables_indicators.csv'))


# Load indicators from conventions (policy)----
## Indicators from tables and appendices in KM_GBF----

km_gbf = read_sheet("https://docs.google.com/spreadsheets/d/1_knKw-wf5a1-uMjFCOxD1ZdmJT8KZwSWylUv23lDlws/edit#gid=0",
                       sheet = "indicators_all")

indic_km_gbf = km_gbf %>% 
  dplyr::rename(indicators = `Indicator (matched)`) %>% 
  # harmonize indicators
  dplyr::mutate(indicators_h = tolower(indicators)) %>%
  dplyr::mutate(indicators_h = str_trim(indicators_h)) %>% 
  harmonize_indic(indicators_h) %>% 
  dplyr::rename(indicators_harmonized = field_harm) %>% 
  dplyr::mutate(indicators_harmonized = str_trim(indicators_harmonized)) %>% 
  dplyr::select(-indicators_h) %>% 
  # remove targets without indicators proposed
  dplyr::filter(indicators_harmonized != 'in development*') %>% 
  dplyr::filter(indicators_harmonized != 'none adopted') %>% 
  # save input data
  write_csv(paste0(git_dir,'input/tables_extraction/km_gbf_indicators.csv')) %>% 
  write_csv(paste0(gd_dir,'input_data2/tables_extraction/km_gbf_indicators.csv')) %>% 
  # add source
  dplyr::mutate(km_gbf = 1) %>% 
  dplyr::distinct(indicators_harmonized, .keep_all = TRUE)  %>% 
  dplyr::select(indic_id, indicators_harmonized, km_gbf)

rm(km_gbf)


## Indicators from tables and appendices in SDGs----

sdg = read_sheet("https://docs.google.com/spreadsheets/d/158nfsL_rY-J_S5INcI5ilOCHG4GPDg1pzkwJ6-ngBFs/edit#gid=783146313",
                  sheet = "sdg_indicators")

indic_sdg = sdg %>% 
  # harmonize indicators
  dplyr::mutate(indicators_h = gsub('  ', '',indicators)) %>% 
  dplyr::mutate(indicators_h = tolower(indicators_h)) %>%
  dplyr::mutate(indicators_h = str_trim(indicators_h)) %>% 
  harmonize_indic(indicators_h) %>% 
  dplyr::rename(indicators_harmonized = field_harm) %>% 
  dplyr::mutate(indicators_harmonized = str_trim(indicators_harmonized)) %>% 
  dplyr::select(-indicators_h) %>% 
  # Remove 11c which is in development
  dplyr::filter(!is.na(indicators_harmonized)) %>% 
  # save input data
  write_csv(paste0(git_dir,'input/tables_extraction/sdg_indicators.csv')) %>% 
  write_csv(paste0(gd_dir,'input_data2/tables_extraction/sdg_indicators.csv')) %>% 
  # add source
  dplyr::mutate(sdg = 1) %>% 
  dplyr::distinct(indicators_harmonized, .keep_all = TRUE)  %>% 
  dplyr::select(indic_id, indicators_harmonized, sdg)

rm(sdg)

## Indicators from tables and appendices in CITES----

cites = read_sheet("https://docs.google.com/spreadsheets/d/1oWNLObbGYA67zF6wlBK_bCNRU-ODJuw3UDGyLXCRjlQ/edit",
                  sheet = "indicators")

indic_cites = cites %>% 
  # harmonize indicators
  dplyr::mutate(indicators_h = tolower(indicators)) %>%
  dplyr::mutate(indicators_h = str_trim(indicators_h)) %>% 
  harmonize_indic(indicators_h) %>% 
  dplyr::rename(indicators_harmonized = field_harm) %>% 
  dplyr::mutate(indicators_harmonized = str_trim(indicators_harmonized)) %>% 
  dplyr::select(-indicators_h) %>% 
  # save input data
  write_csv(paste0(git_dir,'input/tables_extraction/cites_indicators.csv')) %>% 
  write_csv(paste0(gd_dir,'input_data2/tables_extraction/cites_indicators.csv')) %>% 
  # add source
  dplyr::mutate(cites = 1) %>% 
  dplyr::distinct(indicators_harmonized, .keep_all = TRUE)  %>% 
  dplyr::select(indic_id, indicators_harmonized, cites)

rm(cites)

## Indicators from tables and appendices in RAMSAR----

ramsar = read_sheet("https://docs.google.com/spreadsheets/d/1Ho5Qne7hZcnOLCCmZSY4bMtNTTWwr6j4kM1n5qAW4bI/edit#gid=1799137558",
                   sheet = "indicators")

indic_ramsar = ramsar %>% 
  # harmonize indicators
  dplyr::mutate(indicators_h = tolower(indicators)) %>%
  dplyr::mutate(indicators_h = str_trim(indicators_h)) %>% 
  harmonize_indic(indicators_h) %>% 
  dplyr::rename(indicators_harmonized = field_harm) %>% 
  dplyr::mutate(indicators_harmonized = str_trim(indicators_harmonized)) %>% 
  dplyr::select(-indicators_h) %>% 
  # save input data
  write_csv(paste0(git_dir,'input/tables_extraction/ramsar_indicators.csv')) %>% 
  write_csv(paste0(gd_dir,'input_data2/tables_extraction/ramsar_indicators.csv')) %>% 
  # add source
  dplyr::mutate(ramsar = 1) %>% 
  dplyr::distinct(indicators_harmonized, .keep_all = TRUE)  %>% 
  dplyr::select(indic_id, indicators_harmonized, ramsar)

rm(ramsar)

## Indicators from tables and appendices in UNCCD----

unccd = read_sheet("https://docs.google.com/spreadsheets/d/1fQGjfetFYbuK5yNrNZqD8VGJTj_1JDGkh4Jqh00LzHc/edit#gid=1276622018",
                    sheet = "indicators")

indic_unccd = unccd %>% 
  # harmonize indicators
  dplyr::mutate(indicators_h = tolower(indicators)) %>%
  dplyr::mutate(indicators_h = str_trim(indicators_h)) %>% 
  harmonize_indic(indicators_h) %>% 
  dplyr::rename(indicators_harmonized = field_harm) %>% 
  dplyr::mutate(indicators_harmonized = str_trim(indicators_harmonized)) %>% 
  dplyr::select(-indicators_h) %>% 
  # save input data
  write_csv(paste0(git_dir,'input/tables_extraction/unccd_indicators.csv')) %>% 
  write_csv(paste0(gd_dir,'input_data2/tables_extraction/unccd_indicators.csv')) %>% 
  # add source
  dplyr::mutate(unccd = 1) %>% 
  dplyr::distinct(indicators_harmonized, .keep_all = TRUE)  %>% 
  dplyr::select(indic_id, indicators_harmonized, unccd)

rm(unccd)

## Indicators from tables and appendices in CMS----

cms = read_sheet("https://docs.google.com/spreadsheets/d/18QoqherdfukB_URmqKVfkAaijdkHL4fLD6bYKKz-6xQ/edit#gid=0",
                   sheet = "indicators")

indic_cms = cms %>% 
  #rename
  dplyr::rename(indicators = indicators_matched) %>% 
  # harmonize indicators
  dplyr::mutate(indicators_h = tolower(indicators)) %>%
  dplyr::mutate(indicators_h = str_trim(indicators_h)) %>% 
  harmonize_indic(indicators_h) %>% 
  dplyr::rename(indicators_harmonized = field_harm) %>% 
  dplyr::mutate(indicators_harmonized = str_trim(indicators_harmonized)) %>% 
  dplyr::select(-indicators_h) %>% 
  # save input data
  write_csv(paste0(git_dir,'input/tables_extraction/cms_indicators.csv')) %>% 
  write_csv(paste0(gd_dir,'input_data2/tables_extraction/cms_indicators.csv')) %>% 
  # add source
  dplyr::mutate(cms = 1) %>% 
  dplyr::distinct(indicators_harmonized, .keep_all = TRUE)  %>% 
  dplyr::select(indic_id, indicators_harmonized, cms)

rm(cms)

## Indicators from tables and appendices in ICCWC----

iccwc = read_sheet("https://docs.google.com/spreadsheets/d/1hkUinifnfcBVEPjuigT1pStmvWCVoarcAeU5T-mS6H8/edit#gid=0",
                 sheet = "indicators")

indic_iccwc = iccwc %>% 
  # harmonize indicators
  dplyr::mutate(indicators_h = tolower(indicators)) %>%
  dplyr::mutate(indicators_h = str_trim(indicators_h)) %>% 
  harmonize_indic(indicators_h) %>% 
  dplyr::rename(indicators_harmonized = field_harm) %>% 
  dplyr::mutate(indicators_harmonized = str_trim(indicators_harmonized)) %>% 
  dplyr::select(-indicators_h) %>% 
  # save input data
  write_csv(paste0(git_dir,'input/tables_extraction/iccwc_indicators.csv')) %>% 
  write_csv(paste0(gd_dir,'input_data2/tables_extraction/iccwc_indicators.csv')) %>% 
  # add source
  dplyr::mutate(iccwc = 1) %>% 
  dplyr::distinct(indicators_harmonized, .keep_all = TRUE)  %>% 
  dplyr::select(indic_id, indicators_harmonized, iccwc)

rm(iccwc)

## Append all policy indicators----

indic_km_gbf = read_csv(paste0(git_dir,'input/tables_extraction/km_gbf_indicators.csv')) %>% 
  # add source
  dplyr::mutate(km_gbf = 1) %>% 
  dplyr::distinct(indicators_harmonized, .keep_all = TRUE)  %>% 
  dplyr::select(indicators_harmonized, km_gbf)

indic_sdg = read_csv(paste0(git_dir,'input/tables_extraction/sdg_indicators.csv')) %>% 
  # add source
  dplyr::mutate(sdg = 1) %>% 
  dplyr::distinct(indicators_harmonized, .keep_all = TRUE)  %>% 
  dplyr::select(indicators_harmonized, sdg)

indic_cites = read_csv(paste0(git_dir,'input/tables_extraction/cites_indicators.csv')) %>% 
  # add source
  dplyr::mutate(cites = 1) %>% 
  dplyr::distinct(indicators_harmonized, .keep_all = TRUE)  %>% 
  dplyr::select(indicators_harmonized, cites)

indic_ramsar = read_csv(paste0(git_dir,'input/tables_extraction/ramsar_indicators.csv')) %>% 
  # add source
  dplyr::mutate(ramsar = 1) %>% 
  dplyr::distinct(indicators_harmonized, .keep_all = TRUE)  %>% 
  dplyr::select(indicators_harmonized, ramsar)

indic_unccd = read_csv(paste0(git_dir,'input/tables_extraction/unccd_indicators.csv')) %>% 
  # add source
  dplyr::mutate(unccd = 1) %>% 
  dplyr::distinct(indicators_harmonized, .keep_all = TRUE)  %>% 
  dplyr::select(indicators_harmonized, unccd)

indic_cms = read_csv(paste0(git_dir,'input/tables_extraction/cms_indicators.csv')) %>% 
  # add source
  dplyr::mutate(cms = 1) %>% 
  dplyr::distinct(indicators_harmonized, .keep_all = TRUE)  %>% 
  dplyr::select(indicators_harmonized, cms)

indic_iccwc = read_csv(paste0(git_dir,'input/tables_extraction/iccwc_indicators.csv')) %>% 
  # add source
  dplyr::mutate(iccwc = 1) %>% 
  dplyr::distinct(indicators_harmonized, .keep_all = TRUE)  %>% 
  dplyr::select(indicators_harmonized, iccwc)

# Join
indic_policy = indic_km_gbf %>% 
  # join indicators
  full_join(indic_sdg, by = 'indicators_harmonized') %>% 
  full_join(indic_cites, by = 'indicators_harmonized') %>% 
  full_join(indic_cms, by = 'indicators_harmonized') %>% 
  full_join(indic_iccwc, by = 'indicators_harmonized') %>% 
  full_join(indic_ramsar, by = 'indicators_harmonized') %>% 
  full_join(indic_unccd, by = 'indicators_harmonized')

# checks joined indicators
dup = check_dup(indic_policy, indicators_harmonized) # no exact duplicates

indic_policy = indic_policy %>% 
  # harmonize indicators
  dplyr::mutate(indicators_h = tolower(indicators_harmonized)) %>%
  dplyr::mutate(indicators_h = str_trim(indicators_h)) %>% 
  harmonize_indic(indicators_h)
dup = check_dup(indic_policy, indicators_h) # no exact duplicates

# save
indic_policy = indic_policy %>% 
  # clean table
  dplyr::select(-indicators_h, -field_harm) %>%
  # save data
  write_csv(paste0(git_dir,'input/tables_extraction/policy_indicators.csv')) %>% 
  write_csv(paste0(gd_dir,'input_data2/tables_extraction/policy_indicators.csv'))



## Append indicators from supplementary material (tables) and conventions (policy)------

indic_policy = read_csv(paste0(git_dir,'input/tables_extraction/policy_indicators.csv'))
indic_supp_tables = read_csv(paste0(git_dir,'input/tables_extraction/IPBES-IPCC_supp_tables_indicators.csv'))

# Check independent files
dup = check_dup(indic_policy, indicators_harmonized) # no exact duplicates
dup = check_dup(indic_supp_tables, indicators_harmonized) # no exact duplicates
rm(dup)

# Join
indicators_policy_supp_tables = indic_policy %>% 
  full_join(indic_supp_tables, by = 'indicators_harmonized') 

# checks joined indicators
dup = check_dup(indicators_policy_supp_tables, indicators_harmonized) # no exact duplicates

indicators_policy_supp_tables = indicators_policy_supp_tables %>% 
  # harmonize indicators
  dplyr::mutate(indicators_h = tolower(indicators_harmonized)) %>%
  dplyr::mutate(indicators_h = str_trim(indicators_h)) %>% 
  harmonize_indic(indicators_h)
dup = check_dup(indicators_policy_supp_tables, indicators_h) # no exact duplicates

# save
indicators_policy_supp_tables = indicators_policy_supp_tables %>% 
  # clean table
  dplyr::select(-indicators_h, -field_harm) %>%
  # add indicators field
  dplyr::mutate(indic = 1) %>%
  # save data
  write_csv(paste0(git_dir,'input/tables_extraction/policy_supp_tables_indicators.csv')) %>% 
  write_csv(paste0(gd_dir,'input_data2/tables_extraction/policy_supp_tables_indicators.csv'))



###END###




  

# ### IPBES core and highlighted indicators (D&K TF 2017-2018)-----
# core_high = read_excel('input_data/IPBES_2018.xlsx', sheet = 'core-high') %>%
#   mutate(indicators = tolower(`Specific Indicator`)) %>% 
#   #trims trailing commas
#   mutate(indicators = trimws(indicators)) %>% 
#   mutate(ga = if_else(!is.na(`GA Chapter`),
#                              true = 1,
#                              false = 0 )) %>% 
#   mutate(ra = if_else(!is.na(`RA Chapter`),
#                       true = 1,
#                       false = 0 )) %>%  
#   mutate(bip = if_else(!is.na(`BIP`),
#                       true = 1,
#                       false = 0 )) %>%
#   mutate(CF = paste0(CF1,',', CF2,',',CF3)) %>% 
#   mutate(source='IPBES_indicators_2018') %>% 
#   dplyr::select(indicator_type = `type of indicator`,indicators,DPSIR ="DPSIR",CF, ra,ga,bip,source)
# 
# core_high %>%  group_by(indicator_type) %>% distinct() %>% count()
# # core              30
# # highlighted       42
# 
# ## These indicators were manually classified into categories for further work
# ipbes2018 = read_excel('input_data/IPBES_2018.xlsx', sheet = 'core-high-classified')

## All indicators summaries----

all_indicators %>% count()#1586
all_indicators %>% distinct(indicators_harmonized) %>% count()
all_indicators %>% filter(ga==1) %>% count()#687
all_indicators %>% filter(sua==1) %>% count()#199
all_indicators %>% filter(va ==1) %>% count()#18
all_indicators %>% filter(ipbes==1) %>% count()#885
all_indicators %>% filter(geo==1) %>% count()#149
all_indicators %>% filter(ipcc ==1) %>% count()#252
all_indicators %>% filter(km_gbf ==1) %>% count()#299
all_indicators %>% filter(sdg==1) %>% count()#234


ind_cleaned = all_indicators %>%
  # remove indicators that did not passed the first assessment
  filter(!is.na(indicators_harmonized)) %>%
  #mutate(ind_usage=IPBES+GBF+SDG+BIP) %>% 
  #mutate(ipbes_ind_usage=ga2+va+sua) %>% 
  group_by(indicators_harmonized) %>% 
  mutate(across(c(ga:sdg), sum, .names = "{.col}", na.rm = TRUE)) %>%
  #mutate(across(c(gbf_clean:ipbes_clean), ~ replace(.,.>=1,1.))) %>%
  ungroup() %>%
  mutate(ipbes_usage = ga+va+sua) %>% 
  mutate(usage = km_gbf+sdg+ipbes+ipcc+geo)


ind_cleaned2 = ind_cleaned %>% 
  mutate(ipbes = gsub(1, 'ipbes', ipbes)) %>% 
  mutate(ipcc = gsub(1, 'ipcc', ipcc)) %>% 
  mutate(geo = gsub(1, 'geo', geo)) %>% 
  mutate(km_gbf = gsub(1, 'ipbes', km_gbf)) %>% 
  mutate(sdg = gsub(1, 'sdg', sdg)) %>% 
  pivot_longer(
    cols = `geo`:`ipbes`, 
    names_to = "assessment",
    values_to = "value") %>% 
  filter(value != 0) %>% 
  group_by(indicators_harmonized) %>% 
  mutate(assess = paste0(assessment,collapse = ",")) %>% 
  ungroup() %>% 
  dplyr::select(-n_indic, -ga, -va, -sua, -assessment, -value) %>% 
  group_by(assess) %>% 
  count()

### Categorize indicators-----
# This process is done manually following the categories described in Table 1 and 2

## Load previous classified indicators
all_indicators_classified=read_sheet('https://docs.google.com/spreadsheets/d/1RamCu_44EgRZJCI6kEbQJ7gDk-uV9eYw8H_tME140lw/edit#gid=308874240',
                                     sheet = 'tables_extraction_indicators') %>% 
  dplyr::select(-findings, -`Categories 2`, -`Subcategories 2`) %>% 
  dplyr::filter(!is.na(Categories)) %>% 
  dplyr::mutate(ipbes = if_else(ga == 1 | sua == 1,
                               true = 1,
                               false = NA)) %>% 
  dplyr::relocate(ipbes, .after=sdg)
write_csv(all_indicators_classified, 'C:/Users/yanis/Documents/all_indicators_classified.csv')
# relation digaram with https://app.rawgraphs.io/

all_indicators_classified_cleaned %>% 


indicators_generalized = distinct(all_indicators_classified,indicators_generalized)
indicators_categories = distinct(all_indicators_classified,indicators_categories)
indicators_categories2 = distinct(all_indicators_classified,indicators_categories2)

all_indicators_classified %>% count()#1690 -->1739
all_indicators_classified %>% distinct(indicators) %>% count()#1690 --> 1689
 
all_indicators_classified %>% filter(gbf==1) %>% count()#298 --> 314 (unique are the same)
all_indicators_classified %>% filter(sdg==1) %>% count()#307 --> 314 (unique are the same)
all_indicators_classified %>% filter(ipbes ==1) %>% count()#1309 --> 1340 (unique are the same)
# this may be due to a correct disaggregation of indicators or because they matched to multiple categories (which is a problem that I will need to investigate later)

# check/Fix odd characters
odd_ind = check_odd_chr(all_indicators_classified, indicators) # all good, some odd spaces
odd_ind_clean = check_odd_chr(all_indicators_classified, indicators_cleaned)# all good, some odd spaces


### Summaries----

## Cleaned indicators
ind_cleaned = all_indicators_classified %>%
  # remove indicators that did not passed the first assessment
  filter(!is.na(indicators_cleaned)) %>%
  #mutate(ind_usage=IPBES+GBF+SDG+BIP) %>% 
  #mutate(ipbes_ind_usage=ga2+va+sua) %>% 
  group_by(indicators_cleaned) %>% 
  mutate(across(c(gbf:ipbes), sum, .names = "{.col}_clean", na.rm = TRUE)) %>%
  mutate(across(c(gbf_clean:ipbes_clean), ~ replace(.,.>=1,1.))) %>%
  ungroup() %>%
  mutate(ipbes_usage = ga_clean+va_clean+sua_clean) %>% 
  mutate(usage = gbf_clean+sdg_clean+ipbes_clean) %>%
  dplyr::select("indicators_cleaned","indicators_generalized","indicators_categories"="indicators_categories2",
                "gbf_clean","ga_clean","va_clean","sua_clean","sdg_clean","ipbes_clean","ipbes_usage","usage") %>%
  distinct() 

ind_cleaned %>% filter(ipbes_clean == 1) %>% distinct() %>% count() #1007
ind_cleaned %>% filter(ga_clean == 1) %>% distinct() %>% count() #667
ind_cleaned %>% filter(va_clean == 1) %>% distinct() %>% count() #88
ind_cleaned %>% filter(sua_clean == 1) %>% distinct() %>% count() #340
ind_cleaned %>% filter(ipbes_clean == 1) %>% distinct(indicators_categories)

ind_cleaned %>% filter(ipbes_clean == 1) %>% distinct() %>% count(ga_clean, va_clean, sua_clean)
ind_cleaned %>% filter(ipbes_clean == 1, ipbes_usage >= 3) %>% distinct(indicators_cleaned)

## Generalized  indicators
ind_gen = ind_cleaned %>% 
  group_by(indicators_generalized) %>% 
  mutate(across(c(gbf_clean:ipbes_clean), sum, .names = "{.col}_gen", na.rm = TRUE)) %>% 
  mutate(across(c(gbf_clean_gen:ipbes_clean_gen), ~ replace(.,.>=1,1.))) %>%
  ungroup() %>% 
  mutate(ipbes_usage = ga_clean_gen+va_clean_gen+sua_clean_gen) %>%
  dplyr::select("indicators_generalized","indicators_categories",
                "gbf_clean_gen","ga_clean_gen","va_clean_gen","sua_clean_gen","sdg_clean_gen","ipbes_clean_gen","ipbes_usage") %>%
  distinct() 

ind_gen %>% filter(ipbes_clean_gen == 1) %>% distinct() %>% count() #51
ind_gen %>% filter(ga_clean_gen == 1) %>% distinct() %>% count() #50
ind_gen %>% filter(va_clean_gen == 1) %>% distinct() %>% count() #30
ind_gen %>% filter(sua_clean_gen == 1) %>% distinct() %>% count() #49
ind_gen %>% filter(ipbes_clean_gen == 1) %>% distinct(indicators_categories)

ind_gen %>% filter(ipbes_clean_gen == 1) %>% distinct() %>% count(ga_clean_gen, va_clean_gen, sua_clean_gen)
ind_gen %>% filter(ipbes_clean_gen == 1, ipbes_usage >= 3) %>% distinct(indicators_generalized)

## Categorized indicators
ind_cat = ind_gen %>% 
  group_by(indicators_categories) %>% 
  mutate(across(c(gbf_clean_gen:ipbes_clean_gen), sum, .names = "{.col}_cat", na.rm = TRUE)) %>% 
  mutate(across(c(gbf_clean_gen_cat:ipbes_clean_gen_cat), ~ replace(.,.>=1,1.))) %>%
  ungroup() %>% 
  mutate(ipbes_usage = ga_clean_gen_cat+va_clean_gen_cat+sua_clean_gen_cat) %>%
  dplyr::select("indicators_categories",
                "gbf_clean_gen_cat","ga_clean_gen_cat","va_clean_gen_cat","sua_clean_gen_cat","sdg_clean_gen_cat","ipbes_clean_gen_cat","ipbes_usage") %>%
  distinct() 

ind_cat %>% filter(ipbes_clean_gen_cat == 1) %>% distinct() %>% count() #9
ind_cat %>% filter(ga_clean_gen_cat == 1) %>% distinct() %>% count() 
ind_cat %>% filter(va_clean_gen_cat == 1) %>% distinct() %>% count() 
ind_cat %>% filter(sua_clean_gen_cat == 1) %>% distinct() %>% count() 
ind_cat %>% filter(ipbes_clean_gen_cat == 1) %>% distinct(indicators_categories)

ind_cat %>% filter(ipbes_clean_gen_cat == 1) %>% distinct() %>% count(ga_clean_gen_cat, va_clean_gen_cat, sua_clean_gen_cat)
ind_cat %>% filter(ipbes_clean_gen_cat == 1, ipbes_usage >= 3) %>% distinct(indicators_categories)  


# IPBES assessment comparison (Table 3)
ind_cleaned %>% filter(ipbes_clean == 1) %>% group_by(indicators_categories) %>% count() %>% arrange(desc(n))
ind_cleaned %>% filter(ga_clean==1) %>% group_by(indicators_categories) %>% count() %>% arrange(desc(n))
ind_cleaned %>% filter(va_clean==1) %>% group_by(indicators_categories) %>% count() %>% arrange(desc(n))
ind_cleaned %>% filter(sua_clean==1) %>% group_by(indicators_categories) %>% count() %>% arrange(desc(n))

# Frameworks comparison (Table 4)
ind_cleaned %>% filter(ipbes_clean == 1) %>% group_by(indicators_categories) %>% count() %>% arrange(desc(n))
ind_cleaned %>% filter(gbf_clean==1) %>% group_by(indicators_categories) %>% count() %>% arrange(desc(n))
ind_cleaned %>% filter(sdg_clean==1) %>% group_by(indicators_categories) %>% count() %>% arrange(desc(n))

ind_cleaned %>% filter(usage >= 3) %>% distinct(indicators_cleaned) %>% count() # 63 indicators used in the 3 frameworks
ind_cleaned %>% filter(usage >= 3) %>% distinct(indicators_cleaned, indicators_categories) %>% view()

# Comparison of sub categories (Level 2)
## nature
nature = ind_cleaned %>% 
  filter(indicators_categories %in% c('biodiversity','ecosystems')) %>% 
  group_by(indicators_generalized) %>% 
  mutate(across(c(gbf_clean,sua_clean, ipbes_clean), sum, .names = "{.col}_gen", na.rm = TRUE)) %>%
  ungroup() %>% 
  dplyr::select(indicators_generalized, ipbes_clean_gen,gbf_clean_gen,sua_clean_gen) %>% distinct() %>% arrange(desc(ipbes_clean_gen))
write_csv(nature,'input_data/nature.csv' )

## NCP
ncp = ind_cleaned %>% 
  filter(indicators_categories == 'ecosystem services') %>% 
  group_by(indicators_generalized) %>% 
  mutate(across(c(gbf_clean,sua_clean, ipbes_clean), sum, .names = "{.col}_gen", na.rm = TRUE)) %>%
  ungroup() %>% 
  dplyr::select(indicators_generalized, ipbes_clean_gen,gbf_clean_gen,sua_clean_gen) %>% distinct() %>% arrange(desc(ipbes_clean_gen))
write_csv(ncp,'input_data/ncp.csv' )

## DD
dd = ind_cleaned %>% 
  filter(indicators_categories == 'direct drivers') %>% 
  group_by(indicators_generalized) %>% 
  mutate(across(c(gbf_clean,sua_clean, ipbes_clean), sum, .names = "{.col}_gen", na.rm = TRUE)) %>%
  ungroup() %>% 
  dplyr::select(indicators_generalized, ipbes_clean_gen,gbf_clean_gen,sua_clean_gen) %>% distinct() %>% arrange(desc(ipbes_clean_gen))
write_csv(dd,'input_data/dd.csv' )

## Gov
gov = ind_cleaned %>% 
  filter(indicators_categories == 'governance') %>% 
  group_by(indicators_generalized) %>% 
  mutate(across(c(gbf_clean,sua_clean, ipbes_clean), sum, .names = "{.col}_gen", na.rm = TRUE)) %>%
  ungroup() %>% 
  dplyr::select(indicators_generalized, ipbes_clean_gen,gbf_clean_gen,sua_clean_gen) %>% distinct() %>% arrange(desc(ipbes_clean_gen))
write_csv(gov,'input_data/gov.csv' )

## Wellbeing
wellness = ind_cleaned %>% 
  filter(indicators_categories == 'wellness') %>% 
  group_by(indicators_generalized) %>% 
  mutate(across(c(gbf_clean,sua_clean, ipbes_clean), sum, .names = "{.col}_gen", na.rm = TRUE)) %>%
  ungroup() %>% 
  dplyr::select(indicators_generalized, ipbes_clean_gen,gbf_clean_gen,sua_clean_gen) %>% distinct() %>% arrange(desc(ipbes_clean_gen))
write_csv(wellness,'input_data/wellness.csv' )

## Anthropogenic assets
aa = ind_cleaned %>% 
  filter(indicators_categories %in% c('ILK', 'scientific knowledge', 'financial assets')) %>% 
  group_by(indicators_generalized) %>% 
  mutate(across(c(gbf_clean,sua_clean, ipbes_clean), sum, .names = "{.col}_gen", na.rm = TRUE)) %>%
  ungroup() %>% 
  dplyr::select(indicators_generalized, ipbes_clean_gen,gbf_clean_gen,sua_clean_gen) %>% distinct() %>% arrange(desc(ipbes_clean_gen))
write_csv(aa,'input_data/aa.csv' )


### Assess uptake of IPBES Core and Highlighted indicatros-----

# against al frameworks
ipbes_2018_not_used_all = ipbes2018 %>%
  anti_join(all_indicators_classified, by=('indicators_cleaned')) %>% 
  dplyr::left_join(select(core_high,indicators, indicator_type), by='indicators') %>% 
  distinct()

# against IPBES
ipbes_2018_not_used = ipbes2018 %>%
  anti_join(filter(all_indicators_classified, ipbes == 1), by=('indicators_cleaned')) %>% 
  dplyr::left_join(select(core_high,indicators, indicator_type), by='indicators') %>% 
  distinct()




