# clean you environment
rm(list=ls())

##################################################################################
# Extract indicators from tables and supp material from global assessments and MEA
##################################################################################
# Created by Yanina Sica in January 2023
# Updated May 2024

### Settings----

## Source useful functions from folders downloaded from GitHub
your_workdir <- "C:/Users/yanis/Documents/scripts/IPBES-Data/IPBES_Data_Indicators/"
source(paste0(your_workdir,"R/useful_functions_indic.R"))
source(paste0(your_workdir,"R/settings.R"))

## Set working directory
your_user <- Sys.info()["user"]
your_node <- Sys.info()["nodename"]

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
git_dir = "C:/Users/yanis/Documents/scripts/IPBES-Data/IPBES_Data_Indicators/"

# Load indicators from supplementary material (tables)----
## Indicators from tables and appendices in IPBES----

# SUA
ipbes_sua = readxl::read_excel(paste0(git_dir, "input/tables_extraction/IPBES/IPBES_suppMat_indicators.xlsx"),
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
ipbes_ga21 = readxl::read_excel(paste0(git_dir, "input/tables_extraction/IPBES/IPBES_suppMat_indicators.xlsx"),
                       sheet = "ga_suppMat_ch2.1_drivers")

indic_ga21 = ipbes_ga21 %>% 
  dplyr::rename(indicators = Indicator) %>% 
  # clean indicators
  dplyr::mutate(indicators_h = tolower(indicators)) %>%
  dplyr::mutate(indicators_h = str_trim(indicators_h)) %>% 
  dplyr::distinct(indicators_h, .keep_all = TRUE)  %>% 
  dplyr::select(indic_id, indicators,indicators_h)


ipbes_ga22 = readxl::read_excel(paste0(git_dir, "input/tables_extraction/IPBES/IPBES_suppMat_indicators.xlsx"),
                        sheet = "ga_suppMat_ch2.2_nature")

indic_ga22 = ipbes_ga22 %>% 
  dplyr::rename(indicators = `Indicator (matched)`) %>% 
  # clean indicators
  dplyr::mutate(indicators_h = tolower(indicators)) %>%
  dplyr::mutate(indicators_h = str_trim(indicators_h)) %>% 
  dplyr::distinct(indicators_h, .keep_all = TRUE)  %>% 
  dplyr::select(indic_id, indicators,indicators_h)


ipbes_ga23 = readxl::read_excel(paste0(git_dir, "input/tables_extraction/IPBES/IPBES_suppMat_indicators.xlsx"),
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

ipcc = readxl::read_excel(paste0(git_dir, "input/tables_extraction/IPCC/AR6_WG1.xlsx"),
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

km_gbf = readxl::read_excel(paste0(git_dir, "input/tables_extraction/GBF/Kumming_Montreal_GBF_Indicators_20230821.xlsx"),
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

sdg = readxl::read_excel(paste0(git_dir, "input/tables_extraction/SDGs/SDG_indicators.xlsx"),
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

cites = readxl::read_excel(paste0(git_dir, "input/tables_extraction/CITES/cites_indicators.xlsx"),
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

ramsar = readxl::read_excel(paste0(git_dir, "input/tables_extraction/RAMSAR/ramsar.xlsx"),
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

unccd = readxl::read_excel(paste0(git_dir, "input/tables_extraction/UNCCD/UNCCD.xlsx"),
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

cms = readxl::read_excel(paste0(git_dir, "input/tables_extraction/CMS/CMS.xlsx"),
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

iccwc = readxl::read_excel(paste0(git_dir, "input/tables_extraction/ICCWC/ICCWC.xlsx"),
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


### IPBES core and highlighted indicators (D&K TF 2017-2018)-----
core_high = readxl::read_excel(paste0(git_dir, "input/tables_extraction/IPBES/IPBES_2018.xlsx"),
                       sheet = "core-high")

core_high %>%  group_by(`type of indicator`) %>% distinct() %>% count()
# core              30
# highlighted       42

indic_core_high = core_high %>% 
  mutate(indicators = tolower(`Specific Indicator (matched)`)) %>% 
  mutate(type_indicators = tolower(`type of indicator`)) %>% 
  # harmonize indicators
  dplyr::mutate(indicators_h = tolower(indicators)) %>%
  dplyr::mutate(indicators_h = str_trim(indicators_h)) %>% 
  harmonize_indic(indicators_h) %>% 
  dplyr::rename(indicators_harmonized = field_harm) %>% 
  dplyr::mutate(indicators_harmonized = str_trim(indicators_harmonized)) %>% 
  dplyr::select(-indicators_h) %>% 
  # save input data
  write_csv(paste0(git_dir,'input/tables_extraction/ipbes_core_high_indicators.csv')) %>% 
  write_csv(paste0(gd_dir,'input_data2/tables_extraction/ipbes_core_high_indicators.csv')) %>% 
  # add source
  dplyr::mutate(core_high = 1) %>% 
  dplyr::distinct(indicators_harmonized, .keep_all = TRUE)  %>% 
  dplyr::select(indic_id, indicators_harmonized, core_high, type_indicators)

indic_core_high %>%  distinct(indicators_harmonized) %>% count() # 72



#################################### END





%>%
  #trims trailing commas
  mutate(indicators = trimws(indicators)) %>%
  mutate(ga = if_else(!is.na(`GA Chapter`),
                             true = 1,
                             false = 0 )) %>%
  mutate(ra = if_else(!is.na(`RA Chapter`),
                      true = 1,
                      false = 0 )) %>%
  mutate(bip = if_else(!is.na(`BIP`),
                      true = 1,
                      false = 0 )) %>%
  mutate(CF = paste0(CF1,',', CF2,',',CF3)) %>%
  mutate(source='IPBES_indicators_2018') %>%
  dplyr::select(indicator_type = `type of indicator`,indicators,DPSIR ="DPSIR",CF, ra,ga,bip,source)

core_high %>%  group_by(indicator_type) %>% distinct() %>% count()
# core              30
# highlighted       42

## These indicators were manually classified into categories for further work
ipbes2018 = read_excel('input_data/IPBES_2018.xlsx', sheet = 'core-high-classified')

