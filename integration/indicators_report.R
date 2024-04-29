rm(list=ls())

#### INDICATORS USAGE----
# Code to combine indicators
# 
# Created by Yanina Sica
# January 2023
# Updated Sep 2023

### Settings----

## Packages

#library(data.table)
library(tidyverse)
library(dplyr)

library(stringr)
library(readxl)
library(readr)

#install.packages("googlesheets4")
library(googlesheets4)

## Functions

##### Check for duplicates
check_dup <- function(file_name,field){
  dup_df = file_name %>% 
    dplyr::mutate(dup1 = duplicated({{field}}, fromLast = TRUE)) %>%  
    dplyr::mutate(dup2 = duplicated({{field}}, fromLast = FALSE)) %>% 
    dplyr::mutate(dup = ifelse(dup1 == TRUE,
                               yes=dup1,
                               no=dup2)) #%>% 
  #   dplyr::filter(dup == TRUE)   
  # 
  # if (nrow(dup_df) > 0){
  #   print("Check duplicates")
  # }else{
  #   print("No duplicates")
  # }
  # return(dup_df)
}

##### Check character in other fields (e.g. authorship where there is more flexibility-------
check_odd_chr <- function(file_name = file_name,
                                 field = field){
  regexp1 <- "([^a-zA-Z\u00C0-\u024F\u1E00-\u1EFF])" #using extended latin characters
  odd_chr = file_name %>%
    dplyr::mutate(field1 = gsub('\\(', '',{{field}})) %>%
    dplyr::mutate(field1 = gsub('\\)', '',field1)) %>%
    dplyr::mutate(field1 = gsub('\\[', '',field1)) %>%
    dplyr::mutate(field1 = gsub('\\]', '',field1)) %>%
    dplyr::mutate(field1 = gsub('\\.', '',field1)) %>%
    dplyr::mutate(field1 = gsub('\\;', '',field1)) %>%
    dplyr::mutate(field1 = gsub('\\:', '',field1)) %>%
    dplyr::mutate(field1 = gsub('\\,', '',field1)) %>%
    dplyr::mutate(field1 = gsub('\\&', '',field1)) %>%
    dplyr::mutate(field1 = gsub('\\–', '',field1)) %>%
    dplyr::mutate(field1 = gsub('\\/', '',field1)) %>%
    dplyr::mutate(field1 = gsub('\\%', '',field1)) %>%
    dplyr::mutate(field1 = gsub('\\<', '',field1)) %>%
    dplyr::mutate(field1 = gsub('\\>', '',field1)) %>%
    dplyr::mutate(field1 = gsub('\\+', '',field1)) %>%
    dplyr::mutate(field1 = gsub('\\‑', '',field1)) %>%
    dplyr::mutate(field1 = gsub(' ', '',field1)) %>%
    dplyr::mutate(field1 = gsub('  ', '',field1)) %>%
    dplyr::mutate(field1 = gsub('[0-9]', '',field1)) %>%
    dplyr::mutate(field1 = gsub('-', '',field1)) %>%
    dplyr::mutate(field1 = gsub("'", "",field1)) %>%
    dplyr::mutate(field1 = gsub("’", "",field1)) %>%
    dplyr::mutate(field1 = gsub("’", "",field1)) %>%
    dplyr::mutate(bad_chr = grepl(pattern = regexp1, x = field1))  %>%
    dplyr::mutate(field1 = gsub(' ', '',field1)) %>%
    #dplyr::mutate(bad_chr = grepl("[^-'`’;,.äàáâaÁÄåãëèéêÉïíîÍöóòôõøÓØÖüùúûÜÚß&ñÑçÇssSSýYa-zA-Z 0-9]",field1))  %>%
    filter(bad_chr == TRUE)
   
  if (nrow(odd_chr) > 0){
    cat('Check characters')
  }else{
    cat('No odd characters')
  }
  return(odd_chr)
}

##### Harmonize indicators-------
harmonize_indic <- function(file_name = file_name,
                          field = field){
  #regexp1 <- "([^a-zA-Z\u00C0-\u024F\u1E00-\u1EFF])" #using extended latin characters

  harmonized = file_name %>%
    # general
    dplyr::mutate(field_harm = gsub('&', 'and',{{field}})) %>%
    dplyr::mutate(field_harm = gsub('†', '',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('–', '-',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('gross domestic product (gdp)', 'gross domestic product',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('gdp', ' gross domestic product',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('gross value added (gva)',  'gross value added',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('gva ', 'gross value added ',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('mti (marine trophic index)', 'marine trophic index',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('marine trophic index (mti)', 'marine trophic index',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('mti ', 'marine trophic index ',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('no[.]', 'number',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('nmps ', 'national medicines policies ',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('net primary production (npp)', 'net primary production',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('npp ', 'net primary production ',field_harm)) %>%
    dplyr::mutate(field_harm = gsub(' (overall)', '',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('mean species abundance (msa)', 'mean species abundance',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('mean species abundance index', 'mean species abundance',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('the ramsar sites', 'ramsar sites',field_harm)) %>%
    # harmonize names
    dplyr::mutate(field_harm = gsub('area of mangrove forest cover (km2)', 'mangrove forest area',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('rli', 'red list index',field_harm)) %>% 
    dplyr::mutate(field_harm = gsub('red list index (overall)', 'red list index',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('red list index (impacts of utilization/wild relatives of domesticated animals)', 'red list index (for utilized species)',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('red list index (impacts of utilisation)', 'red list index (for utilized species)',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('red list index (species used for food and medicine)', 'red list index (for utilized species)',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('red list index (wild species used for food and medicine)', 'red list index (for utilized species)',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('red list index (wild relatives of farmed and domesticated species)', 'red list index (for utilized species)',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('rli (species used in food & medicine)', 'red list index (for utilized species)',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('red list index (forest specialist species)', 'red list index (forest specialists)',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('red list index (forest tree specialist species)', 'red list index (forest specialists)',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('rli (internationally traded birds)', 'red list index (internationally traded birds)',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('red list index (for internationally traded species and for migratory species)', 'red list index (internationally traded birds)',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('red list index (for internationally traded species)', 'red list index (internationally traded birds)',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('red list index (internationally traded wild species)', 'red list index (internationally traded birds)',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('red list index (pollinating species)', 'red list index (pollinators)',field_harm)) %>%
    dplyr::mutate(field_harm = gsub('red list index (pollinator species)', 'red list index (pollinators)',field_harm)) %>%
    # dplyr::mutate(field_harm = gsub('mti (marine trophic index)', 'marine trophic index (mti))',field_harm)) %>%
    # dplyr::mutate(field_harm = gsub('gross value added (gva)', 'gross value added',field_harm)) %>%
    # dplyr::mutate(field_harm = gsub('gva', 'gross value added',field_harm)) %>%
    
    # dplyr::mutate(field_harm = gsub('[0-9]', '',field_harm)) %>%
    # dplyr::mutate(field_harm = gsub('–', '-',field_harm)) %>%
    # dplyr::mutate(field_harm = gsub("'", "",field_harm)) %>%
    # dplyr::mutate(field_harm = gsub("’", "",field_harm)) %>%
    # dplyr::mutate(field_harm = gsub("’", "",field_harm)) %>%
  
  return(harmonized)
}
## Directories

setwd("C:/Users/yanis/Documents/scripts/IPBES_TSU-DATA/Indicators/")

local_dir = 'C:/Users/yanis/Documents/IPBES/assessments'
gd_dir = 'G:/.shortcut-targets-by-id/18yX-16J7W2Kyq4Mn3YbU_HTjslZyr4hE/IPBES Task Force Knowledge and Data/_DATA/_TSU Internal/_ Weekly reports/Files - TSU Data/indicators/'
git_dir = "C:/Users/yanis/Documents/scripts/IPBES_TSU-DATA/Indicators/"


### Load indicators----

## Indicators from tables and appendices in IPBES----

ipbes_sua = read_sheet("https://docs.google.com/spreadsheets/d/1meGdQdz7g5xeRGm7mD8NPgDv4ugTdutrdVmZTiaZgo0/edit?usp=sharing",
                   sheet = "sua_dmr_ch2")
indic_sua =  ipbes_sua %>% 
  dplyr::mutate(indicators = gsub('[0-9][0-9].[0-9][0-9].[0-9 ]', '',indicators)) %>%
  dplyr::mutate(indicators = gsub('[0-9][0-9].[0-9].[0-9 ]', '',indicators)) %>%
  dplyr::mutate(indicators = gsub('[0-9].[0-9][0-9].[0-9 ]', '',indicators)) %>%
  dplyr::mutate(indicators = gsub('[0-9].[0-9].[0-9 ]', '',indicators)) %>% 
  dplyr::mutate(indicators = gsub('[0-9][0-9].[a-z].[0-9 ]', '',indicators)) %>% 
  dplyr::mutate(indicators = gsub('[0-9].[a-z].[0-9 ]', '',indicators)) %>% 
  dplyr::mutate(indicators = gsub('  ', '',indicators)) %>% 
  dplyr::mutate(indicators = str_trim(indicators)) %>% 
  dplyr::mutate(indicators = tolower(indicators)) %>% 
  harmonize_indic(indicators) %>% 
  dplyr::rename(indicators_harmonized = field_harm) %>% 
  dplyr::distinct(indicators, indicators_harmonized, sua)
  
  
ipbes_ga21 = read_sheet("https://docs.google.com/spreadsheets/d/1meGdQdz7g5xeRGm7mD8NPgDv4ugTdutrdVmZTiaZgo0/edit?usp=sharing",
                       sheet = "ga_suppMat_ch2.1_drivers")

indic_ga21 = ipbes_ga21 %>% 
  dplyr::mutate(indicators = tolower(Indicator)) %>%
  dplyr::mutate(indicators = str_trim(indicators)) %>% 
  dplyr::mutate(ga = 1) %>% 
  dplyr::distinct(indicators, ga)


ipbes_ga22 = read_sheet("https://docs.google.com/spreadsheets/d/1meGdQdz7g5xeRGm7mD8NPgDv4ugTdutrdVmZTiaZgo0/edit?usp=sharing",
                        sheet = "ga_suppMat_ch2.2_nature")

indic_ga22 = ipbes_ga22 %>% 
  dplyr::mutate(indicators = tolower(`Indicator (matched)`)) %>%
  dplyr::mutate(indicators = str_trim(indicators)) %>% 
  dplyr::mutate(ga = 1)  %>% 
  dplyr::distinct(indicators, ga)

ipbes_ga23 = read_sheet("https://docs.google.com/spreadsheets/d/1meGdQdz7g5xeRGm7mD8NPgDv4ugTdutrdVmZTiaZgo0/edit?usp=sharing",
                        sheet = "ga_suppMat_ch2.3_ncp")

indic_ga23 = ipbes_ga23 %>% 
  dplyr::mutate(indicators = tolower(indicators)) %>%
  dplyr::mutate(indicators = str_trim(indicators)) %>% 
  dplyr::mutate(ga = 1) %>% 
  dplyr::distinct(indicators, ga) 

ipbes_ga3 = read_sheet("https://docs.google.com/spreadsheets/d/1meGdQdz7g5xeRGm7mD8NPgDv4ugTdutrdVmZTiaZgo0/edit?usp=sharing",
                        sheet = "ga_suppMat_ch3")

indic_ga3 = ipbes_ga3 %>% 
  dplyr::mutate(indicators = tolower(`Indicator name`)) %>%
  dplyr::mutate(indicators = str_trim(indicators)) %>% 
  dplyr::mutate(ga = 1) %>% 
  dplyr::distinct(indicators, ga)


# combine all GA using harmonized indicators
indic_ga = indic_ga21 %>% 
  harmonize_indic(indicators) %>% 
  rbind(harmonize_indic(indic_ga22,indicators)) %>% 
  rbind(harmonize_indic(indic_ga23,indicators), by = 'field_harm') %>% 
  full_join(harmonize_indic(indic_ga3,indicators), by = 'field_harm') %>% 
  dplyr::rename(indicators_harmonized = field_harm) %>% 
  dplyr::mutate(ga = 1) %>% 
  dplyr::mutate(indicators_harmonized = str_trim(indicators_harmonized)) %>% 
  dplyr::distinct(indicators_harmonized,ga) 
rm(indic_ga21, indic_ga22,indic_ga23,indic_ga3)  


## Indicators from tables and appendices in KM_GBF----

km_gbf = read_sheet("https://docs.google.com/spreadsheets/d/1_knKw-wf5a1-uMjFCOxD1ZdmJT8KZwSWylUv23lDlws/edit#gid=0",
                       sheet = "indicators_all")
indic_km_gbf = km_gbf %>% 
  dplyr::mutate(indicators = tolower(`Indicator`)) %>%
  dplyr::mutate(indicators = str_trim(indicators)) %>% 
  dplyr::mutate(gbf = 1) %>% 
  dplyr::filter(indicators != 'in development*') %>% 
  dplyr::filter(indicators != 'none adopted') %>% 
  dplyr::distinct(indicators, gbf)

### IPBES Assessments indicators------
# Manual extraction of 'indicator', 'index', 'indicates', 'indices' from IPBES assessments

## Global Assessment
ga = read_excel('input_data/IPBES_BIP_indicators.xlsx', sheet = 'ga') %>%
  mutate(indicators = tolower(indicators)) %>% 
  #trims trailing commas
  mutate(indicators = trimws(indicators)) %>% 
  mutate(ga=1) %>% 
  #filter(real_indicator==1) %>% 
  dplyr::select(-real_indicator) %>% 
  distinct()

# Supplementary material from GA
ga_ch2.1 = read_excel('input_data/IPBES_BIP_indicators.xlsx', sheet = 'ga_suppMat_ch2.1_drivers') %>%
  mutate(indicators = tolower(Indicator)) %>% 
  #trims trailing commas
  mutate(indicators = trimws(indicators)) %>% 
  mutate(ga = 1) %>% 
  dplyr::select(indicators, ga) %>% 
  distinct()

ga_ch2.2 = read_excel('input_data/IPBES_BIP_indicators.xlsx', sheet = 'ga_suppMat_ch2.2_nature') %>%
  mutate(indicators = tolower(Indicator)) %>% 
  #trims trailing commas
  mutate(indicators = trimws(indicators)) %>% 
  mutate(ga = 1)%>% 
  dplyr::select(indicators, ga) %>% 
  distinct()

ga_ch2.3 = read_excel('input_data/IPBES_BIP_indicators.xlsx', sheet = 'ga_suppMat_ch2.3_ncp') %>%
  mutate(indicators = tolower(indicators)) %>% 
  #trims trailing commas
  mutate(indicators = trimws(indicators)) %>% 
  mutate(ga = 1)%>% 
  dplyr::select(indicators, ga) %>% 
  distinct()

# Join all sources of GA
ga2 = ga %>% 
  rbind(ga_ch2.1,ga_ch2.2,ga_ch2.3) %>% 
  distinct()

# values Assessment
va = read_excel('input_data/IPBES_BIP_indicators.xlsx', sheet = 'va') %>%
  mutate(indicators = tolower(indicators)) %>% 
  #trims trailing commas
  mutate(indicators = trimws(indicators)) %>% 
  mutate(va=1) %>%
  filter(real_indicator==1) %>% 
  dplyr::select(-real_indicator) %>% 
  distinct()

# Sustainable use assessment
sua = read_excel('input_data/IPBES_BIP_indicators.xlsx', sheet = 'sua') %>%
  mutate(indicators = tolower(indicators)) %>% 
  #trims trailing commas
  mutate(indicators = trimws(indicators)) %>% 
  mutate(sua=1) %>%
  filter(real_indicator==1) %>% 
  dplyr::select(-real_indicator) %>% 
  distinct()

# Supplementary material from SUA
sua_ch2 = read_excel('input_data/IPBES_BIP_indicators.xlsx', sheet = 'sua_dmr_ch2') %>%
  mutate(sua_ch2_indicators = tolower(indicators)) %>% 
  mutate(indicators = word(sua_ch2_indicators,2, -1)) %>% 
  mutate(indicator_id = word(sua_ch2_indicators,1, 1)) %>% 
  mutate(sua=1) %>%
  dplyr::select(indicators, sua) %>% 
  #trims trailing commas
  mutate(indicators = trimws(indicators))

sua2 = sua %>% 
  rbind(sua_ch2) %>% 
  distinct()

#bind all IPBES indicators together (from assessments)
ipbes_indicators = ga2 %>% 
  full_join(va, by = 'indicators') %>%
  full_join(sua2, by = 'indicators') %>% 
  #flag duplicates
  check_dup(indicators) %>% 
  filter(dup1 == FALSE) %>% 
  dplyr::select(-dup1, -dup2, -dup)

ipbes_indicators %>% distinct(indicators) %>% count()#1309
ipbes_indicators %>% filter(ga==1) %>% distinct(indicators) %>% count()#882
ipbes_indicators %>% filter(va==1) %>% distinct(indicators) %>% count()#100
ipbes_indicators %>% filter(sua==1) %>% distinct(indicators) %>% count()#358

#write_csv(ipbes_indicators, 'input_data/ipbes_indicators.csv')
#ipbes_indicators = read_csv('input_data/ipbes_indicators.csv')

### IPBES core and highlighted indicators (D&K TF 2017-2018)-----
core_high = read_excel('input_data/IPBES_2018.xlsx', sheet = 'core-high') %>%
  mutate(indicators = tolower(`Specific Indicator`)) %>% 
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

### Global Biodiversity Framework indicators-------
#from CBD/COP/DEC/15/5
head = read_excel('input_data/Kumming_Montreal_GBF_Indicators.xlsx', sheet = 'headline')
comp = read_excel('input_data/Kumming_Montreal_GBF_Indicators.xlsx', sheet='component')
compy = read_excel('input_data/Kumming_Montreal_GBF_Indicators.xlsx', sheet='complementary')
names(compy)

cbd_ind = head %>% 
  dplyr::select("Draft Goal/Target","Proposed indicator","indicator type") %>% 
  rbind(comp,compy) %>% 
  # rename col
  dplyr::select("goal_target"="Draft Goal/Target","proposed_indicator"="Proposed indicator","indicator_type"="indicator type") %>% 
  #remove trailing spaces and others issues
  mutate(proposed_indicator = trimws(proposed_indicator)) %>% 
  mutate(proposed_indicator = tolower(proposed_indicator)) %>% 
  mutate(proposed_indicator = gsub('[(]in development[)]', '',proposed_indicator)) %>% 
  mutate(proposed_indicator = gsub('™', '',proposed_indicator)) %>% 
  mutate(proposed_indicator = gsub('  ', ' ',proposed_indicator)) %>% 
  mutate(proposed_indicator = gsub('for used species', '(for utilized species)',proposed_indicator)) %>% 
  mutate(proposed_indicator = gsub('growth in species occurrence records accessible through the global biodiversity information facility', 'growth in species occurrence records accessible through gbif',proposed_indicator)) %>% 
  mutate(proposed_indicator = gsub('living planet index [(][(]for utilized species[)][)]', 'living planet index (for utilized species)',proposed_indicator)) %>% 
  mutate(proposed_indicator = gsub('protected connected [(]protconn[)] index', 'protconn',proposed_indicator)) %>% 
  mutate(proposed_indicator = gsub('parc connectedness', 'protected area connectedness index (parc-connectedness)',proposed_indicator)) %>% 
  #mutate(proposed_indicator = gsub('beri', 'bioclimatic ecosystem resilience index (beri)',proposed_indicator)) %>% 
  #mutate(proposed_indicator = gsub('edge', 'changing status of evolutionary distinct and globally endangered species (edge index)',proposed_indicator)) %>% 
  mutate(proposed_indicator = gsub(' [-] trends of bilinguistic diversity and numbers of speakers of indigenous languages', '',proposed_indicator)) %>% 
  mutate(proposed_indicator = gsub('[”] would work here for ips [(]not necessarily lcs[)][,] if [‘]spatial planning[’] was substituted for conservation[.]', '',proposed_indicator)) %>% 
  mutate(proposed_indicator = gsub('green status index', 'green status of species index',proposed_indicator)) %>% 
  mutate(proposed_indicator = gsub('in either medium', 'in medium',proposed_indicator)) %>% 
  mutate(proposed_indicator = gsub('number of plant and animal genetic resources secured in medium or long[-]term conservation facilities', 'number of plant and animal genetic resources for food and agriculture secured in medium- or long-term conservation facilities',proposed_indicator)) %>% 
  mutate(proposed_indicator = gsub('the number of protected areas', 'number of protected areas',proposed_indicator)) %>% 
  mutate(proposed_indicator = gsub('percentage of threatened species that are improving in status[.]', 'percentage of threatened species that are improving in status according to the red list',proposed_indicator)) %>% 
  mutate(proposed_indicator = gsub('iucn red list of threatened species', 'iucn red list',proposed_indicator)) %>% 
  mutate(proposed_indicator = gsub('proportion of local breeds classified as being at risk extinction', 'proportion of local breeds classified as being at risk of extinction',proposed_indicator)) %>% 
  mutate(proposed_indicator = gsub('proportion of local breeds classified as being at risk, extinction', 'proportion of local breeds classified as being at risk of extinction',proposed_indicator)) %>% 
  mutate(proposed_indicator = gsub('protected area management effectiveness [(]pame[)]', 'protected areas management effectiveness',proposed_indicator)) %>% 
  mutate(proposed_indicator = gsub('red list index [(]impact of pollution[)]', 'red list index (impacts of pollution)',proposed_indicator)) %>% 
  mutate(proposed_indicator = gsub('species status index', 'species status information index',proposed_indicator)) %>% 
  mutate(proposed_indicator = gsub('trends in loss of reactive nitrogen to the environment[.]', 'trends in loss of reactive nitrogen to the environment',proposed_indicator)) %>% 
  mutate(proposed_indicator = gsub('world association of zoos and aquariums [(]waza[)] bio[-]literacy survey [(]biodiversity literacy in global zoo and aquarium visitors[)]', 'waza bio-literacy survey (biodiversity literacy in global zoo and aquarium visitors)',proposed_indicator)) %>% 
  mutate(proposed_indicator = gsub('[(]exposure to unsafe water[,] sanitation and hygiene for all [(]wash[)] services', '',proposed_indicator)) %>% 
  mutate(proposed_indicator = gsub(' [(]e[.]g[.][,] pm2[.]5 and pm10[)]', '',proposed_indicator)) %>% 
  mutate(proposed_indicator = gsub(' [(]e[.]g[.] included in nbsaps and reported in national reports', '',proposed_indicator)) %>% 
  mutate(proposed_indicator = gsub('e[.]g[.] lime [;] life[-]cycle impact assessment method based on endpoint modelling', '',proposed_indicator)) %>% 
  #flag empty indicators
  dplyr::mutate(missing_indicator = if_else(proposed_indicator == '-',
                                            true = TRUE,
                                            false = FALSE)) %>% 
  #flag duplicates
  dplyr::mutate(duplicated_indicator = duplicated(proposed_indicator, fromLast = FALSE))

fix = cbd_ind %>% 
  filter(proposed_indicator == 'beri' | proposed_indicator == 'edge') %>% 
  mutate(proposed_indicator = gsub('beri', 'bioclimatic ecosystem resilience index (beri)',proposed_indicator)) %>% 
  mutate(proposed_indicator = gsub('edge', 'changing status of evolutionary distinct and globally endangered species (edge index)',proposed_indicator)) %>% 
  mutate(duplicated_indicator = TRUE)

cbd_ind = cbd_ind %>% 
  filter(proposed_indicator != 'beri' & proposed_indicator != 'edge') %>% 
  rbind(fix) %>% 
  #trims trailing commas
  mutate(indicators = trimws(indicators)) %>% 
  mutate(gbf = 1)

#write_csv(cbd_ind, 'input_data/Kumming_Montreal_GBF_Indicators_all.csv')#ADDED AS A TAB IN SAME FILE
#cbd_ind = read_csv('input_data/Kumming_Montreal_GBF_Indicators_all.csv')

cbd_ind  %>% filter(missing_indicator == FALSE) %>% count() #367
cbd_ind  %>% filter(missing_indicator == FALSE) %>% 
  distinct(proposed_indicator) %>% count() #298

cbd_ind  %>% filter(missing_indicator == FALSE) %>%  group_by(indicator_type) %>% 
  #distinct(proposed_indicator,indicator_type) %>% 
  count()
# complementary    265
# component         66
# headline          36

# # List of indicators from website (https://www.post-2020indicators.org/) ---> OUTDATED!!
# gbf_ind = read_csv('C:/Users/yanis/Documents/IPBES/indicators/CBD_filtered-indicators.csv') 
# names(gbf_ind)
# gbf_ind = gbf_ind %>% 
#   mutate(indicators = word(`Indicator name`,2, -1)) %>% 
#   mutate(milestone = word(`Indicator name`,1, 1)) %>% 
#   mutate(indicators = tolower(indicators)) #%>% 
#   #dplyr::select("Goal/target","indicators","milestone")
# #write_csv(gbf_ind,'C:/Users/yanis/Documents/IPBES/indicators/CBD_filtered-indicators_formated.csv')
# 
# missing_in_cbd = anti_join(gbf_ind,cbd_ind, by = c('indicators' = 'proposed_indicator'))#35 differences
# # most are different naming 

### Sustainable Development Goals indicators------
# from https://unstats.un.org/sdgs/indicators/indicators-list/ (A/RES/71/313)
sdg = read_excel('input_data/SDG_indicators.xlsx', sheet = 'sdg') %>%
  mutate(goal_id = gsub('Goal ','',goal_id)) %>% 
  mutate(sdg_indicators = tolower(sdg_indicators)) %>% 
  mutate(indicators = word(sdg_indicators,2, -1)) %>% 
  mutate(indicator_id = word(sdg_indicators,1, 1)) %>% 
  mutate(targets = word(sdg_targets,2, -1)) %>% 
  mutate(target_id = word(sdg_targets,1, 1)) %>%
  dplyr::select(goal_id, goals=sdg_goals,target_id,targets=sdg_targets,indicator_id,indicators)
  
#compare to relevant indicators from sdg_bip
sdg_bip = read_excel('input_data/IPBES_BIP_indicators.xlsx', sheet = 'sdg_bip') %>%
  dplyr::select(indicators, sdg_bip) %>%
  mutate(indicators = tolower(indicators))

missing_in_sdg = anti_join(sdg_bip, sdg, by='indicators')

# several are missing, will add them!!
names(sdg)

sdg_all = sdg %>% 
  mutate(sdg_bip = NA) %>% 
  rbind(mutate(missing_in_sdg,targets=NA, target_id = NA, indicator_id=NA,goals=NA,goal_id = NA)) %>% 
  #flag duplicates
  dplyr::mutate(duplicated_indicator = duplicated(indicators, fromLast = FALSE)) %>% 
  # add sdg column
  mutate(sdg=1) %>% 
  #trims trailing commas
  mutate(indicators = trimws(indicators))

#write_csv(sdg_all, 'input_data/sdg_all.csv') #ADDED AS A TAB IN SAME FILE 
#sdg_all = read_csv('input_data/sdg_all.csv')

sdg_all  %>% filter(is.na(sdg_bip)) %>% distinct() %>% count() #249 indicators
sdg_all  %>% filter(is.na(sdg_bip)) %>% distinct(indicators) %>% count() #234 indicators

sdg_all  %>% filter(is.na(sdg_bip)) %>% distinct(goals) %>% count() #17 indicators
sdg_all  %>% filter(is.na(sdg_bip)) %>% distinct(targets) %>% count() #169 targets

sdg_all  %>% filter(sdg_bip==1) %>% distinct(indicators) %>% count() #73 indicators from bip
sdg_all  %>% distinct() %>% count() #322 indicators
sdg_all  %>% distinct(indicators) %>% count() #307 indicators


### All indicators------
names(sdg_all)
names(cbd_ind)
names(ipbes_indicators)

all_indicators = 
  #add cbd
  cbd_ind %>% 
  filter(missing_indicator==FALSE, duplicated_indicator == FALSE ) %>% 
  dplyr::select(indicators = proposed_indicator, gbf) %>% 
  #add ipbes
  full_join(ipbes_indicators, by = 'indicators') %>% 
  #add sdg
  full_join((dplyr::select(filter(sdg_all,duplicated_indicator == FALSE), indicators, sdg)), by = 'indicators') %>% 
  distinct()  %>% 
  #IPBES indicators
  mutate(ipbes = if_else(ga==1 | va == 1 | sua ==1,
                       true = 1,
                       false = 0))

names(all_indicators)

#write_csv(all_indicators, 'input_data/all_indicators.csv')
#all_indicators = read_csv('input_data/all_indicators.csv')

all_indicators %>% count()#1690
all_indicators %>% distinct(indicators) %>% count()#1690
all_indicators %>% filter(gbf==1) %>% count()#298
all_indicators %>% filter(sdg==1) %>% count()#307
all_indicators %>% filter(ipbes ==1) %>% count()#1309

### Categorize indicators-----
# This process is done manually following the categories described in Table 1 and 2

## Load previous classified indicators
all_indicators_classified=read_csv('output_data/all_indicators_processed.csv')

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




