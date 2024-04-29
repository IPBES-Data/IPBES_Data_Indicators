rm(list=ls())

############################################################################
# Integrate indicators extracted from global assessments (IPBES, IPCC, GEO) 
# & and other MEA (CBD, SDGs)
############################################################################
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
# Read installed libraries

library(stringr)
library(gtools)
library(dplyr)
library(tidyr)
library(readr)
library(data.table)
library(purrr)
library(googlesheets4)
library(ggplot2)

# Set data directories 
gd_dir = 'G:/.shortcut-targets-by-id/1H9meyomoJK2QW531mPnEcynUYZOl3Itm/_TSU Internal/_ Weekly reports/Files - TSU Data/indicators/'
git_dir = "C:/Users/yanis/Documents/scripts/IPBES-Data/IPBES_Data_Indicators/"

# 1-Append all indicators to classify-----
# Indicators extracted using auto_search_indic.R
# Tables (indicators used policy and indicators used in supplementary material in assessments) extracted using tables_extract.R

indicators_extracted = read_csv(paste0(git_dir,'input/automated_search/automated_search_indicators.csv'))
indicators_policy_supp_tables = read_csv(paste0(git_dir,'input/tables_extraction/policy_supp_tables_indicators.csv'))

all_indicators = indicators_policy_supp_tables %>% 
  full_join(indicators_extracted, by = 'indicators_harmonized') %>% 
  dplyr::mutate(ga = if_else(ga_sup == 1 | ga_extracted == 1,
                             true = 1,
                             false = 0)) %>% 
  dplyr::mutate(sua = if_else(sua_sup == 1 | sua_extracted == 1,
                              true = 1,
                              false = 0)) %>% 
  dplyr::mutate(ipcc = if_else(ipcc_sup == 1 | ipcc_extracted == 1,
                               true = 1,
                               false = 0)) %>% 
  dplyr::mutate(ipbes = if_else(ga == 1 | va_extracted == 1 | sua ==1 | ias_extracted == 1,
                                true = 1,
                                false = 0)) %>% 
  dplyr::mutate(policy = if_else(km_gbf == 1 | sdg == 1 | cites ==1 | cms == 1 | iccwc == 1 | ramsar == 1 | unccd ==1,
                                true = 1,
                                false = 0)) %>% 
  # add as indic_ext all indicators that include the word 'index'
  dplyr::mutate(indic_ext2 = if_else(grepl('index', indicators_harmonized),
                                 true = 1,
                                 false = indic_ext)) %>%
  dplyr::mutate(var_ext2 = if_else(grepl('index', indicators_harmonized),
                               true = NA,
                               false = var_ext)) %>%
  # add other indices as indic_ext
  dplyr::mutate(indic_ext2 = if_else(indicators_harmonized %in% c('indicator of unsustainable fisheries [(]iuu[)]', 'genuine progress indicator','gross domestic product',
                                                              'mean species abundance (msa)', 'mean species abundance [(]msa[)] [(]hotspots[)]','mean species abundance [(]msa[)] [(]indigenous lands[)]'),
                                 true = 1,
                                 false = indic_ext2)) %>%
  dplyr::mutate(var_ext2 = if_else(indicators_harmonized %in% c('indicator of unsustainable fisheries [(]iuu[)]', 'genuine progress indicator','gross domestic product',
                                                            'mean species abundance (msa)', 'mean species abundance [(]msa[)] [(]hotspots[)]','mean species abundance [(]msa[)] [(]indigenous lands[)]'),
                               true = NA,
                               false = var_ext2)) %>%
  dplyr::select(-var_ext, -indic_ext) %>% 
  dplyr::select(indicators_harmonized, ga, sua, va = va_extracted, ias = ias_extracted,ipbes, geo = geo_extracted, ipcc, km_gbf, sdg,cites,cms,iccwc,ramsar, unccd,policy,indic, indic_ext =indic_ext2, var_ext=var_ext2,ILK_ext) %>% 
  write_csv(paste0(git_dir,'input/all_indicators.csv')) %>% 
  write_csv(paste0(gd_dir,'input_data2/all_indicators.csv'))


## 1.a-Merge with classified indicators ----

#Load classified indicators (version Feb 2024)
classified = read_sheet("https://docs.google.com/spreadsheets/d/1-J-iPKU9ARkW5sH0K8K0OOZD4r8FEsfwGk3yg6wBO3U/edit#gid=748616117",
                        sheet = "indicators_to_classify_full") %>% 
  dplyr::select(indicators_harmonized, Categories,	Categories_2,	Subcategories,	Subcategories_2)

# Join
all_indicators_cl = full_join(all_indicators, classified, by = 'indicators_harmonized')

changes = anti_join(classified, all_indicators, by = 'indicators_harmonized')
# seems good

all_indicators_cl %>% filter(!is.na(Categories)) %>% count() #1198 classified
all_indicators_cl %>% filter(is.na(Categories)) %>% count() #1018 NOT classified yet

all_indicators_cl %>% filter(indic == 1 & is.na(Categories)) %>% count() #143 real indicators NOT classifed yet

# save
write_csv(all_indicators_cl, paste0(git_dir,'output/indicators_to_classify_full2.csv'))
write_csv(all_indicators_cl, paste0(gd_dir,'output_data2/indicators_to_classify_full2.csv'))

# 2-Classify indicators----
# This is a manual process happened in mutiple iterations here: https://docs.google.com/spreadsheets/d/1-J-iPKU9ARkW5sH0K8K0OOZD4r8FEsfwGk3yg6wBO3U/edit#gid=748616117
# and here: https://docs.google.com/spreadsheets/d/1SJJBBOYYfUE7pkGaLqbj-ialx6g1RGGubVXK5CfcG3o/edit#gid=1682891338
# 3-Summaries----

all_indicators_cl = read_sheet("https://docs.google.com/spreadsheets/d/1SJJBBOYYfUE7pkGaLqbj-ialx6g1RGGubVXK5CfcG3o/edit#gid=1682891338",
                        sheet = "indicators_to_classify_full2") 

## 3.a-Policy indicators-----

policy_indic_cl = all_indicators_cl %>% 
  dplyr::filter(policy ==1) %>% 
  dplyr::select(-`change in indic name`, -ga, -sua, -va, -ias, -geo, -ipcc, -ipbes,-ILK_ext,-indic_ext,-var_ext,-indic, -policy)%>% 
  # format all sources (not sure why some of them are logical)
  #dplyr::mutate(across(where(is.logical), as.character)) %>% 
  #mutate(across(km_gbf:unccd, gsub, pattern = 'TRUE', replacement = '1')) %>% 
  #mutate(across(km_gbf:unccd, as.numeric)) %>% 
  # calculate how many times the indicators was used
  dplyr::mutate(usage = rowSums(across(km_gbf:unccd), na.rm = TRUE)) %>% 
  pivot_longer(
    cols = km_gbf:unccd,
    names_to = "mea",
    values_to = "value") %>%
  filter(value != 0) %>%
  dplyr::select(-value) %>% 
  group_by(indicators_harmonized) %>%
  mutate(meas = paste0(mea,collapse = ",")) %>%
  ungroup() %>%
  dplyr::select(-mea) %>%
  distinct(indicators_harmonized, meas, .keep_all = TRUE) %>%
  write_csv(paste0(git_dir,'output/policy_indicators.csv'))
  
# summaries  
policy_indic_cl = all_indicators_cl %>% 
  dplyr::filter(policy ==1) %>% 
  dplyr::select(-`change in indic name`, -ga, -sua, -va, -ias, -geo, -ipcc, -ipbes,-ILK_ext,-indic_ext,-var_ext,-indic, -policy)%>% 
  # calculate how many times the indicators was used
  dplyr::mutate(usage = rowSums(across(km_gbf:unccd), na.rm = TRUE)) %>% 
  pivot_longer(
    cols = km_gbf:unccd,
    names_to = "mea",
    values_to = "value") %>%
  filter(value != 0) %>%
  dplyr::select(-value)

#unique indicators and categories
policy_indic_cl %>% distinct(indicators_harmonized) %>% count()#648 indicators
policy_indic_cl %>% distinct(Categories)#8 categories
policy_indic_cl %>% distinct(Categories, Subcategories) %>% View()#51 Subcategories

# indicators usage
policy_indic_cl %>% filter(usage>=2) %>% count() #53
policy_indic_cl %>% filter(usage>=3) #1
policy_indic_cl %>% group_by(meas) %>% count() %>% arrange(desc(n))
  
## 3.a.1-Policy indicators for B3----

# Summaries for b3 deliverable
policy_indic_B3 = policy_indic_cl %>% 
  filter(
    Categories == 'biodiversity' |
    Categories == 'ecosystems' |
    Subcategories == 'Invasive alien species') %>% 
  mutate(Categories = gsub('Direct drivers','Invasive alien species', Categories)) %>% 
  distinct(indicators_harmonized, .keep_all = TRUE) %>% 
  write_csv(paste0(git_dir,'output/b3_indic_summary.csv'))

policy_indic_B3 %>% 
  group_by(meas) %>%  count()

policy_indic_B3 %>% 
  group_by(Categories) %>%  count()
#Categories                 n
#Invasive alien species     5
#biodiversity              74
#ecosystems                98

policy_indic_B3 %>% 
  group_by(Categories, meas) %>%  count()

data_hist1 = policy_indic_B3 %>% 
  # spread long
  mutate(meas = strsplit(as.character(meas), ",")) %>% 
  unnest(meas) %>% 
  mutate(Categories = gsub('Invasive alien species','Invasive Alien Species', Categories)) %>% 
  mutate(Categories = gsub('biodiversity','Biodiversity', Categories)) %>% 
  mutate(Categories = gsub('ecosystems','Ecosystems', Categories)) %>% 
  mutate(meas = toupper(meas)) %>% 
  mutate(meas = gsub('KM_GBF','GBF', meas)) %>% 
  group_by(Categories, meas) %>%  count() %>% arrange(desc(n))

data_hist2 = policy_indic_B3 %>% 
  mutate(meas = strsplit(as.character(meas), ",")) %>% 
  unnest(meas) %>% 
  mutate(Categories = gsub('Direct drivers','Invasive Alien Species', Categories)) %>% 
  mutate(Categories = gsub('biodiversity','Biodiversity', Categories)) %>% 
  mutate(Categories = gsub('ecosystems','Ecosystems', Categories)) %>% 
  mutate(meas = toupper(meas)) %>% 
  mutate(meas = gsub('KM_GBF','GBF', meas)) %>% 
  group_by(meas) %>%  count() %>% arrange(desc(n))

# Stacked barplot with multiple groups
ggplot(data=data_hist1, aes(x=meas, y=n, fill=Categories)) +
  geom_bar(stat="identity", color="black") + 
  labs(y="Number of indicators", x = NULL) +
  scale_fill_brewer(palette="Greens", limits=c("Invasive Alien Species", "Ecosystems", "Biodiversity")) +
  scale_x_discrete(limits=c("GBF", "RAMSAR", "SDG","CMS", "UNCCD", "CITES")) +
  theme_minimal()


policy_indic_cl %>% filter(Categories == 'biodiversity' |Categories == 'ecosystems') %>% 
  group_by(Categories, meas) %>% count() 
policy_indic_cl %>% group_by(usage) %>% count()


policy_indic_cl_l = policy_indic_cl  %>% 
  pivot_longer(
    cols = km_gbf:unccd,
    names_to = "mea",
    values_to = "value") %>%
  filter(value != 0) %>%
  group_by(indicators_harmonized) %>% 
  mutate(meas = paste0(mea,collapse = ",")) %>% 
  ungroup()


policy_indic_cl_l %>% filter(bio) group_by(mea) %>% count()
mutate(n_by_mea = n()) %>% 
  ungroup() %>% 
  group_by(meas) %>% 
  mutate(n_all = n()) %>% 
  ungroup() %>% 
  #mutate(perc_by_assessment=n_by_assessment/nrow(.)) %>% 
  distinct(assessment, assess,n_by_assessment,perc_by_assessment,n_all)


  
## 3.b-Indicators in assessments-----

assess_indic_cl = all_indicators_cl %>% 
  dplyr::filter(ipbes == 1 | geo == 1) %>% 
  dplyr::select(-ipcc,-ILK_indicators,-var,-indicator)%>% 
  # format all sources (not sure why some of them are logical)
  dplyr::mutate(across(where(is.logical), as.character)) %>% 
  mutate(across(km_gbf:unccd, gsub, pattern = 'TRUE', replacement = '1')) %>% 
  mutate(across(km_gbf:unccd, as.numeric)) %>% 
  # calculate how many times the indicators was used
  dplyr::mutate(usage = rowSums(across(ga:geo), na.rm = TRUE)) %>% 
  pivot_longer(
    cols = ga:geo,
    names_to = "assess",
    values_to = "value") %>%
  filter(value != 0) %>%
  dplyr::select(-value) %>% 
  # group_by(indicators_harmonized) %>%
  # mutate(meas = paste0(mea,collapse = ",")) %>%
  # ungroup() %>%
  # dplyr::select(-mea) %>%
  # distinct(indicators_harmonized, meas, .keep_all = TRUE) %>%
  # group_by(meas) %>%  count()
  write_csv(paste0(git_dir,'output/assessments_indicators.csv'))

assess_indic_cl_simpl = all_indicators_cl %>% 
  dplyr::filter(ipbes == 1 | geo == 1) %>% 
  dplyr::select(-ipcc,-ILK_indicators,-var,-indicator)%>% 
  # format all sources (not sure why some of them are logical)
  dplyr::mutate(across(where(is.logical), as.character)) %>% 
  mutate(across(km_gbf:unccd, gsub, pattern = 'TRUE', replacement = '1')) %>% 
  mutate(across(km_gbf:unccd, as.numeric))
assess_indic_cl_simpl %>% filter(ipbes == 1) %>% distinct(indicators_harmonized) %>% count()
assess_indic_cl_simpl %>% filter(ga == 1) %>% distinct(indicators_harmonized) %>% count()
assess_indic_cl_simpl %>% filter(sua == 1) %>% distinct(indicators_harmonized) %>% count()
assess_indic_cl_simpl %>% filter(va == 1) %>% distinct(indicators_harmonized) %>% count()
assess_indic_cl_simpl %>% filter(ias == 1) %>% distinct(indicators_harmonized) %>% count()



# summaries  
assess_indic_cl %>% distinct(indicators_harmonized) %>% count()#1208

#assess_indic_cl %>% distinct(Categories)#8 categories
#assess_indic_cl %>% distinct(Categories, Subcategories) %>% View()#46 Subcategories

## 3.a.1-Assessment indicators for B3----

# Summaries for b3 deliverable
assess_indic_B3 = assess_indic_cl %>% 
  filter(
    Categories == 'biodiversity' |
      Categories == 'ecosystems' |
      Subcategories == 'Invasive alien species') %>% 
  write_csv(paste0(git_dir,'output/b3_assess_indic_summary.csv'))

assess_indic_B3 %>% 
  mutate(Categories = gsub('Direct drivers','Invasive alien species', Categories)) %>% 
  distinct(indicators_harmonized, .keep_all = TRUE) %>% 
  group_by(Categories) %>%  count()
#Categories                 n
#Invasive alien species     3
#biodiversity              126
#ecosystems                99

data_hist1 = assess_indic_B3 %>% 
  mutate(Categories = gsub('Direct drivers','Invasive Alien Species', Categories)) %>% 
  mutate(Categories = gsub('biodiversity','Biodiversity', Categories)) %>% 
  mutate(Categories = gsub('ecosystems','Ecosystems', Categories)) %>% 
  mutate(assess = toupper(assess)) %>% 
  #mutate(mea = gsub('KM_GBF','GBF', mea)) %>% 
  group_by(Categories, assess) %>%  count() %>% arrange(desc(n))

data_hist2 = assess_indic_B3 %>% 
  mutate(Categories = gsub('Direct drivers','Invasive Alien Species', Categories)) %>% 
  mutate(Categories = gsub('biodiversity','Biodiversity', Categories)) %>% 
  mutate(Categories = gsub('ecosystems','Ecosystems', Categories)) %>% 
  mutate(assess = toupper(assess)) %>% 
  #mutate(mea = gsub('KM_GBF','GBF', mea)) %>% 
  group_by(assess) %>%  count() %>% arrange(desc(n))

# Stacked barplot with multiple groups
bplot = ggplot(data=data_hist1, aes(x=mea, y=n, fill=Categories)) +
  geom_bar(stat="identity", color="black")
bplot + 
  labs(y="Number of indicators", x = NULL) +
  scale_fill_brewer(palette="Greens", limits=c("Invasive Alien Species", "Ecosystems", "Biodiversity")) +
  scale_x_discrete(limits=c("GBF", "RAMSAR", "SDG","CMS", "UNCCD", "CITES")) +
  theme_minimal()

ggplot(data=data_hist1, aes(x=mea, y=n, fill=Categories)) +
  geom_bar(stat="identity")+
  geom_text(aes(label=n), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5)+
  scale_fill_brewer(palette="Paired")+
  theme_minimal()


policy_indic_cl %>% filter(Categories == 'biodiversity' |Categories == 'ecosystems') %>% 
  group_by(Categories, mea) %>% count() 
policy_indic_cl %>% group_by(usage) %>% count()


policy_indic_cl_l = policy_indic_cl  %>% 
  pivot_longer(
    cols = km_gbf:unccd,
    names_to = "mea",
    values_to = "value") %>%
  filter(value != 0) %>%
  group_by(indicators_harmonized) %>% 
  mutate(meas = paste0(mea,collapse = ",")) %>% 
  ungroup()


policy_indic_cl_l %>% filter(bio) group_by(mea) %>% count()
mutate(n_by_mea = n()) %>% 
  ungroup() %>% 
  group_by(meas) %>% 
  mutate(n_all = n()) %>% 
  ungroup() %>% 
  #mutate(perc_by_assessment=n_by_assessment/nrow(.)) %>% 
  distinct(assessment, assess,n_by_assessment,perc_by_assessment,n_all)


## 1.b-IPBES core and highlighted indicators (D&K TF 2017-2018)-----
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




