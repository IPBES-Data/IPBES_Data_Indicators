# clean you environment
rm(list=ls())

############################################################################
# Integrate indicators extracted from global assessments (IPBES, IPCC, GEO) 
# & and other MEA (CBD, SDGs)
############################################################################
# Created by Yanina Sica in January 2023
# Updated Sep 2023

### Settings----

## Your working directory (will be set using function in setting.R)
your_dir <- dirname(rstudioapi::getSourceEditorContext()$path) # works only in RStudio
#your_dir <- "path_to_where_code_is" # complete accordingly

## Source useful functions from folder downloaded from GitHub
source(paste0(your_dir,"/useful_functions_indic.R"))
source(paste0(your_dir,"/settings.R"))

## Set working directory and install required libraries
your_user <- Sys.info()["user"]
your_node <- Sys.info()["nodename"]

settings(your_user,your_node)

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

# 1-Append all indicators to classify-----
# Indicators extracted using auto_search_indic.R
# Tables (indicators used policy and indicators used in supplementary material in assessments) extracted using tables_extract.R
indicators_extracted = read_csv('../input/automated_search/automated_search_indicators.csv')
indicators_policy_supp_tables = read_csv('../input/tables_extraction/policy_supp_tables_indicators.csv')

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
  dplyr::filter(!is.na(indicators_harmonized)) %>% 
  write_csv('../input/all_indicators.csv')

all_indicators %>% distinct(indicators_harmonized) %>% count() #2106 unique indicators

## 1.a-Merge with previously classified indicators ----

#Load previously classified indicators (version April 2024)
# can also be found (https://docs.google.com/spreadsheets/d/1SJJBBOYYfUE7pkGaLqbj-ialx6g1RGGubVXK5CfcG3o/edit?gid=1682891338#gid=1682891338)

classified = readxl::read_excel("../input/all_indicators_classifiedApr24.xlsx",
                        sheet = "indicators_to_classify_full2") %>% 
  dplyr::select(indicators_harmonized, Categories,	Categories_2,	Subcategories,	Subcategories_2) %>% 
  filter(!is.na(indicators_harmonized)) %>% 
  filter(!is.na(Categories))
  
# Join
all_indicators_cl = left_join(all_indicators, classified, by = 'indicators_harmonized')

changes = anti_join(classified, all_indicators, by = 'indicators_harmonized')#16 from old extraction were classified (no need to add them)
changes = anti_join(all_indicators,classified, by = 'indicators_harmonized')#5 indicator from last extraction were not classified
# these 5 indicators do not add info (associated species,circulation, radio occultation, pollen deposition==pollen presence)

all_indicators_cl %>% filter(!is.na(Categories)) %>% count() #2101 classified
all_indicators_cl %>% filter(is.na(Categories)) %>% count() #5 NOT classified yet

# Remove not classified indicators (they do not add info)
all_indicators_cl = all_indicators_cl %>%  filter(!is.na(Categories))

all_indicators_cl %>% filter(indic == 1 & is.na(Categories)) %>% count() #0 real indicators NOT classifed yet

# save
write_csv(all_indicators_cl, '../input/all_indicators_classifiedMay24.csv')

# 2-Classify indicators----
# This was manual process that happened in multiple iterations (based on the April24 version, this May24 is the latest one)

# 3-Summaries----

all_indicators_cl %>% filter(!is.na(indicators_harmonized)) %>% distinct(indicators_harmonized) %>% count() #2101 unique indicators
all_indicators_cl %>% filter(!is.na(indicators_harmonized)) %>% filter(!is.na(Categories)) %>% distinct(indicators_harmonized) %>% count() #2101 unique indicators categorized

# re-format table
all_indicators_cl2 = all_indicators_cl %>% 
  # clean dataset
  filter(!is.na(indicators_harmonized)) %>% 
  filter(!is.na(Categories)) %>% 
  dplyr::select(-ILK_ext) %>% 
  dplyr::relocate(ipbes, .after = policy) %>% 
  # calculate how many times the indicators were used
  dplyr::mutate(usage = rowSums(across(ga:unccd), na.rm = TRUE)) %>% 
  dplyr::mutate(usage_policy = rowSums(across(km_gbf:unccd), na.rm = TRUE)) %>% 
  dplyr::mutate(usage_assess = rowSums(across(ga:ipcc), na.rm = TRUE)) %>% 
  dplyr::mutate(usage_ipbes = rowSums(across(ga:ias), na.rm = TRUE)) %>% 
  # get source in a column (1 source by entry --> source)
  pivot_longer(
    cols = ga:unccd,
    names_to = "source",
    values_to = "value") %>%
  filter(value != 0) %>%
  dplyr::select(-value) %>% 
  # create assessment field
  mutate(source = toupper(source)) %>% 
  mutate(source = gsub('KM_GBF','GBF', source)) %>% 
  mutate(assessment = if_else(source %in% c("GA", "SUA", "IAS","VA", "IPCC", "GEO"),
                              true = 1,
                              false = 0)) %>% 
  # paste all sources together (multiple sources by entry--> sources))
  group_by(indicators_harmonized) %>%
  mutate(sources = paste0(source,collapse = ",")) %>%
  ungroup() %>%
  dplyr::select(-source) %>%
  # 1 indicator == 1 entry (keep sources)
  distinct(indicators_harmonized, sources, .keep_all = TRUE) %>%
  # improve categories
  mutate(Categories = str_to_title(Categories)) %>% 
  mutate(Categories_2 = str_to_title(Categories_2)) %>% 
  mutate(Subcategories = str_to_title(Subcategories)) %>% 
  mutate(Subcategories_2 = str_to_title(Subcategories_2)) %>% 
  dplyr::select("indicators_harmonized","policy", "assessment","sources","ipbes",
                "usage","usage_policy","usage_assess","usage_ipbes",
                "indic","indic_ext","var_ext",
                "Categories","Categories_2","Subcategories","Subcategories_2") %>% 
  write_csv('../output/all_harmonized_classified_indicatorsMay24.csv')
names(all_indicators_cl2)

# 3.a-Summaries of indicators by source----

all_indicators_cl2 %>% 
  mutate(sources = strsplit(as.character(sources), ",")) %>% 
  unnest(sources) %>% 
  group_by(sources) %>% count() %>% arrange(desc(n))
# 1 GA        764
# 2 IPCC      374
# 3 GBF       307
# 4 SDG       257
# 5 GEO       223
# 6 SUA       213
# 7 IAS        78
# 8 VA         72
# 9 ICCWC      50
# 10 CITES      41
# 11 CMS        25
# 12 RAMSAR     18
# 13 UNCCD       4

all_indicators_cl2 %>% filter(assessment ==1) %>% distinct(indicators_harmonized) %>% count() #1648
all_indicators_cl2 %>% filter(policy ==1) %>% distinct(indicators_harmonized) %>% count() #647

#filter(!sources %in% c('GBF','SDG','UNCCD','CITES','CMS','ICCWC','RAMSAR' )) %>% #1653 indic in assessments
#filter(sources %in% c('GBF','SDG','UNCCD','CITES','CMS','ICCWC','RAMSAR' )) %>% # 648 indic in policy

data_hist1 = all_indicators_cl2 %>% 
  mutate(source = strsplit(as.character(sources), ",")) %>% 
  unnest(source) %>% 
  filter(assessment ==1) %>%
  filter(!source %in% c('GBF','SDG','UNCCD','CITES','CMS','ICCWC','RAMSAR' )) %>% 
  mutate(organization = if_else(source %in% c('VA', 'IAS','GA','SUA'),
                                true = 'IPBES',
                                false = 'UNEP')) %>%
  mutate(organization = if_else(source == 'IPCC',
                                true = 'IPCC',
                                false = organization)) %>% 
  group_by(source, organization) %>%  count() %>% arrange(organization)

ggplot(data=data_hist1, aes(x=source, y=n, fill=organization)) +
  geom_bar(stat="identity", color="black") + 
  labs(y="Number of indicators", x = "") +
  scale_fill_brewer(palette="Greens", limits=c("UNEP", "IPCC", "IPBES"),name = '') +
  scale_x_discrete(limits=c("GA", "SUA", "IAS","VA", "IPCC", "GEO")) +
  theme_minimal()

data_hist2 = all_indicators_cl2 %>% 
  mutate(source = strsplit(as.character(sources), ",")) %>% 
  unnest(source) %>% 
  filter(policy ==1) %>%
  filter(source %in% c('GBF','SDG','UNCCD','CITES','CMS','ICCWC','RAMSAR' )) %>% 
  group_by(source) %>% count() %>% arrange(desc(n))

ggplot(data=data_hist2, aes(x=source, y=n, fill=source,legend = FALSE )) +
  geom_bar(stat="identity", color="black") + 
  labs(y="Number of indicators", x = "") +
  scale_fill_brewer(palette="Blues", limits=c("UNCCD","RAMSAR","CMS", "CITES","ICCWC","SDG","GBF"),name = '') +
  scale_x_discrete(limits=c("UNCCD","RAMSAR","CMS", "CITES","ICCWC","SDG","GBF")) +
  theme_minimal()

# 3.b-Summaries of shared indicators ----

# by common indicators
all_indicators_cl2 %>% filter(usage >= 2) %>% distinct(indicators_harmonized) %>% count() #235
235/2001
all_indicators_cl2 %>% arrange(desc(usage)) %>% distinct(indicators_harmonized, .keep_all = TRUE) %>%  
  dplyr::select(indicators_harmonized, sources, usage) %>% View()
# red list index (6)
# GA,SUA,IAS,GEO,GBF,SDG
# proportion of fish stocks within biologically sustainable levels (5)
# GA,SUA,GEO,GBF,SDG
# proportion of agricultural area under productive and sustainable agriculture (5)
# GA,SUA,GEO,GBF,SDG
# forest area as a percentage of total land area (5)
# GA,SUA,GEO,GBF,SDG
# proportion of land that is degraded over total land area (5)
# SUA,GEO,GBF,SDG,UNCCD

# by common sources
all_indicators_cl2 %>% filter(grepl('[,]',sources)) %>% group_by(sources) %>% 
  count() %>% arrange(desc(n)) %>% View()
# SUA,SDG (96)
# GA,GBF (29)
# SUA,GBF,SDG (29)
# GA,GEO
# GBF,SDG
# GEO,GBF
# SUA,VA,GBF,SDG

all_indicators_cl %>% filter(!is.na(indicators_harmonized)) %>% filter(!is.na(Categories)) %>% distinct(indicators_harmonized, .keep_all = TRUE) %>% 
   filter(sua == 1 & sdg == 1) %>% count() # 145
  # filter(sdg == 1 & km_gbf == 1 ) %>% count() # 53
  # filter(sua == 1 & km_gbf == 1 ) %>% count() # 47
  # filter(ga == 1 & km_gbf == 1) %>% count() # 42
  # filter(sua == 1 & sdg == 1 & km_gbf == 1 ) %>% count() # 43
  # filter(ga == 1 & geo == 1) %>% count() # 21

# 3.c-Summaries of indicators by category----

all_indicators_cl2 %>% distinct(Categories)
# 1 ecosystems        
# 2 biodiversity      
# 3 Ecosystem services
# 4 Human assets      
# 5 Governance        
# 6 Direct drivers    
# 7 Human well-being  
# 8 Knowledge systems
all_indicators_cl2 %>% distinct(Subcategories) %>% count()#46

data_categories = all_indicators_cl2 %>% 
  group_by(Categories) %>%
  summarize(n = n()) %>%
  mutate(prop = (n / colSums(across(n)))*100) %>% 
  arrange(desc(n))

# Basic piechart
ggplot(data_categories, aes(x="", y=prop, fill=Categories)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  scale_fill_manual(values =c('Ecosystems'="#440154",'Biodiversity'="#46327e",
                              'Governance'="#365c8d",'Direct Drivers'="#277f8e",
                              'Human Assets'="#1fa187",'Ecosystem Services'="#4ac16d",
                              'Human Well-Being'="#a0da39",'Knowledge Systems'="#fde725")) +
  # scale_x_discrete(limits=c('Ecosystems','Biodiversity',
  #                           'Governance','Direct Drivers',
  #                           'Human Assets','Ecosystem Services',
  #                           'Human Well-Being','Knowledge Systems')) +
  
  theme_void() # remove background, grid, numeric labels

all_indicators_cl2 %>% filter(!is.na(Categories_2)) %>% count()#356
all_indicators_cl2 %>% filter(!is.na(Categories_2)) %>% group_by(Categories) %>% count()

data_subcategories = all_indicators_cl2 %>% 
  group_by(Subcategories) %>%
  summarize(n = n()) %>%
  mutate(prop = (n / colSums(across(n)))*100) %>% 
  arrange(desc(n))

# categories
all_indicators_cl2 %>%   
  # unsplit meas to get each policy source per row --> source
  mutate(source = strsplit(as.character(sources), ",")) %>% 
  unnest(source) %>% 
  group_by(Categories, sources) %>% count() %>% arrange(desc(n)) %>% View()

## 4-Policy indicators-----

policy_indic_cl = all_indicators_cl2 %>% 
  # unsplit sources to get each policy source per row --> source
  mutate(source = strsplit(as.character(sources), ",")) %>% 
  unnest(source) %>% 
  # get only policy indicators
  #filter(policy == 1) %>%
  filter(source %in% c('GBF','SDG','UNCCD','CITES','CMS','ICCWC','RAMSAR' )) %>% 
  # paste all assessment sources together --> meas
  group_by(indicators_harmonized) %>%
  mutate(meas = paste0(source,collapse = ",")) %>%
  ungroup() %>%
  # keep each indicator per row keeping multiple sources of policies together (assess)
  distinct(indicators_harmonized, meas, .keep_all = TRUE) %>%
  # remove source to avoid confusion
  dplyr::select(-source) %>% 
  write_csv('../output/policy_indicatorsMay24.csv')
  
policy_indic_cl = read_csv('../output/policy_indicatorsMay24.csv')

#unique indicators and categories
policy_indic_cl %>% distinct(indicators_harmonized) %>% count()#647 indicators
policy_indic_cl %>% distinct(Categories)#8 categories
policy_indic_cl %>% distinct(Categories, Subcategories) %>% count()#51 Subcategories
policy_indic_cl %>% distinct(indicators_harmonized, .keep_all = TRUE) %>% filter(assessment == 1) %>%  count() #194
194/647 #--> less that 30% used in assessments

# indicators usage
policy_indic_cl %>% filter(usage_policy>=3) %>% distinct(indicators_harmonized) %>% count() #1
policy_indic_cl %>% filter(usage_policy>=3) %>% distinct(indicators_harmonized)
policy_indic_cl %>% filter(usage_policy==2) %>% distinct(indicators_harmonized) %>% count() #53

## 4.a-Policy indicators by source-----

# all  policy indic by source
policy_indic_cl %>%   
  # unsplit meas to get each policy source per row --> source
  mutate(source = strsplit(as.character(meas), ",")) %>% 
  unnest(source) %>% group_by(source) %>% 
  count() %>% arrange(desc(n))
# source     n
# 1 GBF      307
# 2 SDG      257
# 3 ICCWC     50
# 4 CITES     41
# 5 CMS       25
# 6 RAMSAR    18
# 7 UNCCD      4

# unique by source
policy_indic_cl %>% filter(!grepl('[,]',meas)) %>% group_by(meas) %>% 
  count() %>% arrange(desc(n))
# meas       n
# 1 GBF      253
# 2 SDG      204
# 3 ICCWC     50
# 4 CITES     41
# 5 CMS       25
# 6 RAMSAR    18
# 7 UNCCD      2

# shared policy indic by source
policy_indic_cl %>% filter(grepl('[,]',meas)) %>% group_by(meas) %>% 
  count() %>% arrange(desc(n))
# 1 GBF,SDG          52
# 2 GBF,SDG,UNCCD     1
# 3 GBF,UNCCD         1



## 4.b-Policy indicators by category-----

policy_indic_cl %>%   
  # unsplit meas to get each policy source per row --> source
  mutate(source = strsplit(as.character(meas), ",")) %>% 
  unnest(source) %>% 
  # summary Cat
  group_by(Categories)  %>% 
  summarize(n = n()) %>%
  mutate(prop = (n / colSums(across(n)))*100) %>% 
  arrange(desc(n))

policy_indic_cl %>%   
  # unsplit meas to get each policy source per row --> source
  mutate(source = strsplit(as.character(meas), ",")) %>% 
  unnest(source) %>% 
  # summary Subcat
  group_by(Subcategories)  %>% 
  summarize(n = n()) %>%
  mutate(prop = (n / colSums(across(n)))*100) %>% 
  arrange(desc(n))


policy_indic_cl %>%   
  # unsplit meas to get each policy source per row --> source
  mutate(source = strsplit(as.character(meas), ",")) %>% 
  unnest(source) %>% 
  # summary cat + mea
  group_by(Categories, meas) %>% 
  count() %>% arrange(desc(n))

policy_categories = policy_indic_cl %>%   
  # unsplit meas to get each policy source per row --> source
  mutate(source = strsplit(as.character(meas), ",")) %>% 
  unnest(source) %>% 
  # summary cat + source
  group_by(source,Categories)  %>% 
  count() %>% arrange(desc(n))


ggplot(policy_categories,
       aes(y = n,
           axis1 = source, axis2 = Categories)) +
  geom_alluvium(aes(fill = Categories),
                width = 1/6, knot.pos = 0, reverse = FALSE) +
  scale_fill_manual(values =c('Ecosystems'="#440154",'Biodiversity'="#46327e",
                              'Governance'="#365c8d",'Direct Drivers'="#277f8e",
                              'Human Assets'="#1fa187",'Ecosystem Services'="#4ac16d",
                              'Human Well-Being'="#a0da39",'Knowledge Systems'="#fde725")) +
  guides(fill = "none") +
  geom_stratum(alpha = .1, width = 1/6, reverse = FALSE) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)),
            reverse = FALSE) +
  scale_x_continuous(breaks = 1:2) +
  theme_void()

## 4.c-ILK indicators in policy-----
policy_indic_cl %>%   
  # unsplit meas to get each policy source per row --> source
  mutate(source = strsplit(as.character(meas), ",")) %>% 
  unnest(source) %>% 
  filter(Subcategories =="Ilk")

## 4.d-Trace back policy indicators to original name-----

#load original tables
cites_indic = read.csv('../input/tables_extraction/cites_indicators.csv') %>% 
  mutate(meas = "CITES") %>% 
  dplyr::select(indic_id, indicators, indicators_harmonized,meas) #%>% 
  #distinct(indicators, .keep_all = TRUE)
ramsar_indic = read.csv('../input/tables_extraction/ramsar_indicators.csv') %>% 
  mutate(meas = "RAMSAR") %>% 
  dplyr::select(indic_id, indicators, indicators_harmonized,meas) #%>% 
  #distinct(indicators, .keep_all = TRUE)
sdg_indic = read.csv('../input/tables_extraction/sdg_indicators.csv') %>% 
  mutate(meas = "SDG") %>% 
  mutate(Indicator = gsub("^\\S+ ", "", sdg_indicators)) %>% 
  dplyr::select(indic_id, indicators = Indicator, indicators_harmonized,meas) #%>% 

  #distinct(indicators, .keep_all = TRUE)
unccd_indic = read.csv('../input/tables_extraction/unccd_indicators.csv') %>% 
  mutate(meas = "UNCCD") %>% 
  dplyr::select(indic_id, indicators, indicators_harmonized, meas) #%>% 
  #distinct(indicators, .keep_all = TRUE)
gbf_indic = read.csv('../input/tables_extraction/km_gbf_indicators.csv') %>% 
  mutate(meas = "GBF") %>% 
  mutate(Indicator = gsub(' [(]SDG 4[.]7[.]1[)]','',Indicator)) %>% 
  mutate(Indicator = gsub('Intact Wilderness','Intact wilderness',Indicator)) %>% 
  # mutate(Indicator = gsub('secured in either medium','secured in medium',Indicator)) %>%
  mutate(Indicator = gsub('Food Insecurity Experience Scale','Food Insecurity Experience Scale (FIES)',Indicator)) %>%
  mutate(Indicator = gsub("Proportion of countries where the legal framework [(]including customary law[)] guarantees women[']s equal rights to land ownership and[/]or control","Proportion of countries where the legal framework (including customary law) guarantees women’s equal rights to land ownership and/or control",Indicator)) %>%
  mutate(Indicator = gsub("flow safely treated","flows safely treated",Indicator)) %>%
  mutate(Indicator = gsub("as being at risk extinction","as being at risk of extinction",Indicator)) %>%
  mutate(Indicator = gsub("Dollar value of financial and technical assistance [(]including through North[-]South[,] South[-]South and triangular cooperation[)] committed to developing countries","Dollar value of financial and technical assistance (including through North-South, South‑South and triangular cooperation) committed to developing countries",Indicator)) %>%
  mutate(Indicator = gsub("Amount of fossil[-]fuel subsidies per unit of GDP [(]production and consumption[)]","Amount of fossil-fuel subsidies (production and consumption) per unit of GDP",Indicator)) %>%
  mutate(Indicator = gsub("Red List index","Red List Index",Indicator)) %>%
  mutate(Indicator = gsub("North[]South ","North-South ",Indicator)) %>%
  mutate(Indicator = gsub("Expected Loss of Phylogenetic diversity","Expected loss of Phylogenetic Diversity",Indicator)) %>%
  mutate(Indicator = gsub("Species status index","Species Status Index",Indicator)) %>%
  mutate(Indicator = gsub("Species habitat Index","Species Habitat Index",Indicator)) %>%
  mutate(Indicator = gsub("Species Status Information Index","Species Status Index",Indicator)) %>%
  mutate(Indicator = gsub("Status of key biodiversity areas","Status of Key Biodiversity Areas",Indicator)) %>%
  mutate(Indicator = gsub("Volume of production per labour unit by classes of farming[/]pastoral[/] forestry enterprise size","Volume of production per labour unit by classes of farming/pastoral/forestry enterprise size",Indicator)) %>%
  # mutate(Indicator = gsub("Index of coastal eutrophication[;] [(]b[)] plastic debris density","(a) Index of coastal eutrophication; and (b) plastic debris density",Indicator)) %>%
  #mutate(Indicator = gsub("Volume of production per labour unit by classes of farming[/]pastoral[/] forestry enterprise size","Volume of production per labour unit by classes of farming/pastoral/forestry enterprise size",Indicator)) %>%
  #mutate(Indicator = gsub("Volume of production per labour unit by classes of farming[/]pastoral[/] forestry enterprise size","Volume of production per labour unit by classes of farming/pastoral/forestry enterprise size",Indicator)) %>%
  dplyr::select(indic_id, indicators = Indicator, indicators_harmonized,meas) #%>% 
  #distinct(indicators, .keep_all = TRUE)
iccwc_indic = read.csv('../input/tables_extraction/iccwc_indicators.csv') %>% 
  mutate(meas = "ICCWC") %>% 
  dplyr::select(indic_id, indicators, indicators_harmonized,meas) #%>% 
  #distinct(indicators, .keep_all = TRUE)
cms_indic = read.csv('../input/tables_extraction/cms_indicators.csv') %>% 
  mutate(meas = "CMS") %>% 
  dplyr::select(indic_id, indicators, indicators_harmonized,meas) #%>% 
  #distinct(indicators, .keep_all = TRUE)

policy_indic = rbind(cites_indic, ramsar_indic, sdg_indic, unccd_indic, gbf_indic, iccwc_indic,cms_indic)
policy_indic %>%  distinct(indicators) %>% count() #636
policy_indic %>%  distinct(indicators_harmonized) %>% count() #647

policy_indic_complete = policy_indic_cl %>%   
  # unsplit meas to get each policy source per row 
  mutate(meas = strsplit(as.character(meas), ",")) %>% 
  unnest(meas) %>% #702
  #join 
  left_join(policy_indic, by = c('indicators_harmonized', 'meas'))

#checks
policy_indic_complete %>%  distinct(indicators) %>% count() #636 (options are aggregated, as text from documents)
policy_indic_complete %>%  distinct(indicators_harmonized) %>% count() #647 (options are dis-aggregated, but there is more harmonization)
policy_indic_cl %>%  distinct(indicators_harmonized) %>% count() #647
#ALL GOOD

missing = policy_indic_cl %>%   
  # unsplit meas to get each policy source per row --> source
  mutate(meas = strsplit(as.character(meas), ",")) %>% 
  unnest(meas) %>% 
  anti_join(policy_indic, by = c('indicators_harmonized', 'meas'))
missing = policy_indic %>%   
  anti_join(mutate(policy_indic_cl,meas = strsplit(as.character(meas), ",")) %>% 
              unnest(meas), by = c('indicators_harmonized', 'meas'))
#ALL GOOD

policy_indic_clean = policy_indic_complete %>% 
  # fix an issue in harmonized indicators
  mutate(indicators_harmonized = gsub("south[‑]south","south-south",indicators_harmonized)) %>%
  # paste all indic_id together
  group_by(indicators_harmonized) %>%
  mutate(ids = paste0(indic_id,collapse = ",")) %>%
  ungroup() %>% 
  # paste all textual indicators together
  group_by(indicators_harmonized) %>%
  mutate(indicators_orig = paste0(indicators,collapse = ",")) %>%
  ungroup() %>% 
  # paste all meas sources together
  group_by(indicators_harmonized) %>%
  mutate(mea = paste0(meas,collapse = ",")) %>%
  ungroup() %>% 
  mutate(mea = gsub('GBF,GBF,GBF',"GBF",mea)) %>%
  mutate(mea = gsub('GBF,GBF',"GBF",mea)) %>%
  mutate(mea = gsub('SDG,SDG,SDG',"SDG",mea)) %>%
  mutate(mea = gsub('SDG,SDG',"SDG",mea)) %>%
  mutate(mea = gsub('GBF,SDG,GBF,SDG',"GBF,SDG",mea)) %>%
  mutate(mea = gsub('RAMSAR,RAMSAR',"RAMSAR",mea)) %>%
  mutate(mea = gsub('SDG,SDG',"SDG",mea)) %>%
  #mutate(mea = gsub('GBF,GBF',"GBF",mea)) %>%
  distinct(indicators_harmonized, .keep_all = TRUE) %>% 
  dplyr::select("indicators_harmonized","ids","mea",'ipbes',
                "Categories","Subcategories","Categories_2","Subcategories_2",
                "indicators_orig",
                ) %>% 
  write_csv('../output/policy_indicatorsMay24_orignames.csv')
  
#checks
policy_indic_clean %>%  distinct(indicators_orig) #619
policy_indic_clean %>%  distinct(indicators_harmonized) #646

dup = check_dup(policy_indic_clean,indicators_orig)
dup = check_dup(policy_indic_clean,indicators_harmonized)

policy_indic_clean %>%  distinct(mea)
policy_indic_clean %>%  distinct(ipbes)
policy_indic_clean %>%  distinct(Categories)
policy_indic_clean %>%  distinct(Subcategories) %>% count()


## 5-Indicators in assessments-----
assess_indic_cl = all_indicators_cl2 %>% 
  # unsplit sources to get each assessment source per row --> source
  mutate(source = strsplit(as.character(sources), ",")) %>% 
  unnest(source) %>% 
  # get only indicators used in assessments
  #filter(assessment == 1) %>%
  filter(!source %in% c('GBF','SDG','UNCCD','CITES','CMS','ICCWC','RAMSAR' )) %>% 
  # paste all assessment sources together --> assess
  group_by(indicators_harmonized) %>%
  mutate(assess = paste0(source,collapse = ",")) %>%
  ungroup() %>%
  # keep each indicator per row keeping multiple sources of assess together (assess)
  distinct(indicators_harmonized, assess, .keep_all = TRUE) %>%
  # remove source to avoid confusion
  dplyr::select(-source) %>% 
  write_csv('../output/assessment_indicatorsMay24.csv')

assess_indic_cl %>% distinct(indicators_harmonized) %>% count()#1648
assess_indic_cl %>% distinct(indicators_harmonized, .keep_all = TRUE) %>% filter(policy == 1) %>% count()#194

# indicators usage
assess_indic_cl %>% filter(usage_assess>=4) %>% distinct(indicators_harmonized, assess)  #4
assess_indic_cl %>% filter(usage_assess==3) %>% distinct(indicators_harmonized, assess)  #10
assess_indic_cl %>% filter(usage_assess==2) %>% distinct(indicators_harmonized) %>% count() #44
assess_indic_cl %>% filter(usage_assess==1) %>% distinct(indicators_harmonized) %>% count() #1590

# unique indicators in each assess 
assess_indic_cl %>% filter(!grepl('[,]',assess)) %>% 
  group_by(assess) %>% count() %>% arrange(desc(n))
#GA 721
#IPCC 368
#GEO 192
#SUA 185
#IAS 67
#VA 57

# shared assess indic --> a lot to put in a table!
assess_indic_cl %>% filter(grepl('[,]',assess)) %>% 
  group_by(assess) %>% count() %>% arrange(desc(n))

# all asses indic by source
assess_indic_cl %>%   
  # unsplit assess to get each assessment source per row --> source
  mutate(source = strsplit(as.character(assess), ",")) %>% 
  unnest(source) %>% group_by(source) %>% 
  count() %>% arrange(desc(n))
# source     n
# 1 GA       764
# 2 IPCC     374
# 3 GEO      223
# 4 SUA      213
# 5 IAS       78
# 6 VA        72

### 5.a-Indicators in IPBES assessments-----
IPBES_indic_cl = all_indicators_cl2 %>% 
  # unsplit sources to get each IPBES source per row --> source
  mutate(source = strsplit(as.character(sources), ",")) %>% 
  unnest(source) %>% 
  # get only indicators used in IPBES
  filter(ipbes == 1) %>%
  filter(source %in% c('GA','VA','SUA','IAS')) %>% 
  # paste all IPBES sources together --> ipbes_assess
  group_by(indicators_harmonized) %>%
  mutate(ipbes_assess = paste0(source,collapse = ",")) %>%
  ungroup() %>%
  # keep each indicator per row keeping multiple sources of assess together (ipbes_assess)
  distinct(indicators_harmonized, ipbes_assess, .keep_all = TRUE) %>%
  # remove source to avoid confusion
  dplyr::select(-source) %>% 
  write_csv('../output/ipbes_assessment_indicatorsMay24.csv')

# summaries  
IPBES_indic_cl %>% distinct(indicators_harmonized) %>% count()#1085

# all asses indic by source
IPBES_indic_cl %>%   
  # unsplit assess to get each assessment source per row --> source
  mutate(source = strsplit(as.character(ipbes_assess), ",")) %>% 
  unnest(source) %>% group_by(source) %>% 
  count() %>% arrange(desc(n))
# 1 GA       764
# 2 SUA      213
# 3 IAS       78
# 4 VA        72
764/1091
213/1091
78/1091
72/1091

# shared assess indic --> a lot to put in a table!
IPBES_indic_cl %>% filter(grepl('[,]',ipbes_assess)) %>% 
  group_by(ipbes_assess) %>% count() %>% arrange(desc(n))
# 1 GA,SUA           12
# 2 GA,VA             6
# 3 GA,IAS            4
# 4 SUA,VA            4
# 5 GA,SUA,VA,IAS     3
# 6 GA,SUA,IAS        1
# 7 GA,SUA,VA         1
# 8 GA,VA,IAS         1
# 9 SUA,IAS           1

IPBES_indic_cl %>% filter(usage_ipbes==4) %>% distinct(indicators_harmonized, ipbes_assess) #3
IPBES_indic_cl %>% filter(usage_ipbes==3) %>% distinct(indicators_harmonized, ipbes_assess) #3
IPBES_indic_cl %>% filter(usage_ipbes==2) %>% distinct(indicators_harmonized,ipbes_assess) %>%  count() #27
IPBES_indic_cl %>% filter(usage_ipbes==1) %>% distinct(indicators_harmonized,ipbes_assess) %>% count() #1052

# all IPBES indic by category

IPBES_indic_cl %>%   
  # unsplit meas to get each policy source per row --> source
  mutate(source = strsplit(as.character(ipbes_assess), ",")) %>% 
  unnest(source) %>% group_by(Categories)  %>% 
  summarize(n = n()) %>%
  mutate(prop = (n / colSums(across(n)))*100) %>% 
  arrange(desc(n))

IPBES_indic_cl %>%   
  # unsplit meas to get each policy source per row --> source
  mutate(source = strsplit(as.character(ipbes_assess), ",")) %>% 
  unnest(source) %>% group_by(Subcategories)  %>% 
  summarize(n = n()) %>%
  mutate(prop = (n / colSums(across(n)))*100) %>% 
  arrange(desc(n))


IPBES_indic_cl %>%   
  # unsplit meas to get each policy source per row --> source
  mutate(source = strsplit(as.character(ipbes_assess), ",")) %>% 
  unnest(source) %>% 
  group_by(Categories, ipbes_assess) %>% count() %>% arrange(desc(n)) %>% View()

IPBES_categories = IPBES_indic_cl %>%   
  # unsplit meas to get each policy source per row --> source
  mutate(source = strsplit(as.character(ipbes_assess), ",")) %>% 
  unnest(source) %>% group_by(source,Categories)  %>% 
  count() %>% arrange(desc(n))


ggplot(IPBES_categories,
       aes(y = n,
           axis1 = source, axis2 = Categories)) +
  geom_alluvium(aes(fill = Categories),
                width = 1/6, knot.pos = 0, reverse = FALSE) +
  scale_fill_manual(values =c('Ecosystems'="#440154",'Biodiversity'="#46327e",
                              'Governance'="#365c8d",'Direct Drivers'="#277f8e",
                              'Human Assets'="#1fa187",'Ecosystem Services'="#4ac16d",
                              'Human Well-Being'="#a0da39",'Knowledge Systems'="#fde725")) +
  guides(fill = "none") +
  geom_stratum(alpha = .1, width = 1/6, reverse = FALSE) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)),
            reverse = FALSE) +
  scale_x_continuous(breaks = 1:2) +
  theme_void()


#ILK
IPBES_indic_cl %>%   
  # unsplit meas to get each policy source per row --> source
  mutate(source = strsplit(as.character(ipbes_assess), ",")) %>% 
  unnest(source) %>% 
  filter(Subcategories =="Ilk")


### 5.b-IPBES core and highlighted indicators (D&K TF 2017-2018)-----
indic_core_high = read_csv('../input/tables_extraction/ipbes_core_high_indicators.csv') %>% 
  # add source
  dplyr::mutate(core_high = 1) %>% 
  dplyr::distinct(indicators_harmonized, .keep_all = TRUE)  %>% 
  dplyr::select(indicators_harmonized, core_high, type_indicators)


# core and highlighted indicators NOT used
not_used = anti_join(indic_core_high,IPBES_indic_cl)
# if disaggregated this indicators is already included.
