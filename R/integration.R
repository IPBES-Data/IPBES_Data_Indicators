# clean you environment
rm(list=ls())

############################################################################
# Integrate indicators extracted from global assessments (IPBES, IPCC, GEO) 
# & and other MEA (CBD, SDGs)
############################################################################
# Created by Yanina Sica in January 2023
# Updated May 2025

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
#library(gtools)
library(dplyr)
library(tidyr)
library(readr)
library(data.table)
library(googlesheets4)
#library(purrr)

# 1-Append all indicators to classify-----
# Indicators and other metrics extracted using `auto_search_indic.R`
ipbes_extracted = read_csv('../input/assessments/automated_search/ipbes_extracted_indicators.csv') %>% 
  dplyr::mutate(sources = 'ipbes') %>% 
  dplyr::select(-ipbes_extracted)
ipcc_extracted = read_csv('../input/assessments/automated_search/ipcc_extracted_indicators.csv') %>% 
  dplyr::mutate(sources = 'ipcc') %>% 
  dplyr::select(-ipcc_extracted)
geo_extracted = read_csv('../input/assessments/automated_search/geo_extracted_indicators.csv') %>% 
  dplyr::mutate(sources = 'geo') %>% 
  dplyr::select(-geo_extracted)

# Indicators used in tables in supplementary material in assessments extracted using `tables_extract.R`
ipbes_sup = read_csv('../input/assessments/tables_extraction/ipbes_sup_indicators.csv') %>% 
  dplyr::mutate(sources = 'ipbes') %>% 
  dplyr::select(-ipbes_sup)
ipcc_sup = read_csv('../input/assessments/tables_extraction/ipcc_sup_indicators.csv') %>% 
  dplyr::mutate(sources = 'ipcc') %>% 
  dplyr::select(-ipcc_sup)

# Join all indicators and other metrics used in assessments
assess_metrics = ipbes_extracted %>% 
  rbind(ipcc_extracted,geo_extracted,ipbes_sup,ipcc_sup) %>% 
  # Group_by indicator_harmonized and concatenate indic_ids and sourcces
  dplyr::group_by(indicator_harmonized) %>% 
  dplyr::summarise(indic_ids = paste0(indic_ids, collapse = ";"),
                   sources = paste0(sources, collapse = ";")) %>% 
  dplyr::mutate(sources = toupper(sources)) %>% 
  dplyr::mutate(sources = gsub('IPBES[;]IPBES[;]IPBES',"IPBES", sources)) %>% 
  dplyr::mutate(sources = gsub('IPBES[;]IPBES',"IPBES", sources)) %>%
  dplyr::mutate(sources = gsub('IPBES[;]GEO[;]IPBES',"IPBES;GEO", sources)) %>% 
  dplyr::mutate(sources = gsub('GEO[;]IPBES',"IPBES;GEO", sources)) %>% 
  dplyr::mutate(sources = gsub('IPCC[;]IPCC',"IPCC", sources)) %>% 
  dplyr::mutate(sources = gsub('IPBES[;]IPCC[;]IPBES',"IPBES;IPCC", sources)) %>% 
  dplyr::mutate(sources = gsub('IPBES[;]IPCC[;]GEO[;]IPBES',"IPBES;IPCC;GEO", sources)) %>% 
  # Save file
  write_csv('../input/assessments/assess_indicators.csv') %>% 
  # Format to merge with policy indicators
  dplyr::mutate(policy = 0) 

# Indicators used in MEAs extracted using `meas_indic_extract.R`
policy_indic = read_csv('../input/meas/policy_indicators.csv') %>% 
  # Concatenate sources in one column
  dplyr::mutate(indic_ids = strsplit(as.character(indic_ids), ";")) %>% 
  tidyr::unnest(indic_ids) %>% 
  dplyr::mutate(source = word(indic_ids,1, sep = '_')) %>% 
  dplyr::group_by(indicator_harmonized) %>% 
  dplyr::summarise(sources = paste0(source, collapse = ";"),indic_ids = paste0(indic_ids, collapse = ";")) %>% 
  dplyr::mutate(sources = gsub('GBF[;]GBF[;]GBF',"GBF", sources)) %>% 
  dplyr::mutate(sources = gsub('GBF[;]GBF',"GBF", sources)) %>%
  dplyr::mutate(sources = gsub('SDG[;]SDG[;]SDG',"SDG", sources)) %>% 
  dplyr::mutate(sources = gsub('SDG[;]SDG',"SDG", sources)) %>% 
  dplyr::mutate(sources = gsub('UNCCD[;]UNCCD',"UNCCD", sources)) %>% 
  dplyr::mutate(sources = gsub('RAMSAR[;]RAMSAR',"RAMSAR", sources)) %>% 
  dplyr::mutate(sources = gsub('CITES[;]CITES',"CITES", sources)) %>% 
  dplyr::mutate(sources = gsub('CMS[;]CMS',"CMS", sources)) %>% 
  dplyr::mutate(sources = gsub('GBF[;]GBF',"GBF", sources)) %>%
  dplyr::mutate(policy = 1)

# Join all indicators and other metrics together (assessments + MEAs)

all_indicators = policy_indic %>% 
  rbind(assess_metrics) %>% 
  dplyr::group_by(indicator_harmonized) %>% 
  dplyr::summarise(indic_ids = paste0(indic_ids, collapse = ";"),
                   sources = paste0(sources, collapse = ";"),
                   policy = sum(policy)) 

# checks
all_indicators %>% count() #1842
all_indicators %>% distinct(indicator_harmonized) %>% count() #1842 unique

all_indicators %>% group_by(sources) %>% count()
all_indicators %>% group_by(policy) %>% count()
all_indicators %>% filter(is.na(indic_ids))

write_csv(all_indicators,'../input/all_indicators_14052025.csv')

# 2-Classify indicators----
# This was manual process that happened in multiple iterations (based on the April24, May24 and May25 versions)
# See May25 version here: https://docs.google.com/spreadsheets/d/1SxRkXLkBcSrQHqKiT3wWrU4XODB7J24MKCdc0tCD2m4/edit?gid=801923522#gid=801923522

#Intermediate version were created merging with previously classified indicators
classified = read_csv("../output/all_indicators_classified_050525_rev14052025.csv") %>%
  # remove identified errors
  filter(error !='remove' | is.na(error)) %>% 
  dplyr::mutate(classif = 'old') %>%
  dplyr::select(indicator_harmonized, Categories,	Categories_2,	Subcategories,	Subcategories_2, classif) %>%
  filter(!is.na(indicator_harmonized)) %>%
  filter(!is.na(Categories))

# Join
all_indicators_cl = left_join(all_indicators, classified, by = 'indicator_harmonized')
all_indicators_cl %>% filter(is.na(Categories)) %>% count()
#all_indicators_cl %>% filter(is.na(Categories)) %>% View()
#all_indicators_to_cl %>% filter(is.na(Categories)) %>% count()

# CHECKS
changes = anti_join(classified, all_indicators, by = 'indicator_harmonized')#423 from old extraction were classified (no need to add them)
changes = anti_join(all_indicators,classified, by = 'indicator_harmonized')#0 indicator from last extraction were not classified

all_indicators_cl %>% filter(!is.na(Categories)) %>% count() #1842 classified
all_indicators_cl %>% filter(is.na(Categories)) %>% count() #0 NOT classified yet

# save
write_csv(dplyr::select(all_indicators_cl, -classif),'../output/all_indicators_classified_14052025.csv')
#all_indicators_cl = read_csv('../output/all_indicators_classified_14052025.csv')

# checks
check_dup(all_indicators_cl,indicator_harmonized)

# 3-Summaries----

all_indicators_cl %>% count() #1842 unique indicators
all_indicators_cl %>%  distinct(indicator_harmonized) %>% count() # all unique
all_indicators_cl %>% filter(!is.na(Categories)) %>% distinct(indicator_harmonized) %>% count() # all classified

# re-format table (long table with repetitions)
indic = all_indicators_cl %>% 
  mutate(source = strsplit(as.character(sources), ";")) %>% 
  unnest(source) %>% 
  mutate(mea = if_else(source %in% c("IPBES","IPCC", "GEO"),
         true = FALSE,
         false = TRUE)) %>% 
  mutate(assess = if_else(!source %in% c("IPBES","IPCC", "GEO"),
                       true = FALSE,
                       false = TRUE)) 

write_csv(indic,'../output/all_indicators_classified_14052025_lv.csv')
#indic = read_csv('../output/all_indicators_classified_14052025_lv.csv')

all = indic %>% count() #2110 indicators
unique = indic %>%  distinct(indicator_harmonized) %>% count() # 1842 unique
cat('all indicators: ',all$n, '\nunique indicators: ',unique$n)

# 3.a-Summaries of indicators by source----

# MEAs vs asses
all_mea = indic %>% filter(mea==TRUE) %>% count() %>% arrange(desc(n))
all_assess = indic %>% filter(assess==TRUE) %>% count() %>% arrange(desc(n))

unique_mea = indic %>% filter(mea == TRUE) %>% distinct(indicator_harmonized, .keep_all = TRUE) %>% 
  count() # 658 unique MEAs indic
unique_assess = indic %>% filter(assess == TRUE) %>% distinct(indicator_harmonized, .keep_all = TRUE) %>% 
  count() # 1369 unique assess indic
cat('all mea indicators: ', all_mea$n,'\nunique mea indicators: ',unique_mea$n, 
    '\n\nall assess indicators: ',all_assess$n, '\nunique assess indicators: ',unique_assess$n)

# sources (extracted with duplicates)
total_by_source = indic %>% group_by(source) %>% count() %>% arrange(desc(n))
# source     n
# 1 IPBES    799
# 2 IPCC     385
# 3 GBF      312
# 4 SDG      240
# 5 GEO      219
# 6 RAMSAR    65
# 7 CITES     52
# 8 CMS       25
# 9 UNCCD     13


#### Fig 1 in plots.R----

# 3.b-Summaries of reusability of indicators----

multiple_use = all_indicators_cl %>% 
  mutate(multiple_use = grepl(';', sources)) %>% group_by(multiple_use) %>% count()
# multiple_use     n
# FALSE         1634 have only 1 source
# TRUE           208 have multiple sources

cat('count of indicators re-used:', multiple_use[2,2]$n,
    '\nprop of indicators reused: ',multiple_use[2,2]$n/unique$n)

fr_indic = indic %>% 
  group_by(indicator_harmonized) %>% 
  count() %>% arrange(desc(n)) %>% 
  filter(n>1) %>% 
  left_join(distinct(indic,indicator_harmonized, .keep_all = TRUE))

fr_indic[,1:3]

#### Co_occurrence_matrix in plots.R-----
co_occurrence_matrix = read.csv('../output/all_indicators_matrix_14052025.csv')
#        IPBES GEO IPCC GBF SDG CITES CMS RAMSAR UNCCD
# IPBES    609  28    3  80 138     0   0      0     1
# GEO       28 179    4  22  12     0   0      0     0
# IPCC       3   4  379   0   0     0   0      0     0
# GBF       80  22    0 218  48     0   0      0     1
# SDG      138  12    0  48  95     0   0      0     1
# CITES      0   0    0   0   0    52   0      0     0
# CMS        0   0    0   0   0     0  25      0     0
# RAMSAR     0   0    0   0   0     0   0     65     0
# UNCCD      1   0    0   1   1     0   0      0    12

SDG_reuse = (138+12+48+1)/total_by_source[4,]$n
SDG_reuseage_MEAs = (48+1)/total_by_source[4,]$n
SDG_reuseage_assess = (138+12)/total_by_source[4,]$n
cat( 'SDG reuse: ',SDG_reuse, '\nmeas:',SDG_reuseage_MEAs, '\nassess:',SDG_reuseage_assess )

GBF_reuse = (80+22+48+1)/total_by_source[3,]$n
GBF_reuseage_MEAs = (48+1)/total_by_source[3,]$n
GBF_reuseage_assess = (80+22)/total_by_source[3,]$n
cat( 'GBF reuse: ',GBF_reuse, '\nmeas:',GBF_reuseage_MEAs, '\nassess:',GBF_reuseage_assess )


IPBES_usage_MEAS = 80+138+1
GEO_usage_MEAS = 22+12
cat( 'IPBES reuse in MEAS: ',IPBES_usage_MEAS, '\nGEO reuse in MEAS:',GEO_usage_MEAS, '\nProp of MEAs indic coming from assess:',(IPBES_usage_MEAS + GEO_usage_MEAS)/658)

#### Fig 2 in plots.R-----
#### Fig 3 in plots.R-----

# 3.c-Summaries of indicators by category-----

all_indicators_cl %>% distinct(Categories) %>% count() #8

(all_indicators_cl %>% filter(!is.na(Categories_2)) %>% count())/ #409
  (all_indicators_cl %>% filter(!is.na(Categories)) %>% count())  #1842
  
all_indicators_cl %>% 
  group_by(Categories) %>% count() %>% mutate(prop = n/unique$n) %>% arrange(desc(n))
# Categories             n   prop
# 1 ecosystems           527 0.286 
# 2 Governance           259 0.141 
# 3 Direct drivers       223 0.121 
# 4 Knowledge systems    209 0.113 
# 5 biodiversity         192 0.104 
# 6 Human assets         170 0.0923
# 7 Human well-being     140 0.0760
# 8 Ecosystem services   122 0.0662

indic %>% 
  group_by(source,Categories) %>%
  summarize(n = n()) %>% 
  count() %>% arrange(desc(n))
# 1 GBF        8
# 2 GEO        8
# 3 IPBES      8
# 4 IPCC       8
# 5 SDG        8
# 6 RAMSAR     7
# 7 CMS        5
# 8 UNCCD      5
# 9 CITES      4

indic %>% 
  group_by(source,Categories) %>% count() %>% View()

# Subcategories
all_indicators_cl %>% distinct(Subcategories) %>%  count() # 45

all_indicators_cl %>%   
  # summary cat + source
  group_by(Subcategories)  %>% 
  count() %>% arrange(desc(n)) %>% 
  mutate(prop = (n / unique$n)*100) %>% View()

  
#### Tables for Supplementary material A----
indic_categories = indic %>%   
  # summary cat + source
  group_by(source, Categories)  %>% 
  count() %>% arrange(desc(n)) %>% 
  # improve viz
  mutate(Categories = gsub('ecosystems', 'Ecosystems', Categories)) %>% 
  mutate(Categories = gsub('biodiversity', 'Biodiversity', Categories)) %>% 
  dplyr::select("Elements" = Categories, Source = source, 'Counts of metrics' = n) %>% 
  mutate(Elements = factor(Elements, 
                             levels=c('Biodiversity','Ecosystems','Ecosystem services','Human well-being',
                                      'Direct drivers','Human assets','Knowledge systems','Governance'))) %>% 
  arrange(Elements) %>% 
  write_csv('../output/sup_material/indic_categories_bysource_140525.csv')

indic_subcategories = indic %>%   
  # summary cat + source
  group_by(source, Categories, Subcategories)  %>% 
  count() %>% arrange(desc(n)) %>% 
  # improve viz
  mutate(Categories = gsub('ecosystems', 'Ecosystems', Categories)) %>% 
  mutate(Categories = gsub('biodiversity', 'Biodiversity', Categories)) %>% 
  dplyr::select("Elements" = Categories, "Subelements" = Subcategories, Source = source, 'Counts of metrics' = n) %>% 
  mutate(Elements = factor(Elements, 
                             levels=c('Biodiversity','Ecosystems','Ecosystem services','Human well-being',
                                      'Direct drivers','Human assets','Knowledge systems','Governance'))) %>% 
  arrange(Elements, Subelements) %>% 
  #mutate(prop = (`Counts of metrics` / 1843)*100) %>% 
  write_csv('../output/sup_material/indic_subcategories_bysource_140525.csv')

#### Fig 4 in plots.R-----

## 3.c-Summaries of policy indicators-----

policy_indic_cl = indic %>% 
  filter(mea == TRUE) 

# remove duplicates and assessments from sources
indic_policy = policy_indic_cl %>%
  distinct(indicator_harmonized, .keep_all = TRUE) %>% 
  mutate(sources = gsub('SDG[;]IPBES[;]GEO$','SDG', sources),
         sources = gsub('GBF[;]IPBES[;]GEO$','GBF', sources),
         sources = gsub('GBF[;]SDG[;]IPBES$','GBF;SDG', sources),
         sources = gsub('GBF[;]SDG[;]IPBES[;]GEO$','GBF;SDG', sources),
         sources = gsub('GBF[;]SDG[;]UNCCD[;]IPBES$','GBF;SDG;UNCCD', sources),
         sources = gsub('SDG[;]GEO$','SDG', sources),
         sources = gsub('SDG[;]IPBES$','SDG', sources),
         sources = gsub('GBF[;]IPBES$','GBF', sources),
         sources = gsub('GBF[;]GEO$','GBF', sources)) 

 indic_policy %>% 
  group_by(sources) %>% 
  count() %>% 
  left_join(total_by_source, by= c('sources'='source')) %>% 
  rename('n'='n.x', 'n_total'='n.y') %>% 
  arrange(sources)
# sources           n n_total
# 1 CITES            52      52
# 2 CMS              25      25
# 3 GBF             264     312
# 4 GBF;SDG          47      NA
# 5 GBF;SDG;UNCCD     1      NA
# 6 RAMSAR           65      65
# 7 SDG             192     240
# 8 UNCCD            12      13

#unique indicators 
policy_indic_cl %>% count()# 707 indicators
indic_policy %>% count() #658 unique indicators

# categories

policy_indic_cl %>% distinct(Categories)#8 categories
policy_indic_cl %>% distinct(Categories,Subcategories) %>% count()#44 Subcategories (ILK is missing)

policy_indic_cl %>% group_by(Categories) %>% count() %>%  View()
policy_indic_cl %>% group_by(Categories,Subcategories) %>% count() %>%  View()


policy_indic_cl %>% distinct(Subcategories) %>% View()#47 Subcategories
policy_indic_cl %>% distinct(indicator_harmonized, .keep_all = TRUE) %>% filter(assessment == 1) %>%  count() #194
194/647 #--> less that 30% used in assessments

# indicators usage
policy_indic_cl %>% filter(usage_policy>=3) %>% distinct(indicator_harmonized) %>% count() #1
policy_indic_cl %>% filter(usage_policy>=3) %>% distinct(indicator_harmonized)
policy_indic_cl %>% filter(usage_policy==2) %>% distinct(indicator_harmonized) %>% count() #53

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








































# categories
all_indicators_cl2 %>%   
  # unsplit meas to get each policy source per row --> source
  mutate(source = strsplit(as.character(sources), ",")) %>% 
  unnest(source) %>% 
  group_by(Categories, sources) %>% count() %>% arrange(desc(n)) %>% View()









all_indicators_cl2 = all_indicators_cl %>% 
  # clean dataset
  filter(!is.na(indicator_harmonized)) %>% 
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
  group_by(indicator_harmonized) %>%
  mutate(sources = paste0(source,collapse = ",")) %>%
  ungroup() %>%
  dplyr::select(-source) %>%
  # 1 indicator == 1 entry (keep sources)
  distinct(indicator_harmonized, sources, .keep_all = TRUE) %>%
  # improve categories
  mutate(Categories = str_to_title(Categories)) %>% 
  mutate(Categories_2 = str_to_title(Categories_2)) %>% 
  mutate(Subcategories = str_to_title(Subcategories)) %>% 
  mutate(Subcategories_2 = str_to_title(Subcategories_2)) %>% 
  dplyr::select("indicator_harmonized","policy", "assessment","sources","ipbes",
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

all_indicators_cl2 %>% filter(assessment ==1) %>% distinct(indicator_harmonized) %>% count() #1648
all_indicators_cl2 %>% filter(policy ==1) %>% distinct(indicator_harmonized) %>% count() #647

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
all_indicators_cl2 %>% filter(usage >= 2) %>% distinct(indicator_harmonized) %>% count() #235
235/2001
all_indicators_cl2 %>% arrange(desc(usage)) %>% distinct(indicator_harmonized, .keep_all = TRUE) %>%  
  dplyr::select(indicator_harmonized, sources, usage) %>% View()
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

all_indicators_cl %>% filter(!is.na(indicator_harmonized)) %>% filter(!is.na(Categories)) %>% distinct(indicator_harmonized, .keep_all = TRUE) %>% 
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
  group_by(indicator_harmonized) %>%
  mutate(meas = paste0(source,collapse = ",")) %>%
  ungroup() %>%
  # keep each indicator per row keeping multiple sources of policies together (assess)
  distinct(indicator_harmonized, meas, .keep_all = TRUE) %>%
  # remove source to avoid confusion
  dplyr::select(-source) %>% 
  write_csv('../output/policy_indicatorsMay24.csv')
  
policy_indic_cl = read_csv('../output/policy_indicatorsMay24.csv')

#unique indicators and categories
policy_indic_cl %>% distinct(indicator_harmonized) %>% count()#647 indicators
policy_indic_cl %>% distinct(Categories)#8 categories
policy_indic_cl %>% distinct(Categories, Subcategories) %>% count()#51 Subcategories
policy_indic_cl %>% distinct(indicator_harmonized, .keep_all = TRUE) %>% filter(assessment == 1) %>%  count() #194
194/647 #--> less that 30% used in assessments

# indicators usage
policy_indic_cl %>% filter(usage_policy>=3) %>% distinct(indicator_harmonized) %>% count() #1
policy_indic_cl %>% filter(usage_policy>=3) %>% distinct(indicator_harmonized)
policy_indic_cl %>% filter(usage_policy==2) %>% distinct(indicator_harmonized) %>% count() #53

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
  dplyr::select(indic_id, indicators, indicator_harmonized,meas) #%>% 
  #distinct(indicators, .keep_all = TRUE)
ramsar_indic = read.csv('../input/tables_extraction/ramsar_indicators.csv') %>% 
  mutate(meas = "RAMSAR") %>% 
  dplyr::select(indic_id, indicators, indicator_harmonized,meas) #%>% 
  #distinct(indicators, .keep_all = TRUE)
sdg_indic = read.csv('../input/tables_extraction/sdg_indicators.csv') %>% 
  mutate(meas = "SDG") %>% 
  mutate(Indicator = gsub("^\\S+ ", "", sdg_indicators)) %>% 
  dplyr::select(indic_id, indicators = Indicator, indicator_harmonized,meas) #%>% 

  #distinct(indicators, .keep_all = TRUE)
unccd_indic = read.csv('../input/tables_extraction/unccd_indicators.csv') %>% 
  mutate(meas = "UNCCD") %>% 
  dplyr::select(indic_id, indicators, indicator_harmonized, meas) #%>% 
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
  dplyr::select(indic_id, indicators = Indicator, indicator_harmonized,meas) #%>% 
  #distinct(indicators, .keep_all = TRUE)
iccwc_indic = read.csv('../input/tables_extraction/iccwc_indicators.csv') %>% 
  mutate(meas = "ICCWC") %>% 
  dplyr::select(indic_id, indicators, indicator_harmonized,meas) #%>% 
  #distinct(indicators, .keep_all = TRUE)
cms_indic = read.csv('../input/tables_extraction/cms_indicators.csv') %>% 
  mutate(meas = "CMS") %>% 
  dplyr::select(indic_id, indicators, indicator_harmonized,meas) #%>% 
  #distinct(indicators, .keep_all = TRUE)

policy_indic = rbind(cites_indic, ramsar_indic, sdg_indic, unccd_indic, gbf_indic, iccwc_indic,cms_indic)
policy_indic %>%  distinct(indicators) %>% count() #636
policy_indic %>%  distinct(indicator_harmonized) %>% count() #647

policy_indic_complete = policy_indic_cl %>%   
  # unsplit meas to get each policy source per row 
  mutate(meas = strsplit(as.character(meas), ",")) %>% 
  unnest(meas) %>% #702
  #join 
  left_join(policy_indic, by = c('indicator_harmonized', 'meas'))

#checks
policy_indic_complete %>%  distinct(indicators) %>% count() #636 (options are aggregated, as text from documents)
policy_indic_complete %>%  distinct(indicator_harmonized) %>% count() #647 (options are dis-aggregated, but there is more harmonization)
policy_indic_cl %>%  distinct(indicator_harmonized) %>% count() #647
#ALL GOOD

missing = policy_indic_cl %>%   
  # unsplit meas to get each policy source per row --> source
  mutate(meas = strsplit(as.character(meas), ",")) %>% 
  unnest(meas) %>% 
  anti_join(policy_indic, by = c('indicator_harmonized', 'meas'))
missing = policy_indic %>%   
  anti_join(mutate(policy_indic_cl,meas = strsplit(as.character(meas), ",")) %>% 
              unnest(meas), by = c('indicator_harmonized', 'meas'))
#ALL GOOD

policy_indic_clean = policy_indic_complete %>% 
  # fix an issue in harmonized indicators
  mutate(indicator_harmonized = gsub("south[‑]south","south-south",indicator_harmonized)) %>%
  # paste all indic_id together
  group_by(indicator_harmonized) %>%
  mutate(ids = paste0(indic_id,collapse = ",")) %>%
  ungroup() %>% 
  # paste all textual indicators together
  group_by(indicator_harmonized) %>%
  mutate(indicators_orig = paste0(indicators,collapse = ",")) %>%
  ungroup() %>% 
  # paste all meas sources together
  group_by(indicator_harmonized) %>%
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
  distinct(indicator_harmonized, .keep_all = TRUE) %>% 
  dplyr::select("indicator_harmonized","ids","mea",'ipbes',
                "Categories","Subcategories","Categories_2","Subcategories_2",
                "indicators_orig",
                ) %>% 
  write_csv('../output/policy_indicatorsMay24_orignames.csv')
  
#checks
policy_indic_clean %>%  distinct(indicators_orig) #619
policy_indic_clean %>%  distinct(indicator_harmonized) #646

dup = check_dup(policy_indic_clean,indicators_orig)
dup = check_dup(policy_indic_clean,indicator_harmonized)

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
  group_by(indicator_harmonized) %>%
  mutate(assess = paste0(source,collapse = ",")) %>%
  ungroup() %>%
  # keep each indicator per row keeping multiple sources of assess together (assess)
  distinct(indicator_harmonized, assess, .keep_all = TRUE) %>%
  # remove source to avoid confusion
  dplyr::select(-source) %>% 
  write_csv('../output/assessment_indicatorsMay24.csv')

assess_indic_cl %>% distinct(indicator_harmonized) %>% count()#1648
assess_indic_cl %>% distinct(indicator_harmonized, .keep_all = TRUE) %>% filter(policy == 1) %>% count()#194

# indicators usage
assess_indic_cl %>% filter(usage_assess>=4) %>% distinct(indicator_harmonized, assess)  #4
assess_indic_cl %>% filter(usage_assess==3) %>% distinct(indicator_harmonized, assess)  #10
assess_indic_cl %>% filter(usage_assess==2) %>% distinct(indicator_harmonized) %>% count() #44
assess_indic_cl %>% filter(usage_assess==1) %>% distinct(indicator_harmonized) %>% count() #1590

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
  group_by(indicator_harmonized) %>%
  mutate(ipbes_assess = paste0(source,collapse = ",")) %>%
  ungroup() %>%
  # keep each indicator per row keeping multiple sources of assess together (ipbes_assess)
  distinct(indicator_harmonized, ipbes_assess, .keep_all = TRUE) %>%
  # remove source to avoid confusion
  dplyr::select(-source) %>% 
  write_csv('../output/ipbes_assessment_indicatorsMay24.csv')

# summaries  
IPBES_indic_cl %>% distinct(indicator_harmonized) %>% count()#1085

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

IPBES_indic_cl %>% filter(usage_ipbes==4) %>% distinct(indicator_harmonized, ipbes_assess) #3
IPBES_indic_cl %>% filter(usage_ipbes==3) %>% distinct(indicator_harmonized, ipbes_assess) #3
IPBES_indic_cl %>% filter(usage_ipbes==2) %>% distinct(indicator_harmonized,ipbes_assess) %>%  count() #27
IPBES_indic_cl %>% filter(usage_ipbes==1) %>% distinct(indicator_harmonized,ipbes_assess) %>% count() #1052

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
  dplyr::distinct(indicator_harmonized, .keep_all = TRUE)  %>% 
  dplyr::select(indicator_harmonized, core_high, type_indicators)


# core and highlighted indicators NOT used
not_used = anti_join(indic_core_high,IPBES_indic_cl)
# if disaggregated this indicators is already included.
