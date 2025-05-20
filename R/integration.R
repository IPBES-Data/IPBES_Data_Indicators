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
all_indicators %>% count() #1829
all_indicators %>% distinct(indicator_harmonized) %>% count() #1829 unique

all_indicators %>% group_by(sources) %>% count()
all_indicators %>% group_by(policy) %>% count()
all_indicators %>% filter(is.na(indic_ids))

write_csv(all_indicators,'../input/all_indicators_20052025.csv')

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
changes2 = anti_join(all_indicators,classified, by = 'indicator_harmonized')#0 indicator from last extraction were not classified

all_indicators_cl %>% filter(!is.na(Categories)) %>% count() #1829 classified
all_indicators_cl %>% filter(is.na(Categories)) %>% count() #0 NOT classified yet

# save
write_csv(dplyr::select(all_indicators_cl, -classif),'../output/all_indicators_classified_20052025.csv')
#all_indicators_cl = read_csv('../output/all_indicators_classified_14052025.csv')

# checks
check_dup(all_indicators_cl,indicator_harmonized)

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

write_csv(indic,'../output/all_indicators_classified_20052025_lv.csv')
#indic = read_csv('../output/all_indicators_classified_19052025_lv.csv')

# 3-Summaries (Result)----

all_indicators_cl %>% count() #1829 unique indicators
all_indicators_cl %>%  distinct(indicator_harmonized) %>% count() # all unique
all_indicators_cl %>% filter(!is.na(Categories)) %>% distinct(indicator_harmonized) %>% count() # all classified


all = indic %>% count() #2098 indicators
unique = indic %>%  distinct(indicator_harmonized) %>% count() # 1829 unique
cat('all indicators: ',all$n, '\nunique indicators: ',unique$n)

# 3.a-Summaries of indicators by source----

# MEAs vs asses
all_mea = indic %>% filter(mea==TRUE) %>% count() %>% arrange(desc(n))
all_assess = indic %>% filter(assess==TRUE) %>% count() %>% arrange(desc(n))

unique_mea = indic %>% filter(mea == TRUE) %>% distinct(indicator_harmonized, .keep_all = TRUE) %>% 
  count() 
unique_assess = indic %>% filter(assess == TRUE) %>% distinct(indicator_harmonized, .keep_all = TRUE) %>% 
  count() 

cat('\nunique assess indicators: ',unique_assess$n, '\nunique mea indicators: ',unique_mea$n)

# sources (extracted with duplicates)
total_by_source = indic %>% group_by(source) %>% count() %>% arrange(desc(n))
# source     n
# 1 IPBES    797
# 2 IPCC     376
# 3 GBF      311
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
# FALSE         1620 have only 1 source
# TRUE           209 have multiple sources
unique$n
cat('count of indicators re-used:', multiple_use[2,2]$n,
    '\nprop of indicators reused: ',multiple_use[2,2]$n/unique$n,
    '(',multiple_use[2,2]$n, 'out of ', unique$n, ')')

fr_indic = indic %>% 
  group_by(indicator_harmonized) %>% 
  count() %>% arrange(desc(n)) %>% 
  filter(n>1) %>% 
  left_join(distinct(indic,indicator_harmonized, .keep_all = TRUE))

fr_indic[,1:3]

#### Co_occurrence_matrix in plots.R-----
co_occurrence_matrix = read.csv('../output/all_indicators_matrix_19052025.csv')
#          IPBES GEO IPCC GBF SDG CITES CMS RAMSAR UNCCD
# 1  IPBES   607  29    2  80 138     0   0      0     1
# 2    GEO    29 177    5  22  13     0   0      0     0
# 3   IPCC     2   5  369   0   0     0   0      0     0
# 4    GBF    80  22    0 218  48     0   0      0     1
# 5    SDG   138  13    0  48  95     0   0      0     1
# 6  CITES     0   0    0   0   0    52   0      0     0
# 7    CMS     0   0    0   0   0     0  25      0     0
# 8 RAMSAR     0   0    0   0   0     0   0     65     0
# 9  UNCCD     1   0    0   1   1     0   0      0    122


# assess --> MEAS
IPBES_usage_MEAS = 80+138+1
GEO_usage_MEAS = 22+13
IPCC_usage_MEAs = 0
cat( 'IPBES reuse in MEAS: ',IPBES_usage_MEAS, '\nGEO reuse in MEAS:',GEO_usage_MEAS, 
     '\nProp of MEAs indic coming from assess:',(IPBES_usage_MEAS + GEO_usage_MEAS)/658,
     '\nAssess indic used in MEAS: ', (IPBES_usage_MEAS + GEO_usage_MEAS))

# MEAs
SDG_reuse = (138+13+48+1)/total_by_source[4,]$n
SDG_reuseage_MEAs = (48+1)/total_by_source[4,]$n
SDG_reuseage_assess = (138+13)/total_by_source[4,]$n
cat( 'SDG reuse: ',SDG_reuse, '(',(138+13+48+1),'out of',total_by_source[4,]$n,')','\nmeas:',SDG_reuseage_MEAs, '\nassess:',SDG_reuseage_assess )

GBF_reuse = (80+22+48+1)/total_by_source[3,]$n
GBF_reuseage_MEAs = (48+1)/total_by_source[3,]$n
GBF_reuseage_assess = (80+22)/total_by_source[3,]$n
cat( 'GBF reuse: ',GBF_reuse, '(',(80+22+48+1),'out of',total_by_source[3,]$n,')', '\nmeas:',GBF_reuseage_MEAs, '\nassess:',GBF_reuseage_assess )


#### Fig 2 in plots.R-----

# Usage of indicators from assess --> MEAS
IPBES_usage_MEAS = 80+138+1
GEO_usage_MEAS = 22+13
IPCC_usage_MEAs = 0
cat( 'IPBES reuse in MEAS: ',IPBES_usage_MEAS, '\nGEO reuse in MEAS:',GEO_usage_MEAS, 
     '\nAssess indic used in MEAS: ', (IPBES_usage_MEAS + GEO_usage_MEAS),
     '\nProp of MEAs indic coming from assess:',(IPBES_usage_MEAS + GEO_usage_MEAS)/unique_mea$n
     )

#### Fig 3 in plots.R-----
#### Fig 3 alternative in plots.R-----

# 3.c-Summaries of indicators by category-----

all_indicators_cl %>% distinct(Categories) %>% count() #8

(all_indicators_cl %>% filter(!is.na(Categories_2)) %>% count())/ #409
  (all_indicators_cl %>% filter(!is.na(Categories)) %>% count())  #1829
  
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
indic_categories = all_indicators_cl %>% 
  dplyr::group_by(Categories) %>% 
  dplyr::count() %>% arrange(desc(n)) %>% 
  dplyr::mutate(Categories = gsub('ecosystems', 'Ecosystems', Categories)) %>% 
  dplyr::mutate(Categories = gsub('biodiversity', 'Biodiversity', Categories)) %>% 
  dplyr::mutate(`Proportion of metrics` = (n/unique$n)*100) %>% 
  dplyr::mutate(Elements = factor(Categories, 
                                  levels=c('Biodiversity','Ecosystems','Ecosystem services','Human well-being',
                                           'Direct drivers','Human assets','Knowledge systems','Governance'))) %>% 
  dplyr::arrange(Elements) %>%
  dplyr::select(Elements,`Number of metrics`=n,`Proportion of metrics`) %>% 
  write_csv('../output/sup_material/indic_categories_190525.csv')

indic_categories_bysource = indic %>%   
  # summary cat + source
  dplyr::group_by(source, Categories)  %>% 
  dplyr::count() %>% arrange(desc(n)) %>% 
  # improve viz
  dplyr::mutate(Categories = gsub('ecosystems', 'Ecosystems', Categories)) %>% 
  dplyr::mutate(Categories = gsub('biodiversity', 'Biodiversity', Categories)) %>% 
  dplyr::select("Elements" = Categories, Source = source, 'Counts of metrics' = n) %>% 
  dplyr::mutate(Elements = factor(Elements, 
                             levels=c('Biodiversity','Ecosystems','Ecosystem services','Human well-being',
                                      'Direct drivers','Human assets','Knowledge systems','Governance'))) %>% 
  arrange(Elements) %>%
  dplyr::mutate(Source = factor(Source, 
                         levels=c("IPBES","IPCC","GEO",'GBF','SDG','RAMSAR','CITES','CMS','UNCCD'))) %>%
  arrange(Source) %>% 
  # set wide format
  tidyr::pivot_wider(names_from = Source, values_from = `Counts of metrics`) %>% 
  write_csv('../output/sup_material/indic_categories_bysource_190525.csv')

indic_subcategories_bysource = indic %>%   
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
  arrange(Elements) %>% 
  dplyr::mutate(Source = factor(Source, 
                                levels=c("IPBES","IPCC","GEO",'GBF','SDG','RAMSAR','CITES','CMS','UNCCD'))) %>%
  arrange(Source) %>% 
  # set wide format
  tidyr::pivot_wider(names_from = Source, values_from = `Counts of metrics`) %>% 
  write_csv('../output/sup_material/indic_subcategories_bysource_190525.csv')

#### Fig 4 in plots.R-----

## 4.a-Policy indicators-----

indic_policy = indic %>% 
  filter(mea == TRUE) %>% 
  mutate(sources = gsub('SDG[;]IPBES[;]GEO$','SDG', sources),
         sources = gsub('GBF[;]IPBES[;]GEO$','GBF', sources),
         sources = gsub('GBF[;]SDG[;]IPBES$','GBF;SDG', sources),
         sources = gsub('GBF[;]SDG[;]IPBES[;]GEO$','GBF;SDG', sources),
         sources = gsub('GBF[;]SDG[;]UNCCD[;]IPBES$','GBF;SDG;UNCCD', sources),
         sources = gsub('SDG[;]GEO$','SDG', sources),
         sources = gsub('SDG[;]IPBES$','SDG', sources),
         sources = gsub('GBF[;]IPBES$','GBF', sources),
         sources = gsub('GBF[;]GEO$','GBF', sources)) 

# remove duplicates
policy_indicators_cl = indic_policy %>% 
  distinct(indicator_harmonized, .keep_all = TRUE) %>% 
  dplyr::select(-source,-"mea",-"assess")

## Table 1: Policy indicators-----

policy_indicators_cl %>% 
  group_by(sources) %>% 
  count() %>% 
  left_join(total_by_source, by= c('sources'='source')) %>% 
  rename('n'='n.x', 'n_total'='n.y') %>% 
  arrange(desc(n))
#   sources           n n_total
# 1 GBF             263     311
# 2 SDG             192     240
# 3 RAMSAR           65      65
# 4 CITES            52      52
# 5 GBF;SDG          47      NA
# 6 CMS              25      25
# 7 UNCCD            12      13
# 8 GBF;SDG;UNCCD     1      NA

#unique indicators 
policy_indicators_cl %>% count()#657 unique indicators
indic_policy %>% count() # 706 indicators

# categories

policy_indicators_cl %>% distinct(Categories)#8 categories
policy_indicators_cl %>% distinct(Categories,Subcategories) %>% count()#44 Subcategories (ILK is missing)

policy_indicators_cl %>% group_by(Categories) %>% count() %>%  View()
policy_indicators_cl %>% group_by(Categories,Subcategories) %>% count() %>%  View()


## 4.b- Assessment indicators-----

indic_assess = indic %>% 
  filter(assess == TRUE) %>% 
  mutate(sources = gsub('GBF[;]GEO$','GEO', sources),
         sources = gsub('GBF[;]IPBES$','IPBES', sources),
         sources = gsub('GBF[;]IPBES[;]GEO$','IPBES;GEO', sources),
         sources = gsub('GBF[;]SDG[;]IPBES$','IPBES', sources),
         sources = gsub('GBF[;]SDG[;]IPBES[;]GEO$','IPBES;GEO', sources),
         sources = gsub('GBF[;]SDG[;]UNCCD[;]IPBES$','IPBES', sources),
         sources = gsub('SDG[;]GEO$','GEO', sources),
         sources = gsub('SDG[;]IPBES$','IPBES', sources),
         sources = gsub('SDG[;]IPBES[;]GEO$','IPBES;GEO', sources)) 
#indic_assess %>%  group_by(sources) %>% count()

# remove duplicates
assess_indicators_cl = indic_assess %>% 
  distinct(indicator_harmonized, .keep_all = TRUE) %>% 
  dplyr::select(-source,-"mea",-"assess")

## Table 2: Assess indicators-----

assess_indicators_cl %>% 
  group_by(sources) %>% 
  count() %>% 
  left_join(total_by_source, by= c('sources'='source')) %>% 
  rename('n'='n.x', 'n_total'='n.y') %>% 
  arrange(desc(n))
# sources        n       n_total
# 1 IPBES        766     797
# 2 IPCC         369     376
# 3 GEO          185     219
# 4 IPBES;GEO     29      NA
# 5 IPCC;GEO       5      NA
# 6 IPBES;IPCC     2      NA

#unique indicators 
assess_indicators_cl %>% count()# 1356 unique indicators
indic_assess %>% count() #1392  indicators

# categories

assess_indicators_cl %>% distinct(Categories)#8 categories
assess_indicators_cl %>% distinct(Categories,Subcategories) %>% count()#43 Subcategories (Restoration efforts and Cultural diversity is missing)
#assess_indicators_cl %>% distinct(Categories,Subcategories) %>% View()
indic_assess %>% filter(source == 'IPCC') %>% group_by(Categories) %>% count()#8 categories

assess_indicators_cl %>% group_by(Categories) %>% count() %>% 
assess_indicators_cl %>% group_by(Categories,Subcategories) %>% count() %>%  View()


## 5.a-Indicators in IPBES assessments-----
IPBES_indic = indic %>% 
  # get only indicators used in IPBES
  filter(source == 'IPBES') %>%
  # unsplit sources to get each IPBES source per row --> source
  mutate(indic_ids = strsplit(as.character(indic_ids), ";")) %>% 
  unnest(indic_ids) %>% 
  #remove duplicated indicators from other sources
  mutate(extra = word(indic_ids, start =1,end =1, sep = '_')) %>% 
  filter(extra == 'IPBES') %>% 
  mutate(subsource = word(indic_ids, start =2,end =2, sep = '_')) %>% 
  dplyr::select(-sources, -classif, -source, -mea, -assess) %>% 
  filter(subsource %in% c('VA','SUA','GA','IAS'))

IPBES_indicators_cl = IPBES_indic %>% 
  dplyr::group_by(indicator_harmonized) %>% 
  dplyr::mutate(subsources = paste0(subsource, collapse = ";")) %>%
  ungroup() %>% 
  distinct(indicator_harmonized, .keep_all = TRUE) %>% 
  mutate(subsources = gsub('GA[;]GA[;]GA[;]GA','GA', subsources)) %>% 
  mutate(subsources = gsub('GA[;]GA[;]GA','GA', subsources)) %>% 
  mutate(subsources = gsub('GA[;]GA','GA', subsources)) %>% 
  mutate(subsources = gsub('SUA[;]SUA[;]SUA','SUA', subsources)) %>% 
  mutate(subsources = gsub('SUA[;]SUA','SUA', subsources)) %>% 
  mutate(subsources = gsub('VA[;]VA[;]VA','SUA', subsources)) %>% 
  mutate(subsources = gsub('VA[;]VA','SUA', subsources)) %>% 
  mutate(subsources = gsub('IAS[;]IAS','IAS', subsources)) %>% 
  mutate(subsources = gsub('GA[;]SUA[;]SUA[;]VA[;]IAS[;]IAS','GA;SUA;VA;IAS', subsources)) %>%  
  mutate(subsources = gsub('GA[;]SUA[;]SUA[;]VA','GA;SUA;VA', subsources)) %>% 
  mutate(subsources = gsub('GA[;]SUA[;]IAS[;]IAS[;]SUA','GA;SUA;IAS', subsources)) 
  
IPBES_indicators_cl %>%  count()
IPBES_indic %>%  count()

IPBES_indic %>% 
  group_by(subsource) %>% 
  count() %>% 
  mutate(prop = n/(IPBES_indic %>%  count())*100) %>% 
  arrange(desc(n))

IPBES_indicators_cl %>% distinct(subsources)

IPBES_indicators_cl %>% distinct(Categories)
IPBES_indic %>% group_by(Categories,subsource) %>% count() %>%  View()
IPBES_indic %>% group_by(Categories,subsource, Subcategories) %>% count() %>%  View()

IPBES_categories = IPBES_indic %>%   
  group_by(subsource,Categories)  %>% 
  count() %>% arrange(desc(n)) %>% 
  # improve viz
  mutate(Categories = gsub('ecosystems', 'Ecosystems', Categories)) %>% 
  mutate(Categories = gsub('biodiversity', 'Biodiversity', Categories)) %>% 
  mutate(Categories = gsub('Human well-being', 'Human\n well-being', Categories)) %>% 
  mutate(Categories = gsub('Knowledge systems', 'Knowledge\n systems', Categories)) %>% 
  mutate(Categories = gsub('Ecosystem services', 'Ecosystem\n services', Categories)) %>% 
  mutate(Categories = gsub('Direct drivers', 'Direct\n drivers', Categories)) %>% 
  mutate(Categories = gsub('Human assets', 'Human\n assets', Categories)) %>% 
  mutate(source = factor(source, 
                         levels=c("GA","SUA","IAS",'VA'))) %>% 
  mutate(Categories = factor(Categories, 
                             levels=c('Biodiversity','Ecosystems','Ecosystem\n services','Human\n well-being',
                                      'Direct\n drivers','Human\n assets','Knowledge\n systems','Governance'))) 


ipbes = ggplot(IPBES_categories,
       aes(y = n,
           axis1 = subsource, axis2 = Categories)) +
  geom_alluvium(aes(fill = Categories),
                width = 1/6, knot.pos = 0, reverse = FALSE) +
  scale_fill_manual(values =c('Governance'="#440154",'Knowledge\n systems'= "#46327e",
                              'Human\n assets'="#365c8d",'Direct\n drivers'="#277f8e",
                              'Human\n well-being'="#1fa187",'Ecosystem\n services'="#4ac16d" ,
                              'Ecosystems'="#a0da39",'Biodiversity'="#fde725")) +
  guides(fill = "none") +
  geom_stratum(alpha = .1, width = 1/6, reverse = FALSE) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)),
            reverse = FALSE) +
  scale_x_continuous(breaks = 1:2) +
  theme_void()

ipbes

ggsave(file="../output/sup_material/ipbes_200525.svg", plot=ipbes, 
       width=7, height=8, units = "in", dpi = 300)
ggsave(file="../output/sup_material/ipbes_200525.png", plot=ipbes, 
       width=7, height=8, units = "in", dpi = 300)

#ILK
IPBES_indicators_cl %>%   
  filter(Subcategories =="ILK")

## 6-Abstract-----
cat('unique indicators: ',unique$n)
cat('unique assess indicators: ',unique_assess$n, '\nunique mea indicators: ',unique_mea$n)
cat('count of indicators re-used:', multiple_use[2,2]$n,
    '\nprop of indicators reused: ',multiple_use[2,2]$n/unique$n,
    '(',multiple_use[2,2]$n, 'out of ', unique$n, ')')
total_by_source
GBF_reuse = (80+22+48+1)/total_by_source[3,]$n
GBF_reuseage_MEAs = (48+1)/total_by_source[3,]$n
GBF_reuseage_assess = (80+22)/total_by_source[3,]$n
cat( 'GBF reuse: ',GBF_reuse, '(',(80+22+48+1),'out of',total_by_source[3,]$n,')', '\nmeas:',GBF_reuseage_MEAs, '\nassess:',GBF_reuseage_assess )
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

# ### 5.b-IPBES core and highlighted indicators (D&K TF 2017-2018)-----
# indic_core_high = read_csv('../input/tables_extraction/ipbes_core_high_indicators.csv') %>% 
#   # add source
#   dplyr::mutate(core_high = 1) %>% 
#   dplyr::distinct(indicator_harmonized, .keep_all = TRUE)  %>% 
#   dplyr::select(indicator_harmonized, core_high, type_indicators)
# 
# 
# # core and highlighted indicators NOT used
# not_used = anti_join(indic_core_high,IPBES_indic_cl)
# # if disaggregated this indicators is already included.
