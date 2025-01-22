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

# Load MEAS frameworks

gbf_f = read_csv('../input/mapping_to_gbf/gbf/meas_goals_gbf - gbf.csv') %>% 
  dplyr::select(id, goal_target_id, goal_target_name, IPBES_cf) %>% 
  filter(!is.na(id))

# Add CMS
cms_f =  read_csv('../input/mapping_to_gbf/cms/meas_goals_gbf - cms.csv') %>% 
  dplyr::select(id, goal, target, GBF_goals_targets)%>% 
  mutate(GBF_goals_targets = strsplit(as.character(GBF_goals_targets), "; ")) %>% 
  unnest(GBF_goals_targets)

# Add CITES
cites_f =  read_csv('../input/mapping_to_gbf/cites/meas_goals_gbf - cites.csv') %>% 
  dplyr::select(id, goal, target = objective, GBF_goals_targets) %>% 
  mutate(GBF_goals_targets = strsplit(as.character(GBF_goals_targets), "; ")) %>% 
  unnest(GBF_goals_targets)

# Add RAMSAR
ramsar_f =  read_csv('../input/mapping_to_gbf/ramsar/meas_goals_gbf - ramsar.csv') %>% 
  dplyr::select(id, goal, target, GBF_goals_targets) %>% 
  mutate(GBF_goals_targets = strsplit(as.character(GBF_goals_targets), "; ")) %>% 
  unnest(GBF_goals_targets)


# Join by MEAs by GBF_goals_targets

cms_gbf_f = gbf_f %>% 
  inner_join(cms_f, by = c('goal_target_id' = 'GBF_goals_targets')) %>% 
  rename(gbf_id = id.x, mea_id = id.y) %>% 
  mutate(mea = if_else(!is.na(mea_id),
                      true = 'cms',false = NA)) %>% 
  dplyr::select(-goal, -target) 

ramsar_gbf_f = gbf_f %>% 
  inner_join(ramsar_f, by = c('goal_target_id' = 'GBF_goals_targets')) %>% 
  rename(gbf_id = id.x, mea_id = id.y) %>% 
  mutate(mea = if_else(!is.na(mea_id),
                       true = 'ramsar',false = NA)) %>% 
  dplyr::select(-goal, -target) 

cites_gbf_f = gbf_f %>% 
  inner_join(cites_f, by = c('goal_target_id' = 'GBF_goals_targets')) %>% 
  rename(gbf_id = id.x, mea_id = id.y) %>% 
  mutate(mea = if_else(!is.na(mea_id),
                       true = 'cites',false = NA)) %>% 
  dplyr::select(-goal, -target) 

meas_gbf_f = rbind(cms_gbf_f,ramsar_gbf_f,cites_gbf_f)

gbf_only = anti_join(gbf_f,meas_gbf_f, by = 'goal_target_name') %>% 
  filter(!goal_target_id %in% c('GA','GB','GD')) %>% 
  rename(gbf_id = id) %>% 
  mutate(mea_id = NA) %>% 
  mutate(mea = 'only GBF')

meas_gbf_f = rbind(meas_gbf_f,gbf_only)


policy_categories = meas_gbf_f %>%   
  # summary cat + source
  group_by(goal_target_id,mea)  %>% 
  count() %>% 
  mutate(mea = factor(mea,levels = c("only GBF","cms","cites","ramsar"))) %>% 
  mutate(goal_target_id = factor(goal_target_id,
                                    levels = c("GC","T1","T2","T3","T4","T5","T6","T7","T8","T9","T10",
                                              "T11","T12","T13","T14","T15","T16","T17","T18","T19",
                                              "T20","T21","T22","T23")))


ggplot(policy_categories,
       aes(y = n,
           axis1 = mea, axis2 = goal_target_id)) +
  geom_alluvium(aes(fill = mea),
                width = 1/6, knot.pos = 0, reverse = FALSE) +
  scale_fill_manual(values =c('cms'="#46327e",
                              'cites'="#277f8e",
                              'ramsar'="#4ac16d"
                              #'Knowledge Systems'="#fde725"
                              )) +
  guides(fill = "none") +
  geom_stratum(alpha = .1, width = 1/6, reverse = FALSE) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)),
            reverse = FALSE) +
  scale_x_continuous(breaks = 1:2, labels = c("MEAS","GBF")) +
  theme_void()

# Load indicators from MEAs----

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
  mutate(indicator_name = gsub('None adopted','',`Indicator name`))
write_csv(gbf_2025, '../input/mapping_to_gbf/gbf/gbf_indicators.csv')

gbf_2025 = gbf_2025 %>% 
  # Filter out indicators that do not exist
  filter(`Indicator name` != 'None adopted') %>% 
  # harmonize indicators
  dplyr::mutate(indicators_h = tolower(`Indicator name`)) %>%
  dplyr::mutate(indicators_h = str_trim(indicators_h)) %>% 
  harmonize_indic(indicators_h) %>% 
  dplyr::rename(indicators_harmonized = field_harm) %>% 
  dplyr::mutate(indicators_harmonized = str_trim(indicators_harmonized)) %>% 
  dplyr::select(-indicators_h, -indicator_name) %>% 
  dplyr::rename(indicators_orig = `Indicator name`)
write_csv(gbf_2025, '../input/tables_extraction/km_gbf_indicators2025.csv')
gbf_2025 = read_csv('../input/tables_extraction/km_gbf_indicators2025.csv')


gbf_2025 %>%  group_by(indicator_type) %>% count()
gbf_2024 %>%  group_by(indicator_type) %>% count()

mismatch1 = anti_join(gbf_2024, gbf_2025, by = 'indicators_harmonized')
mismatch2 = anti_join(gbf_2025,gbf_2024, by = 'indicators_harmonized')
# SOME NEW INDICATORS BUT MOSTLY DIFFERENT NAMING --> use 2025 data


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
write_csv(meas_classified, '../input/gbf_framework/meas_indic_classified_gbf.csv')

