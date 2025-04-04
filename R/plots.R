# clean you environment
rm(list=ls())

############################################
# Plot all indicators (MEAs and assessments)
############################################
# Created by Yanina Sica in April 2025


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
library(patchwork)

# Load indicators----

indic =  read_csv("../output/all_harmonized_classified_indicatorsMay24.csv")

# Merge all IPBES assessments (sources2) and remove ICCWC
indic = indic %>% 
  dplyr::mutate(source = strsplit(as.character(sources), ",")) %>% 
  tidyr::unnest(source) %>% 
  dplyr::filter(source != 'ICCWC') %>% 
  dplyr::mutate(source2 = if_else(source %in% c('VA', 'IAS','GA','SUA'),
                                  true = 'IPBES',
                                  false = source)) %>% 
  dplyr::distinct(indicators_harmonized, source2, .keep_all = TRUE) %>% 
  dplyr::group_by(indicators_harmonized) %>% 
  dplyr::mutate(sources2 = str_flatten(source2, ",")) %>% 
  dplyr::distinct(indicators_harmonized, sources2, .keep_all = TRUE) %>% 
  dplyr::select(-source,-source2) %>% 
  relocate(sources2, .after = sources) %>% 
  dplyr::ungroup()
  
# Checks
indic %>% filter(!is.na(indicators_harmonized)) %>% distinct(indicators_harmonized) %>% count() #2051 unique indicators
indic %>% filter(!is.na(indicators_harmonized)) %>% filter(!is.na(Categories)) %>% distinct(indicators_harmonized) %>% count() #2101 unique indicators categorized


# 1-Summaries of indicators by source----

indic %>% 
  mutate(source = strsplit(as.character(sources2), ",")) %>% 
  unnest(source) %>% 
  group_by(source) %>% count() %>% arrange(desc(n))
# source     n
# 1 IPBES   1085
# 2 IPCC     374
# 3 GBF      307
# 4 SDG      257
# 5 GEO      223
# 6 CITES     41
# 7 CMS       25
# 8 RAMSAR    18
# 9 UNCCD      4

# Assessments
indic %>% filter(assessment ==1) %>% distinct(indicators_harmonized) %>% count() #1648

# MEAs
indic %>% filter(policy ==1) %>% distinct(indicators_harmonized) %>% count() #647

# Figure 1: Summaries of indicators by source----
data_hist_assess = indic %>% 
  mutate(source = strsplit(as.character(sources2), ",")) %>% 
  unnest(source) %>% 
  filter(assessment ==1) %>%
  filter(!source %in% c('GBF','SDG','UNCCD','CITES','CMS','RAMSAR' )) %>% 
  group_by(source) %>%  count() %>% arrange(n)

hist_assess = ggplot(data=data_hist_assess, aes(x=source, y=n, fill=source)) +
  geom_bar(stat="identity", color="black") + 
  labs(y="Number of metrics", x = "") +
  scale_fill_brewer(palette="Greens", limits=c("IPBES", "IPCC","GEO" ),name = '') +
  scale_x_discrete(limits=c("IPBES", "IPCC", "GEO")) +
  scale_y_continuous(limits = c(0,1200), expand = expansion(mult = c(0, .1))) +
  theme_minimal() +
  theme(legend.position = "none", 
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid = element_blank(),
        panel.border = element_blank())


data_hist_meas = indic %>% 
  mutate(source = strsplit(as.character(sources2), ",")) %>% 
  unnest(source) %>% 
  filter(policy ==1) %>%
  filter(source %in% c('GBF','SDG','CITES','CMS','RAMSAR','UNCCD' )) %>% 
  group_by(source) %>% count() %>% arrange(desc(n))

hist_meas = ggplot(data=data_hist_meas, aes(x=source, y=n, fill=source,legend = FALSE )) +
  geom_bar(stat="identity", color="black") + 
  labs(y="Number of metrics", x = "") +
  scale_fill_brewer(palette="Blues", limits=c('GBF','SDG','CITES','CMS','RAMSAR','UNCCD'),name = '') +
  scale_x_discrete(limits=c('GBF','SDG','CITES','CMS','RAMSAR','UNCCD')) +
  scale_y_continuous(limits = c(0,310), expand = expansion(mult = c(0, .1))) +
  theme_minimal() +
  theme(legend.position = "none", 
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid = element_blank(),
        panel.border = element_blank())

hist_meas + hist_assess

# 2-Summaries of shared indicators ----

indic %>% filter(usage >= 2) %>% distinct(indicators_harmonized) %>% count() #235
(indic %>% filter(usage >= 2) %>% distinct(indicators_harmonized) %>% count() * 100) / indic  %>% distinct(indicators_harmonized) %>% count() #11.18

# common indicators
indic %>% arrange(desc(usage)) %>% distinct(indicators_harmonized, .keep_all = TRUE) %>%  
  dplyr::select(indicators_harmonized, sources2) %>% head()
# red list index                                                               IPBES,GEO,GBF,SDG      
# proportion of fish stocks within biologically sustainable levels             IPBES,GEO,GBF,SDG      
# proportion of agricultural area under productive and sustainable agriculture IPBES,GEO,GBF,SDG      
# forest area as a percentage of total land area                               IPBES,GEO,GBF,SDG      
# proportion of land that is degraded over total land area                     IPBES,GEO,GBF,SDG,UNCCD
# index of coastal eutrophication potential (icep)                             IPBES,GEO,GBF,SDG

# Get matrix of shared indicators
# ## matrix
# Define a comprehensive list of all treaties and assessments
all_entities <- c("CITES", "CMS", "RAMSAR", "SDG", "UNCCD", "GBF", "GEO", "IPBES", "IPCC")

# Initialize an empty matrix with all entities
chord_matrix <- matrix(0, nrow = length(all_entities), ncol = length(all_entities),
                       dimnames = list(all_entities, all_entities))

# Fill the matrix with counts of connections between sources
for (i in 1:nrow(indic)) {
  sources <- unique(unlist(strsplit(indic$sources2[i], ",")))
  sources <- intersect(sources, all_entities) 
  if (length(sources) > 1) { # this is the problem
    pairs <- combn(sources, 2)
    for (j in 1:ncol(pairs)) {
      chord_matrix[pairs[1, j], pairs[2, j]] <- chord_matrix[pairs[1, j], pairs[2, j]] + 1
      chord_matrix[pairs[2, j], pairs[1, j]] <- chord_matrix[pairs[2, j], pairs[1, j]] + 1
    }
  }
}

# Fig 2: links----

# Split sources into a list
indic$sources2 <- strsplit(as.character(indic$sources2), ",")

# Create a new data frame for links between treaties and assessments
treaties <- c("GBF", "SDG", "CITES", "CMS", "RAMSAR", "UNCCD")
assessments <- c("IPBES", "GEO", "IPCC")

# Initialize a matrix to store counts
link_matrix <- matrix(0, nrow = length(treaties), ncol = length(assessments),
                      dimnames = list(treaties, assessments))

# Fill the matrix with counts of links
for (i in 1:nrow(indic)) {
  sources <- indic$sources2[[i]]
  for (source in sources) {
    if (source %in% treaties) {
      for (assessment in assessments) {
        if (grepl(assessment, indic$sources2[i], fixed = TRUE)) {
          link_matrix[source, assessment] <- link_matrix[source, assessment] + 1
        }
      }
    }
  }
}

# Create a complete data frame including zero links
links <- as.data.frame(as.table(link_matrix))
colnames(links) <- c("treaties", "assessments", "Freq")



# Define a color scheme for the different entities
grid_color <- c(
  IPBES = "#E5F5E0", IPCC = "#A1D99B", GEO = "#31A354",
  GBF = "#EFF3FF", SDG = "#C6DBEF", 
  CITES = "#FFC107", CMS = "#FFC107", RAMSAR = "#FFC107",
  UNCCD = "#08519C"
)
# library(RColorBrewer)
# display.brewer.all()
# brewer.pal(n=6,"Blues")
# "#EFF3FF": GBF
# "#C6DBEF": SDG
# "#9ECAE1": CITES
# "#6BAED6": CMS
# "#3182BD":RAMSAR
# "#08519C": UNCCD
# brewer.pal(n=3,"Greens")
# "#E5F5E0": IPBES
# "#A1D99B": IPCC
# "#31A354":GEO


# Define the order of entities where treaties are at the top and assessments at the bottom
entity_order <- c("IPBES", "IPCC", "GEO","GBF","SDG","CITES", "CMS", "RAMSAR", "UNCCD")

# Set plot parameters and create the chord diagram with specified order
circos.clear()
circos.par(start.degree = 173, track.margin = c(0.01, 0.01), gap.after = 5,
           cell.padding = c(0, 0, 0, 0))

# Create the chord diagram
chordDiagram(
  links,
  order = entity_order,
  transparency = 0.1,
  annotationTrack = c("grid"),
  preAllocateTracks = 1,
  grid.col = grid_color
)

# Add labels to the sectors
circos.trackPlotRegion(
  track.index = 1, panel.fun = function(x, y) {
    circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index,
                facing = "inside", niceFacing = TRUE, adj = c(0.5, 0))
  },
  bg.border = NA
)

# Finalize and clear the circos plot
circos.clear()

# 3.c-Summaries of indicators by categories----
indic %>% group_by(Categories) %>% count() %>% arrange(desc(n))
# Categories             n
# 1 Ecosystems           568
# 2 Biodiversity         269
# 3 Governance           263
# 4 Direct Drivers       246
# 5 Human Assets         218
# 6 Ecosystem Services   198
# 7 Human Well-Being     180
# 8 Knowledge Systems    159

263/ indic %>% distinct(indicators_harmonized) %>% count() * 100

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
