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
library(reshape2)

# Load indicators----

all_indicators_cl = read_csv('../output/all_indicators_classified_20052025.csv')

# long format with duplication
indic = read_csv('../output/all_indicators_classified_20052025_lv.csv')

# Figure 1: Summaries of indicators by source----

data_hist_meas = indic %>% 
  filter(mea == TRUE) %>%
  #filter(source %in% c('GBF','SDG','CITES','CMS','RAMSAR','UNCCD' )) %>% 
  group_by(source) %>% count() %>% arrange(desc(n))

hist_meas = ggplot(data=data_hist_meas, aes(x=source, y=n, fill=source,legend = FALSE )) +
  geom_bar(stat="identity", color="black") + 
  labs(y="Number of metrics", x = "") +
  scale_fill_brewer(palette="Blues", limits=c('GBF','SDG','RAMSAR','CITES','CMS','UNCCD'),name = '') +
  # order high to low
  scale_x_discrete(limits=c('GBF','SDG','RAMSAR','CITES','CMS','UNCCD')) +
  scale_y_continuous(limits = c(0,320), expand = expansion(mult = c(0, .1))) +
  theme_minimal() +
  labs(tag = "A") +
  theme(legend.position = "none", 
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.tag = element_text(),
        plot.tag.position = c(0.9, 0.9))

hist_meas 

data_hist_assess = indic %>% 
  filter(mea == FALSE) %>%
  #filter(!source %in% c('GBF','SDG','UNCCD','CITES','CMS','RAMSAR' )) %>% 
  group_by(source) %>%  count() %>% arrange(n)

hist_assess = ggplot(data=data_hist_assess, aes(x=source, y=n, fill=source)) +
  geom_bar(stat="identity", color="black") + 
  labs(y="Number of metrics", x = "") +
  scale_fill_brewer(palette="Greens", limits=c("IPBES", "IPCC","GEO" ),name = '') +
  # order high to low
  scale_x_discrete(limits=c("IPBES", "IPCC", "GEO")) +
  scale_y_continuous(limits = c(0,820), expand = expansion(mult = c(0, .1))) +
  theme_minimal() +
  labs(tag = "B") +
  theme(legend.position = "none", 
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid = element_blank(),
        panel.border = element_blank(),        
        plot.tag = element_text(),
        plot.tag.position = c(0.9, 0.9))

hist_assess


plot = hist_meas + hist_assess


ggsave(file="../output/figures/fig1_190525.svg", plot=plot, 
       width=7, height=3.5, units = "in", dpi = 300)
ggsave(file="../output/figures/fig1_190525.png", plot=plot, 
       width=7, height=3.5, units = "in", dpi = 300)

# Figure 2: Summaries of shared indicators ----

## Get matrix of shared indicators

# Split the 'source' column into separate lists
source_list <- strsplit(as.character(all_indicators_cl$sources), ";")

# Create a vector of unique sources
#unique_sources <- sort(unique(unlist(source_list)))
sources_ordered <- c("IPBES","GEO","IPCC","GBF","SDG","CITES","CMS","RAMSAR","UNCCD")

# Initialize an empty matrix to store counts
co_occurrence_matrix <- matrix(0, nrow = length(sources_ordered), ncol = length(sources_ordered), 
                               dimnames = list(sources_ordered, sources_ordered))

# Fill the matrix with counts
for (sources in source_list) {
  unique_pair <- unique(sources)  # Ensure uniqueness within a row
  if (length(unique_pair) == 1) {
    # Increment the diagonal for single-source occurrences
    co_occurrence_matrix[unique_pair, unique_pair] <- 
      co_occurrence_matrix[unique_pair, unique_pair] + 1
  } else {
    # Handle co-occurrences between multiple sources
    for (i in 1:(length(unique_pair) - 1)) {
      for (j in (i + 1):length(unique_pair)) {
        source_i <- unique_pair[i]
        source_j <- unique_pair[j]
        
        # Check to ensure indexes are within bounds
        if (source_i %in% sources_ordered && source_j %in% sources_ordered) {
          co_occurrence_matrix[source_i, source_j] <- 
            co_occurrence_matrix[source_i, source_j] + 1
          co_occurrence_matrix[source_j, source_i] <- 
            co_occurrence_matrix[source_j, source_i] + 1
        }
      }
    }
  }
}

# Display the resulting matrix
print(co_occurrence_matrix)
write.csv(co_occurrence_matrix, '../output/all_indicators_matrix_19052025.csv')
#co_occurrence_matrix = read.csv('../output/all_indicators_matrix_19052025.csv')


# Figure 2: matrix of shared indicators

# Melt the matrix into a long format suitable for ggplot2
melted_matrix <- melt(co_occurrence_matrix)

# Define the desired order to fill in the upper triangle of the matrix
desired_order <- data.frame(
  Var1 = c("IPBES", "IPBES", "GEO", "IPBES", "GEO", "IPCC", "IPBES", "GEO", "IPCC", "GBF",
           "IPBES", "GEO", "IPCC", "GBF", "SDG", "IPBES", "GEO", "IPCC", "GBF", "SDG",
           "CITES", "IPBES", "GEO", "IPCC", "GBF", "SDG", "CITES", "CMS", "IPBES", "GEO",
           "IPCC", "GBF", "SDG", "CITES", "CMS", "RAMSAR", "IPBES", "GEO", "IPCC", "GBF",
           "SDG", "CITES", "CMS", "RAMSAR", "UNCCD"),
  Var2 = c("IPBES", "GEO", "GEO", "IPCC", "IPCC", "IPCC", "GBF", "GBF", "GBF", "GBF",
           "SDG", "SDG", "SDG", "SDG", "SDG", "CITES", "CITES", "CITES", "CITES", "CITES",
           "CITES", "CMS", "CMS", "CMS", "CMS", "CMS", "CMS", "CMS", "RAMSAR", "RAMSAR",
           "RAMSAR", "RAMSAR", "RAMSAR", "RAMSAR", "RAMSAR", "RAMSAR", "UNCCD", "UNCCD",
           "UNCCD", "UNCCD", "UNCCD", "UNCCD", "UNCCD", "UNCCD", "UNCCD")
)

# Filter the melted matrix to include only rows matching the desired order
melted_matrix_upper <- merge(melted_matrix, desired_order, by = c("Var1", "Var2"))

# Order the filtered matrix according to the given order
melted_matrix_upper <- melted_matrix_upper[match(paste(melted_matrix_upper$Var2, melted_matrix_upper$Var1),
                                         paste(desired_order$Var2, desired_order$Var1)), ]

# Filter 0 values so they are not shown in plt
melted_matrix_upper_filtered <- melted_matrix_upper %>%
  filter(value != 0)

# Create the heatmap plot
fig2 = ggplot(melted_matrix_upper_filtered, aes(x = Var1, y = Var2, fill = value), color = "white") +
  geom_tile(color = "white") +
  #scale_fill_gradient(low = "white", high = "steelblue", na.value = "grey80", guide = "colorbar") +
  scale_fill_gradient(low = "#fff0e1", high = "#ff9d9d", na.value = "grey80", guide = "colorbar",
                      limits = c(1, 400), # not sure why 0 still has a color
                      oob = scales::squish) +
  #scale_fill_gradient(low = munsell::mnsl("5P 7/12"), high = munsell::mnsl("5P 2/12"))+
  #scale_fill_distiller(palette = "RdPu") +
  scale_x_discrete(position = "top") +  # Move x-axis to the top
  theme_minimal() +
  labs(title = "",
       x = "Source",
       y = "Source",
       fill = "Shared metrics") +
  theme(axis.text.x = element_text(angle = 45, hjust = 0))

fig2
ggsave(file="../output/figures/fig2_190525.svg", plot=fig2, 
       width=7, height=4, units = "in", dpi = 300)
ggsave(file="../output/figures/fig2_190525.png", plot=fig2, 
       width=7, height=4, units = "in", dpi = 300)

# Figure 3----
# Fig 3a: Indicators shared (and also not shared) among sources----

# ony shared metrics
melted_matrix_upper_shared = 
  # Retain only shared indicators
  filter(melted_matrix_upper, Var1 != Var2) %>% 
  # improve viz
  mutate(value = if_else(value <= 5 & value != 0,
                         true = 7, 
                         false = value))

# all metrics
melted_matrix_upper_all = melted_matrix_upper %>% 
  # Retain only shared indicators
  #filter(melted_matrix_upper, Var1 != Var2) %>% 
  # improve viz
  mutate(value = if_else(value <= 5 & value != 0,
                         true = 7, 
                         false = value))

# Define a color scheme for the different entities
grid_color <- c(
  IPBES = "#E5F5E0", IPCC = "#A1D99B", GEO = "#31A354",
  GBF = "#EFF3FF", SDG = "#C6DBEF", 
  RAMSAR = "#9ECAE1",CITES = "#6BAED6",
  CMS = "#3182BD",UNCCD = "#08519C"
)
# library(RColorBrewer)
# display.brewer.all()
# brewer.pal(n=6,"Blues")
# brewer.pal(n=3,"Greens")

# Define the order of entities where treaties are at the top and assessments at the bottom
entity_order <- c("IPBES", "IPCC", "GEO","GBF","SDG","RAMSAR","CITES", "CMS","UNCCD")


# Set plot saving file and parameters
png(filename = "../output/figures/fig3all_190525.png",
    #filename = "../output/figures/fig3shared_190525.png",
    width = 4, height = 4, units = "in", 
    bg = "white", res = 300) 

# chord parameters
circos.clear()
circos.par(start.degree = 165, track.margin = c(0.01, 0.01), gap.after = 5,
           cell.padding = c(0, 0, 0, 0))

# Create the chord diagram
chordDiagram(
  melted_matrix_upper_all,
  #melted_matrix_upper_shared,
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
                facing = "inside", niceFacing = TRUE, adj = c(0.8, 0))
  },
  bg.border = NA
  )

# Finalize and clear the circos plot
circos.clear()
dev.off()

# Fig 3b: Only Indicators shared between assess and MEAs----

# Split sources into a list
#indic$sources2 <- strsplit(as.character(indic$sources), ";")
source_list2 <- strsplit(as.character(all_indicators_cl$sources), ";")

# Create a new data frame for links between treaties and assessments
treaties <- c("GBF", "SDG", "CITES", "CMS", "RAMSAR", "UNCCD")
assessments <- c("IPBES", "GEO", "IPCC")

# Initialize a matrix to store counts
link_matrix <- matrix(0, nrow = length(treaties), ncol = length(assessments),
                      dimnames = list(treaties, assessments))

# Fill the matrix with counts of links
for (i in 1:nrow(indic)) {
  sources <- source_list2[[i]]
  for (source in sources) {
    if (source %in% treaties) {
      for (assessment in assessments) {
        if (grepl(assessment, source_list2[i], fixed = TRUE)) {
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
  RAMSAR = "#9ECAE1",CITES = "#6BAED6",
  CMS = "#3182BD",UNCCD = "#08519C"
)
# library(RColorBrewer)
# display.brewer.all()
# brewer.pal(n=6,"Blues")
# brewer.pal(n=3,"Greens")

# Define the order of entities where treaties are at the top and assessments at the bottom
entity_order <- c("IPBES", "IPCC", "GEO","GBF","SDG","RAMSAR","CITES", "CMS","UNCCD")

# Set plot saving file and parameters
png(filename = "../output/figures/fig3b_shared_190525.png",
    width = 4, height = 4, units = "in", 
    bg = "white", res = 300) 

# chord parameters
circos.clear()
circos.par(start.degree = 165, track.margin = c(0.01, 0.01), gap.after = 5,
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
dev.off()

# Fig 3c: All Indicators shared (sankey)-----

melted_matrix_upper_viz = melted_matrix_upper %>% 
  # remove 0
  filter(value != 0) %>% 
  # improve viz
  mutate(value = value + 7) %>% 
  mutate(Var1 = factor(Var1, 
                         levels=c("IPBES","IPCC","GEO",'GBF','SDG','RAMSAR','CITES','CMS','UNCCD')))
  # add more space for CMS and UNCCD
  # mutate(n = if_else(source %in% c('CITES','RAMSAR','CMS','UNCCD'),
  #                    true = n + 5,false = n))

fig3c = ggplot(melted_matrix_upper_viz,
       aes(y = value,
           axis1 = Var1, axis2 = Var2)) +
  geom_alluvium(aes(fill = Var1),
                width = 1/6, knot.pos = 0.2, reverse = FALSE) +
  scale_fill_manual(values =c(
    IPBES = "#E5F5E0", IPCC = "#A1D99B", GEO = "#31A354",
    GBF = "#EFF3FF", SDG = "#C6DBEF", 
    RAMSAR = "#9ECAE1",CITES = "#6BAED6",
    CMS = "#3182BD",UNCCD = "#08519C"
  )) +
  guides(fill = "none") +
  geom_stratum(alpha = 0, width = 1/6, reverse = FALSE) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)),reverse = FALSE, size = 3) +
  #scale_x_continuous(breaks = 1:2, labels = c("Sources", "Indicator category")) +
  theme_void()

fig3c
ggsave(file="../output/figures/fig3c_190525.svg", plot=fig3c, 
       width=7, height=8, units = "in", dpi = 300)
ggsave(file="../output/figures/fig3c_190525.png", plot=fig3c, 
       width=7, height=8, units = "in", dpi = 300)

# Figure 4: Summaries of indicators by categories----

indic_categories = indic %>%   
  # summary cat + source
  group_by(Categories,source)  %>% 
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
                                levels=c("IPBES","IPCC","GEO",'GBF','SDG','RAMSAR','CITES','CMS','UNCCD'))) %>% 
  mutate(Categories = factor(Categories, 
                         levels=c('Biodiversity','Ecosystems','Ecosystem\n services','Human\n well-being',
                                  'Direct\n drivers','Human\n assets','Knowledge\n systems','Governance'))) %>% 
  # improve viz for figure
  mutate(n = if_else(source %in% c('CITES','RAMSAR','CMS','UNCCD'),
                     true = n + 5,false = n))

# Plot
fig4 = ggplot(indic_categories,
       aes(y = n,
           axis1 = source, axis2 = Categories)) +
  geom_alluvium(aes(fill = Categories),
                width = 1/6, knot.pos = 0.2, reverse = FALSE) +
  scale_fill_manual(values =c('Governance'="#440154",'Knowledge\n systems'= "#46327e",
                              'Human\n assets'="#365c8d",'Direct\n drivers'="#277f8e",
                              'Human\n well-being'="#1fa187",'Ecosystem\n services'="#4ac16d" ,
                              'Ecosystems'="#a0da39",'Biodiversity'="#fde725")) +
  #scale_x_discrete(limits=c("IPBES","GEO","IPCC","GBF","SDG","CITES","CMS","RAMSAR","UNCCD")) +
  guides(fill = "none") +
  geom_stratum(alpha = 0, width = 1/6, reverse = FALSE) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)),reverse = FALSE, size = 3) +
  #scale_x_continuous(breaks = 1:2, labels = c("Sources", "Indicator category")) +
  theme_void()

fig4
ggsave(file="../output/figures/fig4_190525.svg", plot=fig4, 
       width=7, height=8, units = "in", dpi = 300)
ggsave(file="../output/figures/fig4_190525.png", plot=fig4, 
       width=7, height=8, units = "in", dpi = 300)









