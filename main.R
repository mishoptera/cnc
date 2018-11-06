# Author: Misha Leong
# Date: October 2018
# Project: Exploring urban biodiversity patterns with City Nature Challenge iNaturalist data
# Specificly: This is the main code that pulls everything together


# *************************************************************
# FIRST THINGS FIRST
# *************************************************************

# load libraries
library(tidyverse)
library(vegan)
library(ggmap)
library(ggrepel)
library(ggpubr)
library(stringr)

# load files
load('data/all_wfreq.Rdata')
load('data/cities.Rdata')

# some last minute file cleaning
all_wfreq$scientific_name <- str_replace(all_wfreq$scientific_name,"Columba livia domestica", "Columba livia")
all_wfreq$scientific_name <- as.factor(all_wfreq$scientific_name)

# data subsets for later use
plants <- all_wfreq %>% filter(taxon_class_name %in% c("Magnoliopsida", "Liliopsida", "Polypodiopsida", "Pinopsida", "Agaricomycetes", "Lecanoromycetes"))
animals <- all_wfreq %>% filter(taxon_class_name %in% c("Arachnida", "Aves", "Gastropoda", "Insecta", "Amphibia", "Reptilia", "Mammalia"))
dicots <- all_wfreq %>% filter(taxon_class_name == "Magnoliopsida") %>% mutate (taxon="dicots")
monocots <- all_wfreq %>% filter(taxon_class_name == "Liliopsida") %>% mutate (taxon="monocots")
ferns <- all_wfreq %>% filter(taxon_class_name == "Polypodiopsida")%>% mutate (taxon="ferns")
conifers <- all_wfreq %>% filter(taxon_class_name == "Pinopsida") %>% mutate (taxon="conifers")
birds <- all_wfreq %>% filter(taxon_class_name == "Aves") %>% mutate (taxon="birds")
insects <- all_wfreq %>% filter(taxon_class_name == "Insecta") %>% mutate (taxon="insects")
arachnids <- all_wfreq %>% filter(taxon_class_name == "Arachnida") %>% mutate (taxon="arachnids")
reptiles <- all_wfreq %>% filter(taxon_class_name == "Reptilia") %>% mutate (taxon="reptiles")
amphibians <- all_wfreq %>% filter(taxon_class_name == "Amphibia") %>% mutate (taxon="amphibians")
mammals <- all_wfreq %>% filter(taxon_class_name == "Mammalia") %>% mutate (taxon="mammals")
gastropods <- all_wfreq %>% filter(taxon_class_name == "Gastropoda") %>% mutate (taxon="gastropods")



# *************************************************************
# MAP OF CNC CITIES (Figure 1)
# *************************************************************
map <- get_googlemap(center = c(-98, 38), zoom = 4,
                     color = "bw",
                     style = "feature:road|visibility:off&style=element:labels|visibility:off&style=feature:administrative|visibility:off")

# Plot cities onto map basic
ggmap(map) +
  geom_point(data = cities, aes(x = lon, y = lat))

# Plot cities onto map with colors, sizes, and labels
ggmap(map) +
  geom_point(data = cities, aes(x = lon, y = lat, size = num_obs, color = region)) +
  labs(colour = "Regions", size = "Records")+
  geom_text_repel(data = cities, aes(x = lon, y = lat, label = official_hometown))

# Save it for export
ggsave("figures_n_tables/cnc_map.tiff", width = 20, height = 15, units = "cm")


# *************************************************************
# COMMUNITY COMPOSITION (Figure 2, Figure 3, and Table 2)
# *************************************************************
source('functions/cc_functions.r')

# All Taxa
cc_all <- cc_matrix(all_wfreq)
cc_all_env <- cc_env(cc_all)
plot_cc_us(cc_all, cc_all_env, "All taxa")          # Figure 2
plot_cc_region_4(all_wfreq, "All taxa")             # Figure 3

# All Plants
cc_plants <- cc_matrix(plants) 
cc_plants_env <- cc_env(cc_plants)
plot_cc_us(cc_plants, cc_plants_env, "Plants")
plot_cc_region_4(plants, "Plants")

# All Animals
cc_animals <- cc_matrix(animals) 
cc_animals_env <- cc_env(cc_animals)
plot_cc_us(cc_animals, cc_animals_env, "Animals")
plot_cc_region_4(animals, "Animals")

# Create a table of PERMANOVA results for all taxa in all regions, nested by hometown.
tab_all <- adonis.table.hometown(all_wfreq) %>% mutate (taxon = "all")
tab_plants <- adonis.table.hometown(all_wfreq) %>% mutate (taxon = "plants")
tab_animals <- adonis.table.hometown(all_wfreq) %>% mutate (taxon = "animals")
tab <- bind_rows(tab_all, tab_plants, tab_animals)
write.csv(tab, "figures_n_tables/permanova_results_hometown.csv")       # Table 2

# Create a table of PERMANOVA results for all taxa in all regions, nested by land cover type.
tab_all <- adonis.table.lc(all_wfreq) %>% mutate (taxon = "all")
tab_plants <- adonis.table.lc(all_wfreq) %>% mutate (taxon = "plants")
tab_animals <- adonis.table.lc(all_wfreq) %>% mutate (taxon = "animals")
tab <- bind_rows(tab_all, tab_plants, tab_animals)
write.csv(tab, "figures_n_tables/permanova_results_lc.csv")       # Table 2


# *************************************************************
# INDIVIDUAL SPECIES PATTERNS (Table 3)
# *************************************************************
source('functions/isp_functions.r')

taxa <- c("dicots", "monocots", "ferns", "conifers", "birds", "insects", "reptiles", "amphibians", "mammals", "gastropods")

# create big ranking tables for each taxa
lapply(taxa, function(i){
 assign(paste0("ranks_", i), create_big_table(get(i)))
})

# create city aggregation metric tables
lapply(taxa, function(i){
  ranks_table <- eval(as.name(paste0("ranks_", i)))
  assign(paste0("cam_", i), small_table3(ranks_table))
})

# create averaged ranking metric tables
lapply(taxa, function(i){
  ranks_table <- eval(as.name(paste0("ranks_", i)))
  assign(paste0("arm_", i), small_table(ranks_table))
})

# one table to bind them all
lapply(taxa, function(i){
  assign((paste0("big_", i)), bigify(eval(as.name(paste0("cam_", i))), 
                                      eval(as.name(paste0("arm_", i))), i))

})

# to be able to add common names to table
names <- all_wfreq %>%
  select(scientific_name:common_name) %>%
  unique() 

# creating a single table with all of the above
big_everything <- big_birds %>%
  bind_rows(big_mammals, big_reptiles, big_amphibians, big_gastropods, big_insects, big_dicots, big_monocots, big_ferns, big_conifers) %>%
  left_join(names, by="scientific_name") %>%
  distinct(scientific_name, .keep_all = TRUE) %>%
  select(common_name, everything()) %>%
  select(taxon, everything())

# to make it a bit manageable to share in paper as a table
big_over100obs <- big_everything %>%
  arrange(desc(count)) %>%
  filter(count>=100)

write.csv(big_everything, "figures_n_tables/big_everything.csv")
write.csv(big_over100obs, "figures_n_tables/big_over100obs.csv")    # Table 3


# *************************************************************
# SUMMARY STATS OF INTEREST (Table 4)
# *************************************************************

# setting some variables values
totals <- plants %>% 
  union (animals) %>%
  summarise (num_species = n_distinct (scientific_name),
             num_obs = n())
total_species <- totals$num_species
total_obs <- totals$num_obs

subsets <- plants %>% 
  union (animals) %>%
  group_by(scientific_name)%>%
  mutate (count = n()) %>%
  filter(count>=100) %>%
  ungroup() %>%
  summarise (num_species = n_distinct (scientific_name),
             num_obs = n())
subset_species <- subsets$num_species
subset_obs <- subsets$num_obs


over100 <- plants %>%
  union (animals) %>%
  group_by(scientific_name)%>%
  mutate (count = n()) %>%
  group_by(taxon_class_name)%>%
  filter(count>=100) %>%
  summarise (subset_num_species =  n_distinct(scientific_name),
             subset_num_obs = n(), 
             subset_ratio_species = subset_num_species / subset_species,
             subset_ratio_obs = subset_num_obs / subset_obs) 
  
  
everything <- plants %>%
  union (animals) %>%
  group_by(taxon_class_name)%>%
  summarise (all_num_species =  n_distinct(scientific_name),
             all_num_obs = n(), 
             all_ratio_species = all_num_species / total_species, 
             all_ratio_obs = all_num_obs / total_obs) %>%
  arrange(desc(all_num_species)) %>%
  left_join(over100, by = "taxon_class_name") %>%
  mutate (diff_species = all_ratio_species - subset_ratio_species,
          diff_obs = all_ratio_obs - subset_ratio_obs) 
everything    # Birds and dicots get overrepresented in the top 100, while insects get underrepresented
write.csv(everything, "figures_n_tables/summary_over100obs.csv")  # Table 5
