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
load('all_wfreq.Rdata')
load('cities.Rdata')

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
ggsave("cnc_map.tiff", width = 20, height = 15, units = "cm")


# *************************************************************
# COMMUNITY COMPOSITION
# *************************************************************
source('cc_functions.r')

# All Taxa
cc_all <- cc_matrix(all_wfreq)
cc_all_env <- cc_env(cc_all)
plot_cc_us(cc_all, cc_all_env, "All taxa")
plot_cc_region_4(all_wfreq, "All taxa")

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

# Create a table of PERMANOVA results for all taxa in all regions.
tab_all <- adonis.table(all_wfreq) %>% mutate (taxon = "all")
tab_plants <- adonis.table(all_wfreq) %>% mutate (taxon = "plants")
tab_animals <- adonis.table(all_wfreq) %>% mutate (taxon = "animals")
tab <- bind_rows(tab_all, tab_plants, tab_animals)
write.csv(tab, "permanova_results.csv")       # Table 2


# *************************************************************
# INDIVIDUAL SPECIES PATTERNS
# *************************************************************
source('isp_functions.r')

# big ranking tables
ranks_dicots <- create_big_table(dicots)
ranks_monocots <- create_big_table(monocots)
ranks_ferns <- create_big_table(ferns)
ranks_conifers <- create_big_table(conifers)
ranks_birds <- create_big_table(birds)
ranks_insects <- create_big_table(insects)
ranks_reptiles <- create_big_table(reptiles)
ranks_amphibians <- create_big_table(amphibians)
ranks_mammals <- create_big_table(mammals)
ranks_gastropods <- create_big_table(gastropods)

# create city aggregation metric tables
cam_dicots <- small_table3(ranks_dicots)
cam_monocots <- small_table3(ranks_monocots)
cam_ferns <- small_table3(ranks_ferns)
cam_conifers <- small_table3(ranks_conifers)
cam_birds <- small_table3(ranks_birds)
cam_mammals <- small_table3(ranks_mammals)
cam_gastropods <- small_table3(ranks_gastropods)
cam_insects <- small_table3(ranks_insects)
cam_reptiles <- small_table3(ranks_reptiles)
cam_amphibians <- small_table3(ranks_amphibians)

# create averaged ranking metric tables
arm_dicots <- small_table(ranks_dicots)
arm_monocots <- small_table(ranks_monocots)
arm_ferns <- small_table(ranks_ferns)
arm_conifers <- small_table(ranks_conifers)
arm_birds <- small_table(ranks_birds)
arm_mammals <- small_table(ranks_mammals)
arm_gastropods <- small_table(ranks_gastropods)
arm_insects <- small_table(ranks_insects)
arm_reptiles <- small_table(ranks_reptiles)
arm_amphibians <- small_table(ranks_amphibians)


# one table to bind them all
big_birds <- cam_birds %>%
  left_join(select(arm_birds, -c(count, rank)), by="scientific_name") %>%
  mutate (taxon="birds")

big_mammals <- cam_mammals %>%
  left_join(select(arm_mammals, -c(count, rank)), by="scientific_name") %>%
  mutate (taxon="mammals")

big_reptiles <- cam_reptiles %>%
  left_join(select(arm_reptiles, -c(count, rank)), by="scientific_name") %>%
  mutate (taxon="reptiles")

big_amphibians <- cam_amphibians %>%
  left_join(select(arm_amphibians, -c(count, rank)), by="scientific_name") %>%
  mutate (taxon="amphibians")

big_gastropods <- cam_gastropods %>%
  left_join(select(arm_gastropods, -c(count, rank)), by="scientific_name") %>%
  mutate (taxon="gastropods")

big_insects <- cam_insects %>%
  left_join(select(arm_insects, -c(count, rank)), by="scientific_name") %>%
  mutate (taxon="insects")

big_dicots <- cam_dicots %>%
  left_join(select(arm_dicots, -c(count, rank)), by="scientific_name") %>%
  mutate (taxon="dicots")

big_monocots <- cam_monocots %>%
  left_join(select(arm_monocots, -c(count, rank)), by="scientific_name") %>%
  mutate (taxon="monocots")

big_ferns <- cam_ferns %>%
  left_join(select(arm_ferns, -c(count, rank)), by="scientific_name") %>%
  mutate (taxon="ferns")

big_conifers <- cam_conifers %>%
  left_join(select(arm_conifers, -c(count, rank)), by="scientific_name") %>%
  mutate (taxon="conifers")

names <- all_wfreq %>%
  select(scientific_name:common_name) %>%
  unique() 

big_everything <- big_birds %>%
  bind_rows(big_mammals, big_reptiles, big_amphibians, big_gastropods, big_insects, big_dicots, big_monocots, big_ferns, big_conifers) %>%
  left_join(names, by="scientific_name") %>%
  distinct(scientific_name, .keep_all = TRUE) %>%
  select(common_name, everything()) %>%
  select(taxon, everything())

big_over100obs <- big_everything %>%
  arrange(desc(count)) %>%
  filter(count>=100)

write.csv(big_everything, "big_everything.csv")
write.csv(big_over100obs, "big_over100obs.csv")    # Table 3

##*************************
## Some final summary stats to extract more things of interest
totals <- plants %>% 
  union (animals) %>%
  summarise (num_species = n_distinct (scientific_name),
             total_obs = n())

everything <- plants %>%
  union (animals) %>%
  group_by(taxon_class_name)%>%
  summarise (num_species =  n_distinct(scientific_name),
             num_obs = n(), r_all_species = num_species / 4545, r_all_obs = num_obs / 63375) %>%
  arrange(desc(r_all_obs))

top100 <- plants %>%
  union (animals) %>%
  group_by(scientific_name)%>%
  mutate (count = n()) %>%
  group_by(taxon_class_name)%>%
  filter(count>=100) %>%
  summarise (num_species =  n_distinct(scientific_name),
             num_obs = n(), r_100 = num_species / 100) %>%
  left_join(everything, by = "taxon_class_name") %>%
  mutate (difference = r_all - r_100) %>%
  arrange (desc(r_all)) 
top100     # Birds and dicots get overrepresented in the top 100, while insects get underrepresented
