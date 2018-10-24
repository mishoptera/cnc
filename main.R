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

# some necessary file cleaning
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

# Create a table of PERMANOVA results for all taxa in all regions
tab_all <- adonis.table(all_wfreq) %>% mutate (taxon = "all")
tab_plants <- adonis.table(all_wfreq) %>% mutate (taxon = "plants")
tab_animals <- adonis.table(all_wfreq) %>% mutate (taxon = "animals")
tab <- bind_rows(tab_all, tab_plants, tab_animals)
write.csv(tab, "permanova_results.csv")


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

# one table to bind them all
big_birds <- st_birds %>%
  left_join(select(st3_birds, -c(count, rank)), by="scientific_name") %>%
  mutate (taxon="birds")

big_mammals <- st_mammals %>%
  left_join(select(st3_mammals, -c(count, rank)), by="scientific_name") %>%
  mutate (taxon="mammals")

big_reptiles <- st_reptiles %>%
  left_join(select(st3_reptiles, -c(count, rank)), by="scientific_name") %>%
  mutate (taxon="reptiles")

big_amphibians <- st_amphibians %>%
  left_join(select(st3_amphibians, -c(count, rank)), by="scientific_name") %>%
  mutate (taxon="amphibians")

big_gastropods <- st_gastropods %>%
  left_join(select(st3_gastropods, -c(count, rank)), by="scientific_name") %>%
  mutate (taxon="gastropods")

big_insects <- st_insects %>%
  left_join(select(st3_insects, -c(count, rank)), by="scientific_name") %>%
  mutate (taxon="insects")

big_dicots <- st_dicots %>%
  left_join(select(st3_dicots, -c(count, rank)), by="scientific_name") %>%
  mutate (taxon="dicots")

big_monocots <- st_monocots %>%
  left_join(select(st3_monocots, -c(count, rank)), by="scientific_name") %>%
  mutate (taxon="monocots")

big_ferns <- st_ferns %>%
  left_join(select(st3_ferns, -c(count, rank)), by="scientific_name") %>%
  mutate (taxon="ferns")

big_conifers <- st_conifers %>%
  left_join(select(st3_conifers, -c(count, rank)), by="scientific_name") %>%
  mutate (taxon="conifers")

names <- all_wfreq %>%
  select(scientific_name:common_name) %>%
  unique()

big_everything <- big_birds %>%
  bind_rows(big_mammals, big_reptiles, big_amphibians, big_gastropods, big_insects, big_dicots, big_monocots, big_ferns, big_conifers) %>%
  left_join(names, by="scientific_name")

write.csv(big_everything, "big_everything.csv")
