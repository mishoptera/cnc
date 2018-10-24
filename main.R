# ************************************
# FIRST THINGS FIRST
# ************************************

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

# necessary file cleaning
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

# ************************************
# COMMUNITY COMPOSITION
# ************************************
