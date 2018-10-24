# Author: Misha Leong
# Date: October 2018
# Project: Exploring urban biodiversity patterns with City Nature Challenge iNaturalist data
# Specificly: This is the main script for community composition analyses

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

# *************************************************************
# Run nationwide PERMANOVA analyses
# *************************************************************
tab_all <- adonis.table(all_wfreq) %>% mutate (taxon = "all")
tab_plants <- adonis.table(all_wfreq) %>% mutate (taxon = "plants")
tab_animals <- adonis.table(all_wfreq) %>% mutate (taxon = "animals")
tab <- bind_rows(tab_all, tab_plants, tab_animals)

# *************************************************************
# Run regional PERMANOVA analyses
# *************************************************************
adonis.table <- function(taxon) {
  cc_texas <- cc_matrix(taxon %>% filter(hometown %in% c("houston", "dallas", "austin")))
  texas_perm <- adonis_cc_region(cc_texas)
  texas_r2 <- adonis_r2(texas_perm)
  texas_p <- adonis_p(texas_perm)
  
  cc_atlantic <- cc_matrix(taxon %>% filter(hometown %in% c("boston", "newyork", "washingtondc")))
  atlantic <- adonis_cc_region(cc_atlantic)  
  atlantic_perm <- adonis_cc_region(cc_atlantic)
  atlantic_r2 <- adonis_r2(atlantic_perm)
  atlantic_p <- adonis_p(atlantic_perm)
  
  cc_pacific <- cc_matrix(taxon %>% filter(hometown %in% c("sanfrancisco", "losangeles", "seattle")))
  pacific_perm <- adonis_cc_region(cc_pacific)
  pacific_r2 <- adonis_r2(pacific_perm)
  pacific_p <- adonis_p(pacific_perm)
  
  cc_central <- cc_matrix(taxon %>% filter(hometown %in% c("saltlakecity", "minneapolis", "chicago")))
  central_perm <- adonis_cc_region(cc_central)
  central_r2 <- adonis_r2(central_perm)
  central_p <- adonis_p(central_perm)
  
  adonis_table <- tribble(
    ~city, ~R2,  ~p,
    "Texas", texas_r2,  texas_p,
    "Atlantic", atlantic_r2,  atlantic_p,
    "Pacific", pacific_r2, pacific_p,
    "Central", central_r2, central_p
  )
  print(adonis_table)
  
}