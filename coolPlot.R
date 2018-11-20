
# Function to create a matrix of taxa by city/landuse "site".  Analagous to the dune dataset
cc_matrix <- function(taxon_data) {
  taxon_matrix <- taxon_data %>%
    group_by(hometown, nlcd_group2, common_name) %>%
    summarise(obs = n()) %>%
    spread(common_name, obs, fill = 0) %>%
    as.data.frame()
  
  rownames(taxon_matrix) <-paste(taxon_matrix$hometown, taxon_matrix$nlcd_group2, sep = ".")
  taxon_matrix[,1:2] <- NULL
  
  return(taxon_matrix)
}

# Function to create a complementary matrix of environmental variables for the taxa matrix.
cc_env <- function(taxon_matrix) {
  cities <- rownames(taxon_matrix)
  
  taxon_env <- cities %>%
    as.data.frame.character() %>%
    separate(col = 1, into = c("hometown", "landcover_group"), extra = "merge")
  
  return(taxon_env)
}






# *************************************************************
# PLOTS
# *************************************************************

# ////////////////////
# Bird community compostion plots and for entire US
  # running the NMDS
  all_matrix <- cc_matrix(birds)
  cc_all_env <- cc_env(all_matrix)
 
  mod <- metaMDS(all_matrix, distance = "bray", k = 2, try = 100, trymax = 500)
 
 
  # pulling data out to plot in ggplot
  data_scores <- as.data.frame(scores(mod)) 
  data_scores$hometown <- as.factor(cc_all_env$hometown)
  data_scores$landcover <- cc_all_env$landcover_group
  stress <- signif(mod$stress, digits = 3)
  species_scores <- as.data.frame(scores(mod, "species"))
  species_scores$common_name <- rownames(species_scores)
  species_scores_subset <- species_scores %>%
    left_join(big_everything, by = "common_name") %>%
    filter(count>=30) %>%
    select(NMDS1, NMDS2, common_name)
  subtitle <- paste("2-D Stress =", stress)
  cities_mod <- select(cities, c(hometown, lat, lon, region, official_hometown))
  data_scores <- left_join(data_scores, cities_mod, by = "hometown")
  nice_lc <- tibble(landcover = c("natural", "developed1_open_space", 
                                  "developed2_low_intensity", "developed3_medium_intensity",
                                  "developed4_high_intensity"), urbanization = c("0 - natural",
                                                                                 "1 - open space", "2 - low intensity",
                                                                                 "3 - medium intensity", "4 - high intensity"))
  data_scores <- left_join(data_scores, nice_lc, by = "landcover")
  data_scores_d3 <- data_scores %>% filter(landcover=="developed3_medium_intensity")
  data_scores_natural <- data_scores %>% filter(landcover=="natural")
  
  # plot it
  species <- ggplot()+
    geom_point(data=data_scores,aes(x=NMDS1,y=NMDS2, shape = region, col=urbanization),size=3) + 
    labs (shape = "Regions", colour = "Urbanization Levels", fill = "Urbanization Levels") +
    labs(title = "Land cover groupings", subtitle = subtitle) +
    stat_chull(data=data_scores, geom = "polygon", alpha = 0.1, aes(x=NMDS1,y=NMDS2,
                                                                    fill=urbanization, color=urbanization)) +
    geom_text_repel(data=species_scores_subset,aes(x=NMDS1,y=NMDS2,label=common_name)) +
    theme_bw() +
    coord_equal() +
    theme(axis.text.x = element_blank(),  # remove x-axis text
          axis.text.y = element_blank(), # remove y-axis text
          axis.ticks = element_blank(),  # remove axis ticks
          axis.title.x = element_text(size=12), # remove x-axis labels
          axis.title.y = element_text(size=12), # remove y-axis labels
          panel.background = element_blank(), 
          panel.grid.major = element_blank(),  #remove major-grid labels
          panel.grid.minor = element_blank(),  #remove minor-grid labels
          plot.background = element_blank())

ggsave(plot = species, filename = "figures_n_tables/birds.jpg", height = 40, width = 40, units = "cm")


# ////////////////////////////////////////
# Trying to do birds on a regional scale
plot_cc_region_species <- function (all_matrix, title) {
  all_env <- cc_env(all_matrix)
  mod_all <- metaMDS(all_matrix, distance = "bray", k = 2, try = 100, trymax = 500)
  data_scores <- as.data.frame(scores(mod_all)) 
  data_scores$hometown <- as.factor(all_env$hometown)
  data_scores$landcover <- all_env$landcover_group
  stress <- signif(mod_all$stress, digits = 3)
  species_scores <- as.data.frame(scores(mod_all, "species"))
  species_scores$common_name <- rownames(species_scores)
  species_scores_subset <- species_scores %>%
    left_join(big_everything, by = "common_name") %>%
    filter(count>=10) %>%
    select(NMDS1, NMDS2, common_name)
  subtitle <- paste("2-D Stress =", stress)
  cities <- select(cities, c(hometown, lat, lon, region, official_hometown))
  data_scores <- left_join(data_scores, cities, by = "hometown")
  nice_lc <- tibble(landcover = c("natural", "developed1_open_space", 
                                  "developed2_low_intensity", "developed3_medium_intensity", "developed4_high_intensity"), 
                    urbanization = c("0 - natural", "1 - open space", "2 - low intensity", 
                                     "3 - medium intensity", "4 - high intensity"))
  data_scores_natural <- data_scores %>% filter(landcover=="natural")
  data_scores <- left_join(data_scores, nice_lc, by = "landcover")
  data_scores_d4<- data_scores %>% filter(landcover=="developed4_high_intensity")
  
  all <- ggplot() +
    geom_point(data=data_scores, aes(x = NMDS1, y = NMDS2, shape = official_hometown) ,size=3) + 
    labs (shape = "Cities", fill = "Urbanization Levels", colour = "Urbanization Levels") +
    stat_chull(data = data_scores, geom = "polygon", alpha = 0.1, aes(x = NMDS1, y = NMDS2, 
                                                                      fill = urbanization, color=urbanization)) +
    geom_text_repel(data=species_scores_subset,aes(x=NMDS1,y=NMDS2,label=common_name)) +
    theme_bw() +
    guides(colour = guide_legend(order = 2), 
           fill = guide_legend(order = 2), 
           shape = guide_legend(order = 1)) +
    scale_shape_discrete(guide = FALSE) +
    labs(title = title, subtitle = subtitle) +
    coord_equal() +
    theme(plot.title=element_text(size=20), 
          axis.text.x = element_blank(),  # remove x-axis text
          axis.text.y = element_blank(), # remove y-axis text
          axis.ticks = element_blank(),  # remove axis ticks
          axis.title.x = element_text(size=12), # remove x-axis labels
          axis.title.y = element_text(size=12), # remove y-axis labels
          panel.background = element_blank(), 
          panel.grid.major = element_blank(),  #remove major-grid labels
          panel.grid.minor = element_blank(),  #remove minor-grid labels
          plot.background = element_blank())
  
  filename <- paste("figures_n_tables/cc_region_birds_", title, ".jpg", sep = "")
  ggsave(plot = all, filename = filename, height = 40, width = 40, units = "cm")
  
}
cc_texas <- cc_matrix(birds %>% filter(hometown %in% c("houston", "dallas", "austin")))
plot_cc_region_species(cc_texas, "Texas")

cc_atlantic <- cc_matrix(birds %>% filter(hometown %in% c("boston", "newyork", "washingtondc")))
plot_cc_region_species(cc_atlantic, "Atlantic Coast")

cc_pacific <- cc_matrix(birds %>% filter(hometown %in% c("sanfrancisco", "losangeles", "seattle")))
plot_cc_region_species(cc_pacific, "Pacific Coast")

cc_pacific <- cc_matrix(birds %>% filter(hometown %in% c("sanfrancisco", "losangeles")))
plot_cc_region_species(cc_pacific, "California")
