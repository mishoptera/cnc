# Author: Misha Leong
# Date: October 2018
# Project: Exploring urban biodiversity patterns with City Nature Challenge iNaturalist data
# Specificly: These are the functions utilized for the community composition analyses



# *************************************************************
# REFORMAT DATA
# *************************************************************

# ////////////////////
# Function to create a matrix of taxa by city/landuse "site".  Analagous to the dune dataset
cc_matrix <- function(taxon_data) {
  taxon_matrix <- taxon_data %>%
    unite(long_name, taxon_class_name, taxon_order_name, taxon_family_name, scientific_name, sep = ".", remove = FALSE) %>%
    group_by(hometown, nlcd_group2, long_name) %>%
    summarise(obs = n()) %>%
    spread(long_name, obs, fill = 0) %>%
    as.data.frame()
  
  rownames(taxon_matrix) <-paste(taxon_matrix$hometown, taxon_matrix$nlcd_group2, sep = ".")
  taxon_matrix[,1:2] <- NULL
  
  return(taxon_matrix)
}

# ////////////////////
# Function to create a matrix of taxa by city/landuse "site".  Analagous to the dune dataset
# Just like above, but unites based on common name only to make adding species names to plot
# more legible
cc_matrix_commonnames <- function(taxon_data) {
  taxon_matrix <- taxon_data %>%
    group_by(hometown, nlcd_group2, common_name) %>%
    summarise(obs = n()) %>%
    spread(common_name, obs, fill = 0) %>%
    as.data.frame()
  
  rownames(taxon_matrix) <-paste(taxon_matrix$hometown, taxon_matrix$nlcd_group2, sep = ".")
  taxon_matrix[,1:2] <- NULL
  
  return(taxon_matrix)
}

# ////////////////////
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
# Community compostion plots and analyses nationwide
plot_cc_us <- function (all_matrix, all_env, title) {
  # running the NMDS
  mod <- metaMDS(all_matrix, distance = "bray", k = 2, try = 100, trymax = 500)
  lc_perm <- adonis(all_matrix ~ all_env$landcover_group, data = all_env, 
                    strata= all_env$hometown, permutations = 999)
  print(lc_perm)
  cg_perm <- adonis(all_matrix ~ all_env$hometown, data = all_env, 
                    strata = all_env$landcover_group, permutations = 999)
  print(cg_perm)
  
  # pulling data out to plot in ggplot
  data_scores <- as.data.frame(scores(mod)) 
  data_scores$hometown <- as.factor(cc_all_env$hometown)
  data_scores$landcover <- cc_all_env$landcover_group
  stress <- signif(mod$stress, digits = 3)
  subtitle <- paste("2-D Stress =", stress)
  cities_mod <- select(cities, c(hometown, lat, lon, region2, official_hometown))
  data_scores <- left_join(data_scores, cities_mod, by = "hometown")
  nice_lc <- tibble(landcover = c("natural", "developed1_open_space", 
                                  "developed2_low_intensity", "developed3_medium_intensity",
                                  "developed4_high_intensity"), urbanization = c("0 - natural",
                                                                                 "1 - open space", "2 - low intensity",
                                                                                 "3 - medium intensity", "4 - high intensity"))
  data_scores <- left_join(data_scores, nice_lc, by = "landcover")
  data_scores_d3 <- data_scores %>% filter(landcover=="developed3_medium_intensity")
  data_scores_natural <- data_scores %>% filter(landcover=="natural")
  
  # create city grouping plot
  cg <- ggplot()+
    geom_point(data=data_scores,aes(x=NMDS1,y=NMDS2,shape=urbanization,colour=region2),size=3) + 
    labs (fill = "Regions", colour = "Regions", shape = "Urbanization Levels") +
    labs(title = "City groupings", subtitle = subtitle) +
    stat_chull(data=data_scores, geom = "polygon", alpha = 0.1, aes(x=NMDS1,y=NMDS2,
                                                                    fill=region2, colour = region2, group=hometown)) +
    geom_text_repel(data=data_scores_d3, aes(x=NMDS1, y=NMDS2, label=official_hometown)) +
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
  
  # plot it for landcover grouping
  lc <- ggplot()+
    geom_point(data=data_scores,aes(x=NMDS1,y=NMDS2,shape=region2, col=urbanization),size=3) + 
    labs (shape = "Regions", colour = "Urbanization Levels", fill = "Urbanization Levels") +
    labs(title = "Land cover groupings", subtitle = subtitle) +
    stat_chull(data=data_scores, geom = "polygon", alpha = 0.1, aes(x=NMDS1,y=NMDS2,
                                                                    fill=urbanization, color=urbanization)) +
    geom_text_repel(data=data_scores_d3, aes(x=NMDS1, y=NMDS2, label=official_hometown)) +
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
  
  # Combine into one lovely figure and save
  plots <- ggarrange(lc, cg, labels = c("A", "B"), ncol = 1, nrow = 2)
  plots <- annotate_figure(plots,
                           top = text_grob(title, face = "bold", size = 18))
  filename <- paste("figures_n_tables/cc_us_", title, ".png", sep = "")
  ggsave(plot = plots, filename = filename, height = 20, width = 24, units = "cm")
}


# ////////////////////
# Community compostion plots and analyses regionally
plot_cc_region <- function (all_matrix, title) {
  all_env <- cc_env(all_matrix)
  mod_all <- metaMDS(all_matrix, distance = "bray", k = 2, try = 100, trymax = 500)
  data_scores <- as.data.frame(scores(mod_all)) 
  data_scores$hometown <- as.factor(all_env$hometown)
  data_scores$landcover <- all_env$landcover_group
  stress <- signif(mod_all$stress, digits = 3)
  subtitle <- paste("2-D Stress =", stress)
  cities <- select(cities, c(hometown, lat, lon, region2, official_hometown))
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
  return(all)
}

# ////////////////////
# Combine regional plots into one
plot_cc_region_4 <- function (taxon, title) {
  cc_texas <- cc_matrix(taxon %>% filter(hometown %in% c("houston", "dallas", "austin")))
  texas <- plot_cc_region(cc_texas, "Texas")
  
  cc_atlantic <- cc_matrix(taxon %>% filter(hometown %in% c("boston", "newyork", "washingtondc")))
  atlantic <- plot_cc_region(cc_atlantic, "Atlantic Coast")
  
  cc_pacific <- cc_matrix(taxon %>% filter(hometown %in% c("sanfrancisco", "losangeles", "seattle")))
  pacific <- plot_cc_region(cc_pacific, "Pacific Coast")
  
  cc_central <- cc_matrix(taxon %>% filter(hometown %in% c("saltlakecity", "minneapolis", "chicago")))
  central <- plot_cc_region(cc_central, "Central US")
  
  regions <- ggarrange(texas, atlantic, pacific, central, labels = c("A", "B", "C", "D"), ncol = 2, nrow = 2)
  regions <- annotate_figure(regions, top = text_grob(title, face = "bold", size = 18))
  filename <- paste("figures_n_tables/cc_region_", title, ".png", sep = "")
  ggsave(plot = regions, filename = filename, height = 20, width = 24, units = "cm")
}


# *************************************************************
# PERMANOVA ANALYSES
# *************************************************************

# PERMANOVA analysis no nestedness (only interested in how similar cities are to one another
# because we are feeding in land cover subsets from the get go. The idea being that high intensity
# land cover subsets will have cities that are more similar to one another than the natural
# land cover subsets)
# need to reevaluate how I'm creating these matrices and getting environmental variables set-up
adonis_cc <- function (all_matrix) {
  all_env <- cc_env(all_matrix) %>%
    left_join(cities, by = "hometown")
  perm <- adonis(all_matrix ~ all_env$region2, data = all_env, permutations = 999)
  return (perm)
}

# ////////////////////
# Assemble table of PERMANOVA analyses no nesting
adonis.table <- function(lc_subset) {
  cc <- cc_matrix(lc_subset)
  perm <- adonis_cc(cc)
  r2 <- adonis_r2(perm)
  p <- adonis_p(perm)
  AIC <- adonis_aic(perm)
  
  adonis_table <- tribble(
    ~R2,  ~p, ~AIC,
    r2, p, AIC
  )
  print(adonis_table)
}



# ////////////////////
# Extract R2
adonis_r2 <- function (perm) {
  r2 <- perm[[1]][["R2"]][[1]]
}

# ////////////////////
# Extract p value
adonis_p <- function (perm) {
  p  <- perm[[1]][["Pr(>F)"]][[1]]
}

# ////////////////////
# calculate AIC value
adonis_aic <- function (perm) {
  aic  <- AICc.PERMANOVA(perm)
  return(aic$AIC)
}
#////////////////////
# knit tables
knit_tables <- function(all_inat) {
  tab_all <- adonis.table(all_inat) %>% 
    mutate (urban_intensity = "all")
  tab_natural <- adonis.table(all_inat %>% filter (nlcd_group2 == "natural")) %>% 
    mutate (urban_intensity = "natural")
  tab_os <- adonis.table(all_inat %>% filter (nlcd_group2 == "developed1_open_space")) %>% 
    mutate (urban_intensity = "developed - open space")
  tab_2 <- adonis.table(all_inat %>% filter (nlcd_group2 == "developed2_low_intensity")) %>% 
    mutate (urban_intensity = "developed - low intensity")
  tab_3 <- adonis.table(all_inat %>% filter (nlcd_group2 == "developed3_medium_intensity")) %>% 
    mutate (urban_intensity = "developed - medium intensity")
  tab_4 <- adonis.table(all_inat %>% filter (nlcd_group2 == "developed4_high_intensity")) %>% 
    mutate (urban_intensity = "developed - high intensity")
  tab <- bind_rows(tab_natural, tab_os, tab_2, tab_3, tab_4)
  return(tab)
}
# -----------------------
## R Script from https://github.com/kdyson/R_Scripts/blob/master/AICc_PERMANOVA.R

AICc.PERMANOVA <- function(adonis.model) {
  
  # check to see if object is an adonis model...
  
  if (!(adonis.model$aov.tab[1,1] >= 1))
    stop("object not output of adonis {vegan} ")
  
  # Ok, now extract appropriate terms from the adonis model
  # Calculating AICc using residual sum of squares (RSS) since I don't think that adonis returns something I can use as a liklihood function...
  
  RSS <- adonis.model$aov.tab[rownames(adonis.model$aov.tab) == "Residuals", "SumsOfSqs"]
  MSE <- adonis.model$aov.tab[rownames(adonis.model$aov.tab) == "Residuals", "MeanSqs"]
  
  k <- ncol(adonis.model$model.matrix)# + 1 # add one for error variance
  
  nn <- nrow(adonis.model$model.matrix)
  
  # AIC : 2*k + n*ln(RSS)
  # AICc: AIC + [2k(k+1)]/(n-k-1)
  
  # based on https://en.wikipedia.org/wiki/Akaike_information_criterion;
  # https://www.researchgate.net/post/What_is_the_AIC_formula;
  # http://avesbiodiv.mncn.csic.es/estadistica/ejemploaic.pdf
  
  # AIC.g is generalized version of AIC = 2k + n [Ln( 2(pi) RSS/n ) + 1]
  # AIC.pi = k + n [Ln( 2(pi) RSS/(n-k) ) +1],
  
  AIC <- 2*k + nn*log(RSS)
  AIC.g <- 2*k + nn * (1 + log( 2 * pi * RSS / nn))
  AIC.MSE <- 2*k + nn * log(MSE)
  AIC.pi <- k + nn*(1 + log( 2*pi*RSS/(nn-k) )   )
  AICc <- AIC + (2*k*(k + 1))/(nn - k - 1)
  AICc.MSE <- AIC.MSE + (2*k*(k + 1))/(nn - k - 1)
  AICc.pi <- AIC.pi + (2*k*(k + 1))/(nn - k - 1)
  
  output <- list("AIC" = AIC, "AIC.g" = AIC.g, "AICc" = AICc,
                 "AIC.MSE" = AIC.MSE, "AICc.MSE" = AICc.MSE,
                 "AIC.pi" = AIC.pi, "AICc.pi" = AICc.pi, "k" = k)
  
  return(output)   
  
}