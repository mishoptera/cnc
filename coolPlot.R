library(devtools)
install_github("hms-dbmi/UpSetR")
library(UpSetR)

movies <- read.csv(system.file("extdata", "movies.csv", package = "UpSetR"), 
                   header = T, sep = ";")
cnc <- read.csv("figures_n_tables/upset_test.csv")

upset(movies, nsets = 6, number.angles = 30, point.size = 3.5, line.size = 2, 
      mainbar.y.label = "Genre Intersections", sets.x.label = "Movies Per Genre", 
      text.scale = c(1.3, 1.3, 1, 1, 2, 0.75))

upset(movies, sets = c("Action", "Adventure", "Comedy", "Drama", "Mystery", 
                       "Thriller", "Romance", "War", "Western"), mb.ratio = c(0.55, 0.45), order.by = "freq")

upset(cnc, sets = c("n", "d1", "d2", "d3", "d4", 
                       "d5"), mb.ratio = c(0.55, 0.45), order.by = "freq")


#?????????????????

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
  all_matrix <- cc_matrix(birds)
  all_env <- cc_env(all_matrix)
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
  species_scores <- as.data.frame(scores(mod, "species"))
  species_scores$common_name <- rownames(species_scores)
  species_scores_subset <- species_scores %>%
    left_join(big_everything, by = "common_name") %>%
    filter(count>=100) %>%
    select(NMDS1, NMDS2, species)
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
  
  species <- ggplot()+
    geom_point(data=data_scores,aes(x=NMDS1,y=NMDS2,shape=region, col=urbanization),size=3) + 
    labs (shape = "Regions", colour = "Urbanization Levels", fill = "Urbanization Levels") +
    labs(title = "Land cover groupings", subtitle = subtitle) +
    stat_chull(data=data_scores, geom = "polygon", alpha = 0.1, aes(x=NMDS1,y=NMDS2,
                                                                    fill=urbanization, color=urbanization)) +
    geom_text_repel(data=species_scores,aes(x=NMDS1,y=NMDS2,label=species),alpha=0.5) +
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
}
