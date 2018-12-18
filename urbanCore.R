# Author: Misha Leong
# Date: December 2018
# Project: Exploring urban biodiversity patterns with City Nature Challenge iNaturalist data
# Specificly: Work in progress to better pull out who urban specialists are

# *************************************************************
# BIOTIC HOMOGENIZATION ACROSS CITIES
# *************************************************************
# Widespread species

# what are the total species numbers and observations in the dataset
totals <- all_inat %>%
  summarise (num_species = n_distinct (scientific_name),
             num_obs = n())
totals$num_species
totals$num_obs

# for later use to match common names to scientific names
names <- all_inat %>%
  select(scientific_name:common_name) %>%
  unique()

# creating a way to see the number of cities all species were found in
over8 <- all_inat %>%
  group_by (taxon, scientific_name) %>%
  summarise (num_cities = n_distinct(hometown), num_obs = n()) %>%
  left_join(names, by="scientific_name") %>%
  distinct(scientific_name, .keep_all = TRUE) %>%
  select(taxon, common_name, scientific_name, num_cities, num_obs) %>%
  filter(num_cities > 7) %>%
  arrange(desc(num_cities))
over8
write.csv(over8, "figures_n_tables/over8.csv")

# Comparisons between over8 with larger pool grouped by taxon group
# This is the larger pool
all_stats <- all_inat %>%
  group_by (taxon) %>%
  summarise (total_species = n_distinct(scientific_name), total_obs = n())
all_stats

# this is to create blank variables for the taxon groups that do not have any species in >8 cities
blanks <- tibble(taxon = c("gastropods", "ferns"), 
                 over8_species = c(0, 0), 
                 over8_obs = c(0, 0))

# nice final data with the data all togther
taxon_over8 <- over8 %>% 
  group_by(taxon) %>%
  summarise (over8_species = n_distinct(scientific_name), over8_obs = sum(num_obs)) %>%
  bind_rows(blanks) %>%
  left_join(all_stats, by = "taxon") %>%
  mutate(prop_sp = (over8_species/total_species)*100, prop_obs = (over8_obs/total_obs)*100) %>%
  arrange(taxon)
write.csv(taxon_over8, "figures_n_tables/taxon_over8.csv")


###################################################
# CALCULATING SLOPES BASED ON CAM AND ARM
###################################################

# a function to calculate the slope of the n:d4 points!
get_slope <- function(n, d1, d2, d3, d4) { 
  x <- c(1:5)
  y <- c(n, d1, d2, d3, d4)
  slope_result <- lm(y~x, na.action=na.exclude)$coeff[[2]]
  return(slope_result)
}

# creating a table of the arm and cam slopes
slopes <- big_over100obs %>%
  rowwise() %>%  #this is such an important thing!!!
  mutate(slope_cam = get_slope(n,d1, d2, d3, d4)) %>%
  mutate(slope_arm = get_slope(n.mean,d1.mean, d2.mean, d3.mean, d4.mean)) %>%
  ungroup() 

# plotting the cam slopes
cam_labels_over <- slopes %>%
  filter(slope_cam >0) %>%
  filter(num_cities>7)
cam_labels_under <- slopes %>%
  filter(slope_cam < -1) %>%
  filter(num_cities>7)
plot_cam <- ggplot(data=slopes,aes(x=num_cities,y=slope_cam, colour=taxon))+
  geom_point() + 
  labs(title = "evaluated with City Aggregation Metric", x = "number of cities", y = "slope of CAM") +
  geom_text_repel(data = cam_labels_over, aes(x=num_cities, y=slope_cam, label = common_name)) + 
  geom_text_repel(data = cam_labels_under, aes(x=num_cities, y=slope_cam, label = common_name)) + 
  theme_bw() 
plot_cam

# plotting the arm slopes
arm_labels_over <- slopes %>%
  filter(slope_arm < -2) %>%
  filter(num_cities>7)
arm_labels_under <- slopes %>%
  filter(slope_arm > 0 ) %>%
  filter(num_cities>7)
plot_arm <- ggplot(data=arm_slopes,aes(x=num_cities,y=slope_arm, colour=taxon))+
  geom_point() + 
  geom_text_repel(data = arm_labels_over, aes(x=num_cities, y=slope_arm, label = common_name)) + 
  geom_text_repel(data = arm_labels_under, aes(x=num_cities, y=slope_arm, label = common_name)) + 
  labs(title = "evaluated with Averaged Ranking Metric", x = "number of cities", y = "slope of ARM") +
  theme_bw()+
  scale_y_reverse(lim=c(8, -8))
plot_arm

# Combine the cam and arm slopes into one lovely figure and save
plots <- ggarrange(plot_cam, plot_arm, labels = c("A", "B"), ncol = 1, nrow = 2)
plots <- annotate_figure(plots,
                         top = text_grob("Biotic homogenization with urbanization intensity", face = "bold", size = 18))
ggsave(plot = plots, filename = "figures_n_tables/bh_CAM_ARM.jpg", height = 24, width = 20, units = "cm")

