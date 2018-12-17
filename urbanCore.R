# Author: Misha Leong
# Date: December 2018
# Project: Exploring urban biodiversity patterns with City Nature Challenge iNaturalist data
# Specificly: Work in progress to better pull out who urban specialists are

# *************************************************************
# BIOTIC HOMOGENIZATION ACROSS CITIES
# *************************************************************
# Widespread speicies
# 1. which species are found in 8 or more cities?
# 2. What proportion of total species and observations do these make up?
# 3. Which taxa are these based in?
# pulling out total species richness and observation counts for later usage

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


# okay I already have #1 done with create_big_table_simple function.
# the trick for me is to figure out which species that have a ranking of over 20
# are also found on the top 20 lists for other cities.

# already have and can work with a version of big_simple_ranks!
big_simple_ranks2 <- simple_birds %>%
  bind_rows(simple_mammals, simple_reptiles, simple_amphibians, simple_gastropods, simple_insects, simple_dicots, simple_monocots, simple_ferns, simple_conifers) %>%
  left_join(names, by="scientific_name") %>%
  left_join(total_cities, by="scientific_name") %>%
  distinct(scientific_name, .keep_all = TRUE) %>%
  select(taxon, common_name, scientific_name, rank, num_cities, contains("rank")) %>%
  rename(overall_rank = rank) %>%
  gather("city", "city_rank", austin_rank:washingtondc_rank) %>%
  filter(city_rank <= 10) %>%
  group_by(taxon, common_name, scientific_name) %>%
  mutate (top10_count = n()) %>%
  filter (num_cities > 3) %>%
  filter (top10_count == 1)

View(big_simple_ranks2)
  


# *************************************************************
# LIST COMPARISON BETWEEN UBIQUITOUS AND UNIQUE
# *************************************************************
# 1. create a rank of the top 100 observed species for each land cover type for each city
# 2. Which species are found in all land cover types?
# 3. Which species are only found in d1-d4?
# 4. Which species are only found in d0-d1?

## here we go!
process_city_list <- function (hometown1, taxa, nlcd) {
  name_rank = paste(if_else (nlcd == "natural", "n", 
                             if_else (nlcd == "developed1_open_space", "d1",  
                                      if_else (nlcd == "developed2_low_intensity", "d2", 
                                               if_else (nlcd == "developed3_medium_intensity", "d3","d4")))), hometown1, sep = "_")
  
  
  city_taxa <- taxa %>%
    filter(hometown == hometown1) %>%
    filter(nlcd_group2 == nlcd) %>%
    group_by(scientific_name) %>%
    summarise(count = n()) %>%
    arrange(desc(count)) %>%
    mutate(rank = rank(desc(count), ties.method="average")) %>%
    select(-count) %>%
    rename(!!name_rank := rank)
  return(city_taxa)
}

names <- all_wfreq %>%
  select(scientific_name:common_name) %>%
  unique()

process_city_core <- function(hometown1, taxa)  {
  n <- process_city_list(hometown1, taxa, "natural")
  d1 <- process_city_list(hometown1, taxa, "developed1_open_space")
  d2 <- process_city_list(hometown1, taxa, "developed2_low_intensity")
  d3 <- process_city_list(hometown1, taxa, "developed3_medium_intensity")
  d4 <- process_city_list(hometown1, taxa, "developed4_high_intensity")
  
  everywhere <- n %>%
    inner_join(d1, by="scientific_name") %>%
    inner_join(d2, by="scientific_name") %>%
    inner_join(d3, by="scientific_name") %>%
    inner_join(d4, by="scientific_name")%>%
    left_join(names, by="scientific_name") %>%
    distinct(scientific_name, .keep_all = TRUE) %>%
    select(common_name, everything())
  everywhere
  
  everywhere_butn <- d1 %>%
    inner_join(d2, by="scientific_name") %>%
    inner_join(d3, by="scientific_name") %>%
    inner_join(d4, by="scientific_name")%>%
    left_join(names, by="scientific_name") %>%
    distinct(scientific_name, .keep_all = TRUE) %>%
    select(common_name, everything())
  everywhere_butn
  
  natural_sp <- n %>%
    full_join(d1, by="scientific_name") %>%
    gather("lc", "rank", 2:3) %>%
    filter(rank<=100)
  
  urban_sp <- d2 %>%
    full_join(d3, by="scientific_name") %>%
    full_join(d4, by="scientific_name") %>%
    gather("lc", "rank", 2:4) %>%
    filter(rank<=100)
  
  only_natural_sp <- natural_sp %>%
    anti_join(urban_sp, by="scientific_name") %>%
    left_join(names, by="scientific_name") %>%
    distinct(scientific_name, .keep_all = TRUE) %>%
    select(common_name, everything())
  
  only_urban_sp <- urban_sp %>%
    anti_join(natural_sp, by="scientific_name") %>%
    left_join(names, by="scientific_name") %>%
    distinct(scientific_name, .keep_all = TRUE) %>%  # fixes problem of multiple common names
    select(common_name, everything()) %>%
    arrange(rank)
  
  return(only_urban_sp)
}

Austin  <- process_city_core("austin", taxa)
Boston  <- process_city_core("boston", taxa)
Chicago  <- process_city_core("chicago", taxa)
Dallas  <- process_city_core('dallas', taxa)
Houston  <- process_city_core('houston', taxa)
Los_Angeles  <- process_city_core('losangeles', taxa)
Miami  <- process_city_core('miami', taxa)
Minneapolis  <- process_city_core('minneapolis', taxa)
New_York  <- process_city_core('newyork', taxa)
Raleigh  <- process_city_core('raleigh', taxa)
Salt_Lake_City  <- process_city_core('saltlakecity', taxa)
San_Francisco  <- process_city_core('sanfrancisco', taxa)
Seattle  <- process_city_core('seattle', taxa)
Washington_DC <- process_city_core('washingtondc', taxa)

all_urban <- Austin %>%
  bind_rows(Boston, Chicago, Dallas, Houston, Los_Angeles, Miami, Minneapolis,
            New_York, Raleigh, Salt_Lake_City, San_Francisco, Seattle, Washington_DC) %>%
  group_by(scientific_name) %>%
  summarise(num_cities = n()) %>%
  arrange(desc(num_cities)) %>%
  left_join(names, by="scientific_name") %>%
  distinct(scientific_name, .keep_all = TRUE) %>%
  select(common_name, everything()) %>%
  filter(num_cities > 4)
all_urban



