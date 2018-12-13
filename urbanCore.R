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
totals <- plants %>% 
  union (animals) %>%
  summarise (num_species = n_distinct (scientific_name),
             num_obs = n())
total_species <- totals$num_species
total_obs <- totals$num_obs

names <- all_wfreq %>%
  select(scientific_name:common_name) %>%
  unique()

over8_obs <-dicots %>% 
  union (monocots) %>%
  union (ferns) %>%
  union (conifers) %>%
  union (birds) %>%
  union (insects) %>%
  union (reptiles) %>%
  union (amphibians) %>%
  union (mammals) %>%
  union (gastropods) %>%
  group_by (scientific_name) %>%
  mutate (num_cities = n_distinct(hometown)) %>%
  filter(num_cities > 7) %>%
  summarise(num_obs = n())%>%
  arrange(desc(num_obs))
over8_obs
total_over8obs <- sum(over8_obs$num_obs)

numberCities <-dicots %>% 
  union (monocots) %>%
  union (ferns) %>%
  union (conifers) %>%
  union (birds) %>%
  union (insects) %>%
  union (reptiles) %>%
  union (amphibians) %>%
  union (mammals) %>%
  union (gastropods) %>%
  group_by (taxon, scientific_name) %>%
  summarise (num_cities = n_distinct(hometown)) %>%
  left_join(names, by="scientific_name") %>%
  distinct(scientific_name, .keep_all = TRUE) %>%
  select(taxon, common_name, scientific_name, num_cities) %>%
  arrange(desc(num_cities))

over8 <- numberCities %>%
  filter(num_cities > 7) %>%
  left_join(over8_obs, by = "scientific_name")
over8
write.csv(over8, "figures_n_tables/over8.csv")

# a function to calculate the slope of the n:d4 points!
get_slope <- function(n, d1, d2, d3, d4) { 
  x <- c(1:5)
  y <- c(n, d1, d2, d3, d4)
  slope_result <- lm(y~x, na.action=na.exclude)$coeff[[2]]
  return(slope_result)
  }
test <- big_over100obs %>%
  rowwise() %>%  #this is such an important thing!!!
  mutate(slope_cam = get_slope(n,d1, d2, d3, d4)) %>%
  mutate(slope_arm = get_slope(n.mean,d1.mean, d2.mean, d3.mean, d4.mean)) %>%
  ungroup() 

test_labels_cam <- test %>%
  filter(slope_cam >0) %>%
  filter(num_cities>7)

test_labels_cam2 <- test %>%
  filter(slope_cam < -1) %>%
  filter(num_cities>7)

test_labels_arm <- test %>%
  filter(slope_arm < -2) %>%
  filter(num_cities>7)

test_labels_arm2 <- test %>%
  filter(slope_arm > 0 ) %>%
  filter(num_cities>7)

plot_cam <- ggplot(data=test,aes(x=num_cities,y=slope_cam, colour=taxon))+
  geom_point() + 
  labs(title = "evaluated with City Aggregation Metric", x = "number of cities", y = "slope of CAM") +
  geom_text_repel(data = test_labels_cam, aes(x=num_cities, y=slope_cam, label = common_name)) + 
  geom_text_repel(data = test_labels_cam2, aes(x=num_cities, y=slope_cam, label = common_name)) + 
  theme_bw() 
plot_cam

plot_arm <- ggplot(data=test,aes(x=num_cities,y=slope_arm, colour=taxon))+
  geom_point() + 
  geom_text_repel(data = test_labels_arm, aes(x=num_cities, y=slope_arm, label = common_name)) + 
  geom_text_repel(data = test_labels_arm2, aes(x=num_cities, y=slope_arm, label = common_name)) + 
  labs(title = "evaluated with Averaged Ranking Metric", x = "number of cities", y = "slope of ARM") +
  theme_bw()+
  scale_y_reverse(lim=c(8, -8))
plot_arm

# Combine into one lovely figure and save
plots <- ggarrange(plot_cam, plot_arm, labels = c("A", "B"), ncol = 1, nrow = 2)
plots <- annotate_figure(plots,
                         top = text_grob("Biotic homogenization with urbanization intensity", face = "bold", size = 18))
ggsave(plot = plots, filename = "figures_n_tables/bh_CAM_ARM.jpg", height = 24, width = 20, units = "cm")

test_plot_armD <- ggplot(data=test %>%filter(taxon=="dicots"),aes(x=num_cities,y=slope_arm, colour=taxon))+
  geom_point() + 
  geom_text_repel(data = test_labels_arm%>%filter(taxon=="dicots"), aes(x=num_cities, y=slope_arm, label = common_name)) + 
  theme_bw()+
  scale_y_reverse()
test_plot_armD

  
mean(test$slope_cam)
mean(over8_wslopes$slope_cam)


numberCities_plot <- ggplot(data=numberCities, aes(num_cities, fill = taxon, colour = taxon)) +
  stat_count(width=0.9) +
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank())  #remove minor-grid labels
numberCities_plot
  
over8_plot <- ggplot(data=over8, aes(num_cities, fill = taxon, colour = taxon)) +
  stat_count(width=0.9) +
  theme(legend.position="none",
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank())  #remove minor-grid labels)
  
over8_plot
  
# Insert xbp_grob inside the scatter plot
over8_grob <- ggplotGrob(over8_plot)
numberCities_plot + annotation_custom(grob = over8_grob, xmin = 5, xmax = 14, 
                       ymin = 700, ymax = 2500)


# 
# Top 10 rankings
# 1. create a taxa list for each city top 10.
# 2. How many taxa are unique to just one city versus found in over 8?
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
# FIGURES ILLUSTRATING THE CAM AND ARM METRICS
# *************************************************************
# subset of big everything that pulls out urban specialists
big_urban_arm <- big_everything %>%
  arrange(diff_arm, diff_cam) %>%
  filter(count>=50) %>%
  filter(taxon != "dicots") %>%
  filter(diff_arm < -20)%>%
  rename(d0.mean = n.mean) %>%
  gather("lc_arm", "arm", d0.mean:d4.mean)
bua <- ggplot(data=big_urban_arm,aes(x=lc_arm,y=arm, colour=common_name, group=common_name))+
  geom_point() + 
  geom_line () +
  theme_bw() +
  scale_y_reverse( lim=c(50,0))
bua

big_natural_arm <-  big_everything %>%
  arrange(desc(diff_cam, diff_arm))  %>%
  filter(count>=50) %>%
  filter(taxon != "dicots") %>%
  filter(diff_arm > 10)%>%
  rename(d0.mean = n.mean) %>%
  gather("lc_arm", "arm", d0.mean:d4.mean)
bna <- ggplot(data=big_natural_arm,aes(x=lc_arm,y=arm, colour=taxon, group=common_name))+
  geom_point() + 
  geom_line () +
  theme_bw() +
  scale_y_reverse( lim=c(50,0))
bna

big_urban_cam <- big_everything %>%
  arrange(diff_cam, diff_arm) %>%
  filter(count>=50) %>%
  filter(taxon != "dicots") %>%
  filter(diff_cam < -1) %>%
  rename(d0 = n) %>%
  gather("lc_cam", "cam", d0:d4)
buc <- ggplot(data=big_urban_cam,aes(x=lc_cam,y=cam, colour=common_name, group=common_name))+
  geom_point() + 
  geom_line() +
  theme_bw()
buc

# subset of big_everything that pulls out the natural areas specialists
big_natural_cam <- big_everything %>%
  arrange(diff_cam, diff_arm) %>%
  filter(count>=50) %>%
  filter(taxon != "dicots") %>%
  filter(diff_cam > 7) %>%
  rename(d0 = n) %>%
  gather("lc_cam", "cam", d0:d4)
bnc <- ggplot(data=big_natural_cam,aes(x=lc_cam,y=cam, colour=common_name, group=common_name))+
  geom_point() + 
  geom_line() +
  theme_bw()
bnc

# Same as the above but with dicots only
big_dicots_urban_arm <- big_everything %>%
  arrange(diff_arm, diff_cam) %>%
  filter(count>=50) %>%
  filter(taxon == "dicots") %>%
  filter(diff_arm < -50)%>%
  rename(d0.mean = n.mean) %>%
  gather("lc_arm", "arm", d0.mean:d4.mean)
bdua <- ggplot(data=big_dicots_urban_arm,aes(x=lc_arm,y=arm, colour=taxon, group=common_name))+
  geom_point() + 
  geom_line () +
  theme_bw() +
  scale_y_reverse( lim=c(50,0))
bdua

big_dicots_natural_arm <-  big_everything %>%
  arrange(desc(diff_cam, diff_arm))  %>%
  filter(count>=50) %>%
  filter(taxon == "dicots") %>%
  filter(diff_arm > 2)%>%
  rename(d0.mean = n.mean) %>%
  gather("lc_arm", "arm", d0.mean:d4.mean)
bdna <- ggplot(data=big_dicots_natural_arm,aes(x=lc_arm,y=arm, colour=taxon, group=common_name))+
  geom_point() + 
  geom_line () +
  theme_bw() +
  scale_y_reverse( lim=c(50,0))
bdna

big_dicots_urban_cam <- big_everything %>%
  arrange(diff_cam, diff_arm) %>%
  filter(count>=50) %>%
  filter(taxon == "dicots") %>%
  filter(diff_cam < -1) %>%
  rename(d0 = n) %>%
  gather("lc_cam", "cam", d0:d4)
bduc <- ggplot(data=big_dicots_urban_cam,aes(x=lc_cam,y=cam, colour=common_name, group=common_name))+
  geom_point() + 
  geom_line() +
  theme_bw()
bduc

big_dicots_natural_cam <- big_everything %>%
  arrange(diff_cam, diff_arm) %>%
  filter(count>=50) %>%
  filter(taxon == "dicots") %>%
  filter(diff_cam > 7) %>%
  rename(d0 = n) %>%
  gather("lc_cam", "cam", d0:d4)
bdnc <- ggplot(data=big_dicots_natural_cam,aes(x=lc_cam,y=cam, colour=common_name, group=common_name))+
  geom_point() + 
  geom_line() +
  theme_bw()
bdnc




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



