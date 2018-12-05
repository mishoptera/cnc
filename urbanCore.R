# Author: Misha Leong
# Date: December 2018
# Project: Exploring urban biodiversity patterns with City Nature Challenge iNaturalist data
# Specificly: Work in progress to better pull out who urban specialists are


# *************************************************************
# FIGURES ILLUSTRATING THE CAM AND ARM METRICS
# *************************************************************
# subset of big everything that pulls out urban specialists
big_urban_arm <- big_everything %>%
  arrange(diff_arm, diff_cam) %>%
  filter(count>=50) %>%
  filter(taxon != "dicots") %>%
  filter(diff_arm < -5)%>%
  rename(d0.mean = n.mean) %>%
  gather("lc_arm", "arm", d0.mean:d4.mean)
bua <- ggplot(data=big_urban_arm,aes(x=lc_arm,y=arm, colour=taxon, group=common_name))+
  geom_point() + 
  geom_line () +
  theme_bw() +
  scale_y_reverse( lim=c(50,0))
bua

big_natural_arm <-  big_everything %>%
  arrange(desc(diff_cam, diff_arm))  %>%
  filter(count>=50) %>%
  filter(taxon != "dicots") %>%
  filter(diff_arm > 2)%>%
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
    inner_join(d4, by="scientific_name")
  everywhere
  
  natural_sp <- n %>%
    full_join(d1, by="scientific_name") %>%
    gather("lc", "rank", 2:3) %>%
    filter(rank<50)
  
  urban_sp <- d2 %>%
    full_join(d3, by="scientific_name") %>%
    full_join(d4, by="scientific_name") %>%
    gather("lc", "rank", 2:4) %>%
    filter(rank<50)
  
  only_natural_sp <- natural_sp %>%
    anti_join(urban_sp, by="scientific_name") %>%
    full_join(names, by="scientific_name") %>%
    distinct(scientific_name, .keep_all = TRUE) %>%
    select(common_name, everything())
  
  only_urban_sp <- urban_sp %>%
    anti_join(natural_sp, by="scientific_name") %>%
    full_join(names, by="scientific_name") %>%
    distinct(scientific_name, .keep_all = TRUE) %>%  # fixes problem of multiple common names
    select(common_name, everything())
  
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
  full_join(names, by="scientific_name") %>%
  distinct(scientific_name, .keep_all = TRUE) %>%
  select(common_name, everything()) %>%
  filter(num_cities > 3)
all_urban



