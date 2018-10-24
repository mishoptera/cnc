# Author: Misha Leong
# Date: October 2018
# Project: Exploring urban biodiversity patterns with City Nature Challenge iNaturalist data
# Specificly: These are the functions used to make the individual species patterns tables



# *************************************************************
# FUNCTIONS TO CREATE A GIANT TABLE OF RANKS FOR EACH TAXA
# *************************************************************

# ////////////////////
# Creates a ranked list of the most common species for each city
process_city <- function (hometown1, taxa, nlcd) {
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
    mutate(rank = rank(desc(count), ties.method="random")) %>%
    select(-count) %>%
    rename(!!name_rank := rank)
  
  return(city_taxa)
}

# ////////////////////
# mini table of all the different land use types for each city
create_table_hometown <- function(hometown1, taxa)  {
  n <- process_city(hometown1, taxa, "natural")
  d1 <- process_city(hometown1, taxa, "developed1_open_space")
  d2 <- process_city(hometown1, taxa, "developed2_low_intensity")
  d3 <- process_city(hometown1, taxa, "developed3_medium_intensity")
  d4 <- process_city(hometown1, taxa, "developed4_high_intensity")
  
  
  dummy <- taxa %>%
    filter(hometown == hometown1) %>%
    group_by(scientific_name) %>%
    summarise(count = n()) %>%
    arrange(desc(count)) %>%
    mutate(rank = rank(desc(count), ties.method="random"))
  
  hometown_table <- dummy %>%
    left_join(n, by="scientific_name") %>%
    left_join(d1, by="scientific_name") %>%
    left_join(d2, by="scientific_name") %>%
    left_join(d3, by="scientific_name") %>%
    full_join(d4, by="scientific_name") %>%
    select(-count) %>%
    select(-rank) %>%
    distinct()
}

# ////////////////////
# knit all mini city tables together!
create_big_table <- function(taxa)  {
  
  total  <- taxa %>%
    group_by(scientific_name) %>%
    summarise(count = n()) %>%
    #filter(count>=15)%>%
    arrange(desc(count)) %>%
    mutate(rank = rank(desc(count), ties.method="random"))
  
  Austin  <- create_table_hometown("austin", taxa)
  Boston  <- create_table_hometown("boston", taxa)
  Chicago  <- create_table_hometown("chicago", taxa)
  Dallas  <- create_table_hometown('dallas', taxa)
  Houston  <- create_table_hometown('houston', taxa)
  Los_Angeles  <- create_table_hometown('losangeles', taxa)
  Miami  <- create_table_hometown('miami', taxa)
  Minneapolis  <- create_table_hometown('minneapolis', taxa)
  New_York  <- create_table_hometown('newyork', taxa)
  Raleigh  <- create_table_hometown('raleigh', taxa)
  Salt_Lake_City  <- create_table_hometown('saltlakecity', taxa)
  San_Francisco  <- create_table_hometown('sanfrancisco', taxa)
  Seattle  <- create_table_hometown('seattle', taxa)
  Washington_DC <- create_table_hometown('washingtondc', taxa)
  
  big_table <- total %>%
    left_join(Austin, by="scientific_name") %>%
    left_join(Boston, by="scientific_name") %>%
    left_join(Chicago, by="scientific_name") %>%
    full_join(Dallas, by="scientific_name") %>%
    full_join(Houston, by="scientific_name") %>%
    full_join(Los_Angeles, by="scientific_name") %>%
    full_join(Miami, by="scientific_name") %>%
    full_join(Minneapolis, by="scientific_name") %>%
    full_join(New_York, by="scientific_name") %>%
    full_join(Raleigh, by="scientific_name") %>%
    full_join(Salt_Lake_City, by="scientific_name") %>%
    full_join(San_Francisco, by="scientific_name") %>%
    full_join(Seattle, by="scientific_name") %>%
    full_join(Washington_DC, by="scientific_name")%>%
    distinct()
}



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
