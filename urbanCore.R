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
