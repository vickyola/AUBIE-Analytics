library("here")
library("conflicted")
library("readxl")
library("rgdal")
library("tidyverse")
library("ggplot2")
library("ggridges")
library("hrbrthemes")
library("magrittr")
library("viridis")
library("RColorBrewer")
library("tidytext")
library("cowplot")
library("grid")
library("plotly")

#load data:       #read csv file Version 1! version2 would be better, where is it?

bdata <- read.csv(here("bird_classification_v1","bird_classification_v1.csv"), check.names=TRUE,sep=",")

################################################################################
# Plotting######################################################################
################################################################################
################################################################################
#Bubbleplot  #Overview occurrence

 

#Number of detections per site per species 
occuplot <- bdata %>%  dplyr::filter(Rank==1,) %>%
  dplyr::select("Site","Common.Name","permanent_grassland_proportion_class","edge_length_class","date")%>% 
  group_by(Site, Common.Name) %>% mutate(numocc = length((Common.Name)) ) %>%
  distinct(Common.Name , Site, .keep_all= TRUE) %>% 
  ggplot(aes(x = Site, y=reorder_within(x = Common.Name, by = numocc, within=Common.Name, FUN =sum )))+
  geom_point(alpha = 0.5,aes(size = numocc,color=numocc ) )+ 
  scale_color_continuous(type = "viridis",direction = -1)+ 
  scale_size(range = c(1, 24))+
  scale_y_reordered()+
  labs(x ="Site", y= "Common Name", size = "Number of detections", col="Number of detections")+
  theme_minimal()  +
  theme(axis.text=element_text(size=8),legend.position = "bottom")+
  guides(color= guide_legend(), size=guide_legend())+
  ggtitle("Detections per species per site")

occuplot
################################################################################

#Overview grassland proportion class - Mean number of detections in grassland proportion class per species
avgrass <- bdata %>%  dplyr::filter(Rank==1,) %>%
  dplyr::select("Site","Common.Name","permanent_grassland_proportion_class")%>% 
  group_by(Site, Common.Name) %>%
  mutate(numocc = length((Common.Name)) ) %>%
  distinct(Common.Name , Site, .keep_all= TRUE) %>% 
  group_by(Common.Name, permanent_grassland_proportion_class) %>%
  mutate(avgrass = mean(numocc)) %>%  
  distinct(Common.Name , permanent_grassland_proportion_class, .keep_all= TRUE) %>% 
  ggplot(aes(x = permanent_grassland_proportion_class, y=reorder_within(x = Common.Name, by = avgrass, within=Common.Name, FUN =sum ))) +
  geom_point(alpha = 0.3,aes(size = avgrass,color=avgrass ) )+ 
  scale_color_continuous(type = "viridis",direction = -1)+ 
  scale_size(range = c(1, 24))+
  scale_y_reordered()+       
  labs(x ="grassland proportion class", y= "Common Name", size = "Number of detections", col="Number of detections")+
  theme_minimal()  +
  theme(axis.text=element_text(size=8),legend.position = "bottom")+
  guides(color= guide_legend(), size=guide_legend())+
  ggtitle("Detections per species per site") 
avgrass

#Overview edge length class - Mean number of detections in edge length class per species
avedge <-bdata %>%  dplyr::filter(Rank==1,) %>%
  dplyr::select("Site","Common.Name","edge_length_class","date")%>% 
  group_by(Site, Common.Name) %>%
  mutate(numocc = length((Common.Name)) ) %>%
  distinct(Common.Name , Site, .keep_all= TRUE) %>% 
  group_by(Common.Name, edge_length_class) %>%
  mutate(avedge = mean(numocc) ) %>% 
  distinct(Common.Name , edge_length_class, .keep_all= TRUE) %>% 
  ggplot( aes(x = edge_length_class, y=reorder_within(x = Common.Name, by = avedge, within=Common.Name, FUN =sum ))) +
  geom_point(alpha = 0.3,aes(size = avedge,color=avedge ) )+ 
  scale_color_continuous(type = "viridis",direction = -1)+ 
  scale_size(range = c(1, 24))+
  scale_y_reordered()+
  labs(x ="edge length class", y= "Common Name", size = "Number of detections", col="Number of detections")+
  theme_minimal()  +
  theme(axis.text=element_text(size=8),legend.position = "bottom")+
  guides(color= guide_legend(), size=guide_legend())+
  ggtitle("Detections per species per site") 
avedge


################################################################################




