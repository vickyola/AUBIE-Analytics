# Load packages.
library("here")
library("dplyr")
library("leaflet")
library("leaflet.extras")
library("leaflegend")
library("rgdal")
library("raster")
library("plotly")
library("ggridges")
library("conflicted")
library("readxl")
library("rgdal")
library("tidyverse")
library("ggplot2")
library("ggridges")
library("hrbrthemes")
library("viridis")
library("RColorBrewer")
library("tidytext")
library("cowplot")
library("grid")

# Load bird detections data and remove ranks not equal to 1.
bdata <- read.csv(here("data","bird_classification_v1.csv"), check.names=TRUE,sep=",") %>%
         dplyr::filter(Rank==1,)

# Plots

## Map of number of detections and sp. richness per site.

### Calculate the spatial indicators.
#### Bird detections by site.
ndet_site <- bdata %>%  
  group_by(Site) %>%
  summarise(counts = n(), latitude = mean(latitude), longitude = mean(longitude))

#### Bird species richness by site.
rich_site <- distinct_at(bdata, vars(Site, Common.Name), .keep_all=TRUE) %>%
  group_by(Site) %>%
  summarise(counts = n(), latitude = mean(latitude), longitude = mean(longitude))

### Map of detections and richness per site (to spatial objects).
coordinates(ndet_site) <- ~ longitude+latitude
projection(ndet_site) <- "+init=epsg:4326"
coordinates(rich_site) <- ~ longitude+latitude
projection(richn_site) <- "+init=epsg:4326"

### Create color palettes.
n = 5
pal_det <- colorQuantile(palette = c("white", "gray", "red"), domain = ndet_site$counts,
                         n = n)
pal_rch <- colorQuantile(palette = c("white", "gray", "red"), domain = rich_site$counts,
                         n = n)
### Create leaflet map.
m <- leaflet(ndet_site) %>%
  
  addCircles(
    radius = 500,
    weight = 0.9,
    opacity = 1,
    fillOpacity = 0.6,
    color = "black",
    stroke = TRUE,
    fillColor = ~pal_det(ndet_site$counts),
    group = "Detections")%>%
  addLegendQuantile(
    pal = pal_det,
    values = ndet_site$counts,
    position = 'topright',
    numberFormat = function(x) {prettyNum(x, big.mark = ',',
                                          scientific = FALSE, digits = 2)},
    shape = 'circle',
    group = "Detections")%>%
  
  addCircles(
    radius = 500,
    weight = 0.9,
    opacity = 1,
    fillOpacity = 0.6,
    color = "black",
    stroke = TRUE,
    fillColor = ~pal_rch(rich_site$counts),
    group = "Richness")%>%
  addLegendQuantile(
    pal = pal_rch,
    values = rich_site$counts,
    position = 'topright',
    numberFormat = function(x) {prettyNum(x, big.mark = ',',
                                          scientific = FALSE, digits = 2)},
    shape = 'circle',
    group = "Richness")%>%
  
  addProviderTiles('Esri.WorldImagery') %>%
  addFullscreenControl() %>%
  addLayersControl(position="topleft",
                   overlayGroups = c("Detections", "Richness"),
                   options = layersControlOptions(collapsed = FALSE)
  )%>%
  hideGroup("Richness") %>%
  addResetMapButton()

m

## Bubble plot of number of detections per site on the (edge length, grassland proportion) plane.
ndet_site <- bdata %>%  
  group_by(Site) %>%
  summarise(Detections = n(), 
            permanent_grassland_proportion = mean(permanent_grassland_proportion), 
            edge_length_m = mean(edge_length_m))

markSize <- function(arr, size = 1){
  size <- 2. * max(arr) / (size ** 2)
  return(size)
}

### Detections
fig <- plot_ly(ndet_site, 
               x = ~permanent_grassland_proportion, 
               y = ~edge_length_m, 
               type = 'scatter',
               mode = 'markers', 
               color = ~Detections,
               colors = 'Reds',
               text = ~Detections,
               marker = list(symbol = 'circle',
                             size = ~Detections, 
                             sizemode = 'diameter',
                             opacity = 0.8,
                             sizeref = markSize(ndet_site$Detections, size=7)))

fig <- fig %>% plotly::layout(title = 'Detections per site',
                              xaxis = list(title = "Permanent grassland proportion (%)", showgrid = TRUE),
                              yaxis = list(title = "Edge length (m)", showgrid = TRUE))

fig

### Bubble plot of species richness per site on the (edge length, grassland proportion) plane.
rich_site <- distinct_at(bdata, vars(Site, Common.Name), .keep_all=TRUE) %>%
  group_by(Site) %>%
  summarise(Richness = n(), latitude = mean(latitude), 
            permanent_grassland_proportion = mean(permanent_grassland_proportion), 
            edge_length_m = mean(edge_length_m))

fig <- plot_ly(rich_site, 
               x = ~permanent_grassland_proportion, 
               y = ~edge_length_m, 
               type = 'scatter',
               mode = 'markers', 
               color = ~Richness,
               colors = 'Reds',
               text = ~Richness,
               marker = list(symbol = 'circle',
                             size = ~Richness, 
                             sizemode = 'diameter',
                             opacity = 0.8,
                             sizeref = markSize(rich_site$Richness, size = 7)))

fig <- fig %>% plotly::layout(title = 'Species richness per site',
                              xaxis = list(title = "Permanent grassland proportion (%)", showgrid = TRUE),
                              yaxis = list(title = "Edge length (m)", showgrid = TRUE))

fig


## Bubble plot of number of detections per site per species. 
ndet_site_sp <- bdata %>%
  dplyr::select("Site","Common.Name","permanent_grassland_proportion_class","edge_length_class","date")%>% 
  group_by(Site, Common.Name) %>% mutate(numocc = length((Common.Name)) ) %>%
  distinct(Common.Name , Site, .keep_all= TRUE) 
  
ggplot(data=ndet_site_sp,aes(x = Site, y=reorder_within(x = Common.Name, by = numocc, within=Common.Name, FUN =sum )))+
  geom_point(alpha = 0.5,aes(size = numocc,color=numocc ) )+ 
  scale_color_continuous(type = "viridis",direction = -1)+ 
  scale_size(range = c(1, 24))+
  scale_y_reordered()+
  labs(x ="Site", y= "", size = "Number of detections", col="Number of detections")+
  theme_minimal()  +
  theme(axis.text=element_text(size=8),legend.position = "bottom")+
  guides(color= guide_legend(), size=guide_legend())+
  ggtitle("Detections per species per site")

## Bubble plot of mean number of detections in grassland proportion class per species. 
meandet_grass <- bdata %>%
  dplyr::select("Site","Common.Name","permanent_grassland_proportion_class")%>% 
  group_by(Site, Common.Name) %>%
  mutate(numocc = length((Common.Name)) ) %>%
  distinct(Common.Name , Site, .keep_all= TRUE) %>% 
  group_by(Common.Name, permanent_grassland_proportion_class) %>%
  mutate(avgrass = mean(numocc)) %>%  
  distinct(Common.Name , permanent_grassland_proportion_class, .keep_all= TRUE)

ggplot(data = meandet_grass, aes(x = permanent_grassland_proportion_class, y=reorder_within(x = Common.Name, by = avgrass, within=Common.Name, FUN =sum ))) +
  geom_point(alpha = 0.3,aes(size = avgrass,color=avgrass ) )+ 
  scale_color_continuous(type = "viridis",direction = -1)+ 
  scale_size(range = c(1, 24))+
  scale_y_reordered()+       
  labs(x ="grassland proportion class", y= "", size = "Number of detections", col="Number of detections")+
  theme_minimal()  +
  theme(axis.text=element_text(size=8),legend.position = "bottom")+
  guides(color= guide_legend(), size=guide_legend())+
  ggtitle("Detections per species per site") 


## Overview edge length class - Mean number of detections in edge length class per species.
meandet_edge <-bdata %>%  dplyr::filter(Rank==1,) %>%
  dplyr::select("Site","Common.Name","edge_length_class","date")%>% 
  group_by(Site, Common.Name) %>%
  mutate(numocc = length((Common.Name)) ) %>%
  distinct(Common.Name , Site, .keep_all= TRUE) %>% 
  group_by(Common.Name, edge_length_class) %>%
  mutate(avedge = mean(numocc) ) %>% 
  distinct(Common.Name , edge_length_class, .keep_all= TRUE) %>% 

ggplot(data = meandet_edge, aes(x = edge_length_class, y=reorder_within(x = Common.Name, by = avedge, within=Common.Name, FUN =sum ))) +
  geom_point(alpha = 0.3,aes(size = avedge,color=avedge ) )+ 
  scale_color_continuous(type = "viridis",direction = -1)+ 
  scale_size(range = c(1, 24))+
  scale_y_reordered()+
  labs(x ="edge length class", y= "", size = "Number of detections", col="Number of detections")+
  theme_minimal()  +
  theme(axis.text=element_text(size=8),legend.position = "bottom")+
  guides(color= guide_legend(), size=guide_legend())+
  ggtitle("Detections per species per site") 


## Density plots per species (ridgeline plots).

### Density plots of grassland proportion per species.
ridge_site <- distinct_at(bdata, vars(Site), .keep_all=TRUE)
gprop_mean <- group_by(bdata, Common.Name) %>%
  summarise(meanprop = mean(permanent_grassland_proportion)) %>%
  arrange(-meanprop)

sps <- gprop_mean$Common.Name
ridge_site_aux <- bdata[which(bdata$Common.Name %in% sps),]
ridge_site_aux$Common.Name <- factor(ridge_site_aux$Common.Name,
                                     levels = rev(sps))
ggplot(ridge_site_aux,
       aes(x = permanent_grassland_proportion, y = Common.Name, fill = "darkred")) +
  geom_density_ridges(fill = "dark red", alpha = 0.6) +
  theme_ridges() + 
  xlim(0, 50)+
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)) +
  xlab("Permanent grassland proportion (%)") + 
  ylab(" ")

### Density plots of edge length per species.
edge_site <- distinct_at(bata, vars(Site), .keep_all=TRUE)
gprop_mean <- group_by(bdata, Common.Name) %>%
  summarise(meanprop = mean(edge_length_m)) %>%
  arrange(-meanprop)

sps <- gprop_mean$Common.Name
edge_site_aux <- bdata[which(bdata$Common.Name %in% sps),]
edge_site_aux$Common.Name <- factor(edge_site_aux$Common.Name,
                                    levels = rev(sps))
ggplot(edge_site_aux,
       aes(x = edge_length_m, y = Common.Name, )) +
  geom_density_ridges(fill = "dark green", alpha = 0.7) +
  theme_ridges() + 
  xlim(580, 13025)+
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)) +
  xlab("Edge length (m)") + 
  ylab(" ")

