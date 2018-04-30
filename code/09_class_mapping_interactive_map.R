# Filename: 09-class_mapping_interactive.R (2017-09-14)
#
# TO DO: map kmeans classes
#
# Author(s): Jannes Muenchow, Eric Krueger
#
#**********************************************************
# CONTENTS-------------------------------------------------
#**********************************************************
#
# 1. ATTACH PACKAGES AND DATA
# 2.
#
#**********************************************************
# 1 ATTACH PACKAGES AND DATA-------------------------------
#**********************************************************

# attach packages
pacman::p_load(sp,spData,sf, lattice,latticeExtra, tidyverse)


# define directories
dir_main = "."
dir_data = file.path(dir_main, "data")
dir_ima = file.path(dir_main, "images")
dir_figs = file.path(dir_main, "figures")

# attach data
load(file.path(dir_ima, "07_classes.Rdata"))
wos <- readRDS("images/00_wos.rds")
mat <- readRDS("images/07_mat.rds")
qual <- readRDS("images/00_qual.rds")

#**********************************************************
# 4 MAPPING CLUSTERS---------------------------------------
#**********************************************************

# rownames correspond to idCitavi
clus = data.frame(cluster = classes$cluster)
clus$idCitavi = as.numeric(rownames(clus))
# sum(rownames(clus) == rownames(mat)) == nrow(mat)  # TRUE, perfect
setdiff(clus$idCitavi, qual$fidCitavi)
# 13 manuscripts without an abstract
setdiff(qual$fidCitavi, clus$idCitavi)
clus = dplyr::inner_join(clus,
                         dplyr::select(qual, fid_citavi, lat = Latitude,
                                       lon = Longtitude),
                         by = c("idCitavi" = "fid_citavi"))
head(clus)
clus[clus$lon > 180 | clus$lon < -180, ]
clus[clus$lon > 180 | clus$lon < -180, "lon"] = -12.31

# check lat/lon data
dim(clus[clus$lon == 0 & clus$lat == 0, ])  # 100 studies without coordinates
# remove them
clus = clus[!(clus$lon == 0 & clus$lat == 0), ]

#plot(st_geometry(world), ylim = c(-30, 30), xlim = c(-140, 140))
points(clus$lon, clus$lat, pch = 16, col = clus$cluster)
# check if there were really studies in Hawaii and Tahiti

ind = filter(clus, lon < -129 & lon > -160) %>% dplyr::pull(idCitavi)
filter(qual, qual$fidCitavi %in% ind)
filter(wos, wos$idCitavi %in% c(109, 448))
# ok, first article is located in Hawaii, second in Polyesia, perfect
colnames(clus)[2] <- "id_citavi"
x = clus[, c("lon", "lat", "cluster")]
coordinates(clus) =~ lon + lat


#**********************************************************
# 5 Interactive MAPPING CLUSTERS---------------------------
#**********************************************************

library(leaflet)

##data-----
clus_con <- left_join(st_as_sf(clus),wos)

clus_con <- st_as_sf(clus_con)

clus_con$cluster_name[clus_con$cluster == 1] <- "UI cluster"
clus_con$cluster_name[clus_con$cluster == 2] <- "EL cluster"
clus_con$cluster_name[clus_con$cluster == 3] <- "MT cluster"
clus_con$cluster_name[clus_con$cluster == 4] <- "PC cluster"

###popups
clus_content <- paste(
  "<b>","Titel",": ","</b>", clus_con$title,
  "</br>",
  "<b>Author(s): </b>",
  clus_con$author,"</br>",
  "<b>Year: </b>", clus_con$year, "</br>",
  "<b>Cluster: </b>", clus_con$cluster_name)

pal = RColorBrewer::brewer.pal("Set3", n =  6)[3:6]
##leaflet
leaflet(clus_con) %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.PositronNoLabels, group = "Dark") %>%
  addCircleMarkers(st_coordinates(clus_con)[,1],st_coordinates(clus_con)[,2],
                   popup = as.character(clus_content), color = ~pal, radius =  4) %>%
  addLegend("bottomright",
            colors =c("#BEBADA", "#FB8072", "#80B1D3", "#FDB462"),
                        labels= c("UI cluster","PC cluster",
                                  "EL cluster","MT Cluster"),
                        title= "Clusters",
                        opacity = 1)


