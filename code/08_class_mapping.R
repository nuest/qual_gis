# Filename: 02-class_mapping.R (2017-09-14)
#
# TO DO: map kmeans classes
#
# Author(s): Jannes Muenchow
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
library("sp")
library("spData")
library("sf")
library("lattice")
library("latticeExtra")
library("RODBC")
library("RPostgreSQL")

# define directories
dir_main = "D:/uni/science/projects/computing/qual_gis/review"
dir_data = file.path(dir_main, "data")
dir_ima = file.path(dir_main, "images")
dir_figs = file.path(dir_main, "figures")

# attach data
load(file.path(dir_ima, "01-mat.Rdata"))
load(file.path(dir_ima, "01-classes.Rdata"))

# connect to Access
con = odbcConnectAccess2007(file.path(dir_data, "Database_WOS.accdb"))
sqlTables(con)
# retrieve table with all the abstracts
qual = sqlFetch(con, "tblQUAL_GIS", as.is = TRUE)  # 
wos = sqlFetch(con, "tblWOS", as.is = TRUE)  
# idCitavi -> tblQualGIS
# WOS connecting tblWOS and tblAbstract
odbcClose(con)

drv <- dbDriver("PostgreSQL")
# note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = "mreolgsw",        
                 # change con to elephantsql database
                 host = "horton.elephantsql.com", port = 5432,
                 user = "mreolgsw", 
                 password = "VOvgCnJaQuFBr5dZknPbyDDO1vcUpfnW")
RPostgreSQL::dbListTables(conn = con)
qual = dbGetQuery(con, "SELECT * FROM main_qual_gis")
wos = dbGetQuery(con, "SELECT * FROM wos")  
dbDisconnect(conn = con)

#**********************************************************
# 4 MAPPING CLUSTERS---------------------------------------
#**********************************************************

# rownames correspond to idCitavi
clus = data.frame(cluster = classes$cluster)
clus$idCitavi = as.numeric(rownames(clus))
# sum(rownames(clus) == rownames(mat)) == nrow(mat)  # TRUE, perfect
# ERIC: WhHY ARE THESE IDS MISSING FROM QUAL?????????????
setdiff(clus$idCitavi, qual$fidCitavi)
# setdiff(qual$fidCitavi, clus$idCitavi)
clus = dplyr::inner_join(clus, 
                  dplyr::select(qual, fidCitavi, lat = Latitude,
                                lon = Longtitude),
                  by = c("idCitavi" = "fidCitavi"))
head(clus)
clus[clus$lon > 180 | clus$lon < -180, ]
clus[clus$lon > 180 | clus$lon < -180, "lon"] = -12.31
# 136 studies with 0, 0 coordinates, i.e. NAs... (ask ERIC!!!!)
dim(clus[clus$lon == 0 & clus$lat == 0, ])
# remove them
clus = clus[!(clus$lon == 0 & clus$lat == 0), ]

plot(st_geometry(world), ylim = c(-30, 30), xlim = c(-140, 140))
points(clus$lon, clus$lat, pch = 16, col = clus$cluster)
x = clus[, c("lon", "lat", "cluster")]
coordinates(clus) =~ lon + lat
pal = RColorBrewer::brewer.pal("Set3", n =  6)[3:6]
world = spTransform(as(world, "Spatial"), "+proj=wintri")
proj4string(clus) = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
clus = spTransform(clus, "+proj=wintri")
clus@data$cluster = as.factor(clus@data$cluster)

# this draws a "bar" legend, but I would prefer points...
spplot(clus, "cluster", col.regions = pal, border = "black", cex = 0.75,
       colorkey = list(
         right = list( # see ?levelplot in package trellis, argument colorkey:
           fun = draw.colorkey,
           args = list(
             key = list(
               at = 0:4,
               col = rev(pal), width = 1, height = 0.5,
               labels = list(
               at = seq(0.5, 3.5, 1),
               labels = paste("cluster", 4:1))
             )
           ))),
       sp.layout = list(
         # list("sp.points", clus, col = "black", pch = 21, cex = 0.7),
         list("sp.polygons", world, col = "lightgrey",
              first = TRUE)
       ))

# # using lattice Extra...
# library(latticeExtra)
# library(sp)
# loadMeuse()
# levelplot(zinc~x+y, as.data.frame(meuse), panel=panel.levelplot.points,
#           aspect = "iso", scales = list(draw=FALSE), xlab=NULL, ylab = NULL,
#           colorkey = list(space = "bottom", width = 0.2, height = 2)) 
# levelplot(cluster~lat+lon, as.data.frame(clus), panel = panel.levelplot.points,
#           apsect = "iso", scales = list(draw=FALSE), xlab=NULL, ylab = NULL,
#           colorkey = list(space = "bottom", width = 2, height = 0.2))


# in fact, spplot.points uses xyplot (see github sp)
xyplot(coordinates(clus)[, 2] ~ coordinates(clus)[, 1], asp = "iso", col = pal, 
       pch = 20, 
       key = list(points = list(fill = pal, pch = 21,
                                border = "black", cex = 2),
                  space = "right", 
                  text = list(paste("cluster", 1:4))))

map = spplot(clus, "cluster", col.regions = pal, cex = 1.2,
             key.space = "right",
             key = list(
               points = list(fill = pal, pch = 21, border = "black",
                             cex = 1.5),
               text = list(paste("cluster", 1:4), cex = 0.8)
             ),
             sp.layout = list(
               # list("sp.points", clus, col = "black", pch = 21, cex = 1.2),
               list("sp.polygons", world, col = "lightgrey",
                    first = TRUE)
             )
)
png(filename = file.path(dir_figs, "map_cluster.png"), res = 300,
    width = 17, height = 8, units = "cm")
print(map)
dev.off()



