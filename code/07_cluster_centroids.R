# Filename: 07_cluster_centroids.R (2018-12-20)
#
# TO DO: Find the 20 publications closest to the cluster centroid
#
# Author(s): Jannes Muenchow
#
#**********************************************************
# CONTENTS-------------------------------------------------
#**********************************************************
#
# 1. ATTACH PACKAGES AND DATA
# 2. TOP20 STUDIES NEAR CLUSTER CENTROIDS
# 3. ADD DC, GC AND USED GIS
#
#**********************************************************
# 1 ATTACH PACKAGES AND DATA-------------------------------
#**********************************************************

# attach packages
library("dplyr")

# attach data
abs_df = readRDS("images/00_abs_df.rds")
tc = readRDS("images/00_tc.rds")
wos = readRDS("images/00_wos.rds")
trans_soft_qd = readRDS("images/03_trans_soft_qd.rds")
ord = readRDS("images/04_ord.rds")
clus = readRDS("images/04_classes_df.rds")

#**********************************************************
# 2 TOP20 STUDIES NEAR CLUSTER CENTROIDS-------------------
#**********************************************************

# add id_citavi
abs_df = inner_join(abs_df, dplyr::select(wos, WOS, id_citavi),
                    by = "WOS")

# add times cited
abs_df = inner_join(abs_df, dplyr::select(tc, tc, WOS), by = "WOS")

# find cluster centroids (centers) in ordination space
x = data.frame(scores(ord, display = "sites")[, 1:2], class = clus$cluster)
plot(x[, 1:2], col = clus$cluster, pch = 16)
cen = group_by(x, class) %>%
  summarize_all(funs(mean))
points(cen[, c("DCA1", "DCA2")], cex = 3, pch = 16,  col = "yellow")
# find the points closest to each center
dists = sp::spDists(as.matrix(dplyr::select(x, -class)),
                    as.matrix(dplyr::select(cen, -class)))
dists = as.data.frame(dists)

# using a spatial approach (centroids of convex hulls)
# x = st_as_sf(x, coords = c("DCA1", "DCA2"))
# x_1 = filter(x, class == 1)
# x_1 = st_union(x_1) %>% st_convex_hull
# plot(st_centroid(x_1), add = TRUE, col = "blue", pch = 16)
# plot(st_geometry(x))
# plot(st_centroid(x_1), add = TRUE, col = "blue", pch = 16)
# points(within(cen, rm(class)), col = "red", cex = 1, pch = 16)

# use the original rownames (inherited from abs_df, i.e., id_citavi)
rownames(dists) = rownames(x)
# find the 5 closest points to each centroid
out = lapply(seq_len(ncol(dists)), function(i) {
  # make sure to only select points belonging to the corresponding cluster
  tmp = dists[x$class == levels(x$class)[i], ]
  # now select the twenty closest points
  rownames(tmp[order(tmp[, i]), ])[1:20]
})
# check
x[out[[1]], ]  # ok
x[out[[2]], ]  # ok
i = 2
filter(x, class == levels(x$class)[i]) %>% dplyr::select(-class) %>% plot
points(cen[i, c("DCA1", "DCA2")], cex = 3, pch = 16,  col = "yellow")
points(dplyr::select(x[out[[i]], ], -class), pch = 16, col = "lightblue")

res = data.frame(x[unlist(out), "class"], as.numeric(unlist(out)),
                 stringsAsFactors = FALSE)
names(res) = c("class", "id_citavi")
res = inner_join(dplyr::select(abs_df, -abstract), res, by = "id_citavi")

res = group_by(res, class) %>%
  # top_n(15, tc) %>%
  arrange(class, desc(tc))

#**********************************************************
# 3 ADD DC, GP and used GIS--------------------------------
#**********************************************************

# add data collection methods, geoprocessing methods, and used GIS

# check if there are studies which use more than 1 GIS
tmp = group_by(trans_soft_qd, w) %>% 
  summarize(n = n_distinct(GIS)) %>% 
  pull(n) != 1
sum(tmp)  # 0
# check if there are studies which use more than one transformation method
tmp = group_by(trans_soft_qd, w) %>% 
  summarize(n = n_distinct(t)) %>% 
  pull(n) != 1
sum(tmp)  # 0

trans_soft_qd = group_by(trans_soft_qd, w) %>%
  # put all qualitative data collection methods into one column
  mutate(qd = paste(unique(qdata), collapse = ";")) %>%
  # just keep the first row of each group
  slice(1) %>%
  dplyr::select(-qdata)
dim(trans_soft_qd)  # 380 rows, perfect

res = left_join(dplyr::select(res, WOS, year, titel, tc, class),
                dplyr::select(trans_soft_qd, -year), by = c("WOS" = "w"))
setnames(res, c("WOS", "titel", "GIS", "t", "qd"), 
         c("wos_id", "title", "used_GIS", "geoprocessing", "collect_meth"))

# save your output
# res$geoprocessing = gsub("-\n", "", res$geoprocessing)
# xlsx::write.xlsx2(as.data.frame(res), 
#                   file = file.path(tempdir(), "centroids.xlsx"),
#                   row.names = FALSE)
# write.csv2(res, file = "C:/Users/pi37pat/Desktop/centroids.csv",
#            row.names = FALSE)
