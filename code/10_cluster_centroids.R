# Filename: 10_cluster_centroids.R (2018-02-13)
#
# TO DO: find manuscripts which are closest to the cluster centroids
#
# Author(s): Jannes Muenchow
#
#**********************************************************
# CONTENTS-------------------------------------------------
#**********************************************************
#
# 1. ATTACH PACKAGES AND DATA
# 2. CLUSTER CENTROIDS
#
#**********************************************************
# 1 ATTACH PACKAGES AND DATA-------------------------------
#**********************************************************

# attach packages
library("dplyr")
library("sf")
library("ggplot2")
library("vegan")

# define directories
dir_main = "."
dir_data = file.path(dir_main, "data")
dir_ima = file.path(dir_main, "images")
dir_figs = file.path(dir_main, "figures")

# attach data
load(file.path(dir_ima, "01_input.Rdata"))
load(file.path(dir_ima, "07_mat.Rdata"))
load(file.path(dir_ima, "07_classes.Rdata"))

# add citavi ID
abs_df = inner_join(abs_df, dplyr::select(wos, WOS, idCitavi),
                    by = "WOS")
abs_df = inner_join(abs_df, dplyr::select(tc, tc, WOS), by = "WOS")

#**********************************************************
# 2 CLUSTER CENTROIDS--------------------------------------
#**********************************************************

# find cluster centroids (centers) in ordination space
x = data.frame(scores(ord, display = "sites")[, 1:2], class = classes$cluster)
plot(x[, 1:2], col = classes$cluster, pch = 16)
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

# use the original rownames (inherited from abs_df, i.e. idCitavi)
rownames(dists) = rownames(x)
# find the 5 closest points to each centroid
out = lapply(seq_len(ncol(dists)), function(i) {
  # make sure to only select points belonging to the corresponding cluster
  tmp = dists[x$class == i, ]
  # now select the ten closest points
  rownames(tmp[order(tmp[, i]), ])[1:15]
})
# check
x[out[[1]], ]  # ok
x[out[[2]], ]  # ok
i = 2
filter(x, class == i) %>% dplyr::select(-class) %>% plot
points(cen[i, c("DCA1", "DCA2")], cex = 3, pch = 16,  col = "yellow")
points(dplyr::select(x[out[[i]], ], -class), pch = 16, col = "lightblue")

res = data.frame(x[unlist(out), "class"], as.numeric(unlist(out)),
                 stringsAsFactors = FALSE)
names(res) = c("class", "idCitavi")
res = inner_join(dplyr::select(abs_df, -abstract), res, by = "idCitavi")

res = group_by(res, class) %>%
  top_n(5, tc) %>%
  arrange(class, desc(tc))
# to find the right cluster names, use the ordination plot
pal = RColorBrewer::brewer.pal("Set3", n =  6)[3:6]
p_1 = ggplot(out_2) +
  ggrepel::geom_label_repel(aes(out_2$scores_1,
                                out_2$scores_2,
                                fill = factor(out_2$class),
                                label = out_2$words,
                                colour = pal),
                            segment.color = NA,
                            fontface = "bold", color = "black", size = 3,
                            box.padding = unit(0.15, "lines")) +
  labs(x = "scores 1st axis", y = "scores 2nd axis") +
  guides(fill = ggplot2::guide_legend(title = NULL)) +
  theme_classic(base_size = 12) +
  scale_fill_manual(values = pal)

res$class = as.factor(res$class)
levels(res$class) =
  c("Ecology and landscape", "Participation and community",
    "Urban and infrastructure", "Media and technology")

# save your output
# write.csv2(res, file = "C:/Users/pi37pat/Desktop/centroids.csv",
#            row.names = FALSE)



