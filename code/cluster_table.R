# Filename: cluster_table.R (2018-11-29)
#
# TO DO: cluster table: n, mean(citations), median year, pie charts used
# methods, used GIS, used transformations
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
library("dplyr")

# attach data
qual = readRDS("images/00_qual.rds")
wos = readRDS("images/00_wos.rds")
# ind = readRDS("images/04_ind.rds")
# ord = readRDS("images/04_ord.rds")
classes = readRDS("images/04_classes.rds")
tmp = readRDS("images/03_trans_soft_qd.rds")

# To do: build a table with n per cluster, mean citations and create pie charts
# or alike for GIS, transformation and data collection method per class or
# portion of the most prominent GIS/collection/transformation category

# steps
# 1. add wos to qual 
# 2. add qual to classes via id_citavi 
# 3. join with 03_trans_soft_qd.rd -> create pie charts per class

#**********************************************************
# 2 DATA PREPARATION---------------------------------------
#**********************************************************

# rownames correspond to id_citavi
clus = data.frame(cluster = classes$cluster)
clus$id_citavi = as.numeric(rownames(clus))
# sum(rownames(clus) == rownames(mat)) == nrow(mat)  # TRUE, perfect
setdiff(clus$id_citavi, qual$fid_citavi)
setdiff(clus$id_citavi, wos$id_citavi)
clus = inner_join(clus, select(wos, year, no_authors, WOS, id_citavi),
                  by = "id_citavi")

tab = group_by(clus, cluster) %>%
  summarize(n = n(),
            median_year = median(year),
            mean_author = round(mean(no_authors), 2))
tab

# join GIS & Co.
setdiff(clus$WOS, tmp$w)
clus = inner_join(clus, tmp, by = c("WOS" = "w"))
tmp = group_by(clus, cluster, GIS) %>%
  summarize(n = n()) %>%
  mutate(total = sum(n),
         percent = round(n / total * 100)) %>%
  select(cluster, GIS, percent) %>%
  reshape2::dcast(formula = cluster ~ GIS, value = percent)

compute_percentage = function(df = clus, group = "cluster", cat) {
  group_by_(df, group, cat) %>%
    summarize(n = n()) %>%
    mutate(total = sum(n),
           percent = round(n / total * 100, 2)) %>%
    select_(group, cat, "percent")
}

gis = compute_percentage(cat = "GIS")
gis = reshape2::dcast(gis, formula = cluster ~ GIS, value.var = "percent")

transf = compute_percentage(cat = "t")
transf = reshape2::dcast(transf, formula = cluster ~ t, value.var = "percent")

qdata = compute_percentage(cat = "qdata")
qdata = reshape2::dcast(qdata, formula = cluster ~ qdata, value.var = "percent")

# construct output table
out = plyr::join_all(list(select(tab, -median_year), 
                    gis, 
                    select(transf, -"NA"), 
                    select(qdata, "cluster", "Interview", "Mapping\nWorkshop", "Survey")),
                    by = "cluster")

library("flextable")
library("officer")

# create gis barplots
d = select(out, "cluster", "No GIS", "ArcGIS", "free GIS") %>%
  reshape2::melt(., id.var = "cluster")
levels(d$variable)
save_barplot(d, "~/Desktop/bar_gis_")

# create geoprocessing barplots
d = select(out, "cluster", "GIS extensions", "Hyperlinks",
       "Transformations" = "Transfor-\nmations") %>%
  reshape2::melt(., id.vars = "cluster")
levels(d$variable)
save_barplot(d, "~/Desktop/bar_geopro_")

# create data collection barplots
d = select(out, "cluster", "Interview", "Survey", 
           "Mapping Workshop" = "Mapping\nWorkshop") %>%
  reshape2::melt(., id.vars = "cluster")
levels(d$variable)
save_barplot(d, "~/Desktop/bar_dc_")

# create a flextable
tab = out[, 1:2]
tab[, c("gis", "geopro", "dc")] = NA
ft = flextable(tab) 
ft = set_header_labels(ft,
                       gis = "Used GIS (%)", 
                       geopro = "Applied geoprocessing (%)",
                       dc = "Data collection method (%)")
ft = align(ft, align = "center", part = "header")
ft = align(ft, align = "center")
# add barplots to the flextable
gis_src = paste0("~/Desktop/bar_gis_", 1:4, ".png")
ft = display(ft,
        i = 1:4,
        col_key = "gis", 
        pattern = "{{pic}}",
        formatters = 
          list(pic ~ as_image("gis", src = gis_src, width = 1.8,
                              height = 0.9)))
geopro_src = paste0("~/Desktop/bar_geopro_", 1:4, ".png")
ft = display(ft,
             i = 1:4,
             col_key = "geopro", 
             pattern = "{{pic}}",
             formatters = 
               list(pic ~ as_image("geopro",
                                   src = geopro_src, width = 1.8,
                                   height = 0.9)))
dc_src = paste0("~/Desktop/bar_dc_", 1:4, ".png")
ft = display(ft,
             i = 1:4,
             col_key = "dc", 
             pattern = "{{pic}}",
             formatters = 
               list(pic ~ as_image("dc",
                                   src = dc_src, width = 1.8,
                                   height = 0.9)))
ft


out_2 = select(out, -n, -mean_author)
d = reshape2::melt(out_2, id.var = "cluster")
d$group_variable = rep(c("GIS", "geopro", "dc"), each = 12)
library("lattice")
b_1 = barchart(value ~ variable | factor(group_variable) + factor(cluster), 
               data = d,
               groups = factor(group_variable), 
               stack = TRUE,
               # auto.key = list(title = "Group variable", columns = 3),
               scales = list(relation = list(x = "free"),
                             x = list(rot = 45, cex = 0.5)))

library("latticeExtra")
useOuterStrips(
  b_1, 
  strip = strip.custom(bg = c("white"),
                       par.strip.text = list(cex = 0.8)),
  strip.left = strip.custom(bg = "white", 
                            par.strip.text = list(cex = 0.8))
)
