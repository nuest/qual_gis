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
# 2. DATA PREPARATION
# 3. CREATE TABLE/FIGURE
#
#**********************************************************
# 1 ATTACH PACKAGES AND DATA-------------------------------
#**********************************************************

# attach packages
library("dplyr")

# attach data
qual = readRDS("images/00_qual.rds")
wos = readRDS("images/00_wos.rds")
tc = readRDS("images/00_tc.rds")
clus = readRDS("images/04_classes_df.rds")
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

# sum(rownames(clus) == rownames(mat)) == nrow(mat)  # TRUE, perfect
setdiff(clus$id_citavi, qual$fid_citavi)
setdiff(clus$id_citavi, wos$id_citavi)
clus = inner_join(clus, dplyr::select(wos, year, no_authors, WOS, id_citavi),
                  by = "id_citavi")
# add times cited
setdiff(clus$WOS, tc$WOS)
setdiff(tc$WOS, clus$WOS) 
# 13 WOS missing, this is because there was no abstract for thirteen pubs,
# hence, we could not assign them a cluster categorie (which was based on the
# abstract words)
clus = inner_join(clus, dplyr::select(tc, -year), by = "WOS")
# I guess the year is not that important
boxplot(clus$year ~ clus$cluster)
tab = group_by(clus, cluster) %>%
  summarize(n = n(),
            # median_year = median(year),
            mean_author = round(mean(no_authors), 2),
            mean_tc = mean(tc))
tab

# join GIS & Co.
setdiff(clus$WOS, tmp$w)
clus = inner_join(clus, tmp, by = c("WOS" = "w"))
tmp = group_by(clus, cluster, GIS) %>%
  summarize(n = n()) %>%
  mutate(total = sum(n),
         percent = round(n / total * 100)) %>%
  dplyr::select(cluster, GIS, percent) %>%
  reshape2::dcast(formula = cluster ~ GIS, value = percent)

compute_percentage = function(df = clus, group = "cluster", cat) {
  group_by_(df, group, cat) %>%
    summarize(n = n()) %>%
    mutate(total = sum(n),
           percent = round(n / total * 100, 2)) %>%
    select_(group, cat, "percent")
}

gis = compute_percentage(cat = "GIS")
# gis = reshape2::dcast(gis, formula = cluster ~ GIS, value.var = "percent")
gis = dplyr::rename(gis, feature = GIS) %>%
  mutate(cat = "GIS")
transf = compute_percentage(cat = "t")
# transf = reshape2::dcast(transf, formula = cluster ~ t, value.var = "percent")
# gp = geoprocessing
transf = dplyr::rename(transf, feature = t) %>%
  mutate(cat = "gp")

qdata = compute_percentage(cat = "qdata")
# qdata = reshape2::dcast(qdata, formula = cluster ~ qdata, value.var = "percent")
# dc = data collection
qdata = dplyr::rename(qdata, feature = "qdata") %>%
  mutate(cat = "dc")

# construct output table
out = rbind(gis, transf, qdata)

#**********************************************************
# 3 CREATE TABLE/FIGURE------------------------------------
#**********************************************************

# 3.1 Table version========================================
#**********************************************************
# attach further necessary packages
library("flextable")
library("officer")
# source your own barplot function
source("code/funs.R")

# create gis barplots
d = filter(out, cat == "GIS")
d[d$feature == "No GIS", "feature"] = NA
d = mutate(d, 
           feature = as.factor(feature),
           feature = forcats::fct_explicit_na(feature))
save_barplot(d, value = "percent", bar_name = "feature", 
             dir_name = "figures/bars/bar_gis_")

# create geoprocessing barplots
d = filter(out, cat == "gp") %>%
  mutate(feature = gsub("-\\n", "", feature),
         feature = forcats::fct_explicit_na(feature))
save_barplot(d, value = "percent", bar_name = "feature",
             dir_name = "figures/bars/bar_geopro_")

# create data collection barplots

# first, find out which are the most widely used dc methods
filter(out, cat == "dc") %>%
  group_by(cluster) %>% arrange(cluster, desc(percent)) %>% slice(1:4) 
# filter the two most important dc methods of each cluster
d = filter(out, cat == "dc" & feature %in% 
             c("Mapping\nWorkshop", "Survey", NA, "Narration")) %>%
  mutate(feature = gsub("\\n", " ", feature))
# ok, Narration is not available for cluster 1...
# so, add it
d = ungroup(d) %>%
  mutate(feature = as.factor(feature)) %>%
  mutate(feature = forcats::fct_explicit_na(feature)) %>%
  # add Narration to cluster 1
  complete(cluster, feature, fill = list(percent = 0, cat = "dc"))

# create data collection barplots
save_barplot(d, value = "percent", bar_name = "feature", 
             dir_name = "figures/bars/bar_dc_")

# create a flextable
tab_2 = tab
tab_2 = mutate_at(tab_2, .vars = c("mean_author", "mean_tc"),
          .funs = function(x) as.numeric(as.character(round(x, 1)))
       )
tab_2[, c("gis", "geopro", "dc")] = NA
ft = flextable(tab_2) 
ft = set_header_labels(ft,
                       gis = "Used GIS (%)", 
                       geopro = "Applied geoprocessing (%)",
                       dc = "Data collection\nmethod (%)")
ft = align(ft, align = "center", part = "header")
ft = align(ft, align= "left", part = "header", j = c("gis", "geopro", "dc"))
ft = align(ft, align = "center")
ft = colformat_num(ft, digits = 1, col_keys = c("mean_author", "mean_tc"))
# add barplots to the flextable
gis_src = paste0("figures/bars/bar_gis_", levels(clus$cluster), ".png")
ft = display(ft,
        i = 1:4,
        col_key = "gis", 
        pattern = "{{pic}}",
        formatters = 
          list(pic ~ as_image("gis", src = gis_src, width = 1.8,
                              height = 0.9)))
geopro_src = paste0("figures/bars/bar_geopro_", levels(clus$cluster), ".png")
ft = display(ft,
             i = 1:4,
             col_key = "geopro", 
             pattern = "{{pic}}",
             formatters = 
               list(pic ~ as_image("geopro",
                                   src = geopro_src, width = 1.8,
                                   height = 0.9)))
dc_src = paste0("figures/bars/bar_dc_", levels(clus$cluster), ".png")
ft = display(ft,
             i = 1:4,
             col_key = "dc", 
             pattern = "{{pic}}",
             formatters = 
               list(pic ~ as_image("dc",
                                   src = dc_src, width = 1.8,
                                   height = 0.9)))
ft

# 3.2 Lattice version======================================
#**********************************************************
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
