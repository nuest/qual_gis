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
plyr::join_all(list(select(tab, -median_year), 
                    gis, 
                    select(transf, -"NA"), 
                    select(qdata, "cluster", "Interview", "Mapping\nWorkshop", "Survey")),
                    by = "cluster")

               