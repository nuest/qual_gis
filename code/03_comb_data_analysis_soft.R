# Filename: 03_comb_data_analysis_soft.R (2018-04-30)
#
# TO DO:  GIS-Transfer- GIS - Software - Qual.-Data
#         script for a three sited net.plot
#
#
# Author(s): Eric Krueger, Jannes Muenchow
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
pacman::p_load(ggplot2, ggthemes, tidyverse,
               stringr, plotly, ggraph, igraph, magrittr)
# attach data
abs_df = readRDS("images/00_abs_df.rds")
qual = readRDS("images/00_qual.rds")
tc = readRDS("images/00_tc.rds")
wos = readRDS("images/00_wos.rds")
gis_all = readRDS("images/00_gis_all.rds")

#**********************************************************
# 2 DATA PREPARATION---------------------------------------
#**********************************************************

# join wos and qual
relevant = left_join(wos, qual, by = c("id_citavi" = "fid_citavi"))
relevant %<>%
  # filter out model spatial reasoning -> do not delete them but mutate them
  # filter(fidQualGIS_transfer != 7)
  mutate(fidQualGIS_transfer = ifelse(fidQualGIS_transfer == 7, NA, fidQualGIS_transfer))
table(relevant$fidQualGIS_transfer)
colSums(is.na(relevant))
# build required tables
transfer = select(relevant, w = WOS, t = fidQualGIS_transfer)
soft = select(relevant, w = WOS, year = year, GIS = fidGIS)
trans_soft = left_join(soft, transfer, by = "w")
# renaming trans_soft
trans_soft %<>% mutate_at(.funs = funs(as.factor), .vars = c("GIS", "t"))
levels(trans_soft$GIS) =
  list("No GIS" = "1",
       "ArcGIS" = "4",
       "free GIS" =
         levels(trans_soft$GIS)[!levels(trans_soft$GIS) %in% c("1", "4")])
levels(trans_soft$t) = c(NA, "Transform.", "Hyperlinks", "GIS extensions")
# spatial reasoning has been deleted
# trans_soft$t[trans_soft$t == 7 ] = "Modelling spatial reasoning"
# so you delete all NAs..., is this really a good idea???
colSums(is.na(trans_soft))  # ok, there are NAs
trans_soft = trans_soft[complete.cases(trans_soft), ]

# qdata colums
tmp = separate(relevant, fidQualData, into = paste0("qdata", 1:8), sep = ";")
qdata = select(tmp, w = WOS, starts_with("qdata"))
qdata = select(relevant, w = WOS,
               qdata = str_split_fixed(relevant$fidQualData, ";", 8))
# reshape
dt_data_all = reshape2::melt(qdata, id.vars = "w")
dt_data_all %<>%
  select(-variable) %>%
  rename(qdata = value) %>%
  mutate(qdata = as.numeric(qdata)) %>%
  # delete NAs (reasonable here)
  filter(!is.na(qdata))

# renaming dt_data_all
dt_data_all %<>% mutate(qdata = as.factor(qdata))
levels(dt_data_all$qdata)
# "22" "23" "24" "25" "26" "27" "28" "29" "30" "31" "32" "33" "34" "36" "37"
levels(dt_data_all$qdata) =
  c("Interview", "Recording", "Photography", "Survey", "Observation",
    "Focus Group", "Story", "Narration", "Survey", "Diary", "Observation",
    "mapping \n Workshop", "Sketch", "Q-Survey", NA)

# and again you are deleting all NAs...
colSums(is.na(dt_data_all))  # 37 has been converted into NA
filter(dt_data_all, is.na(qdata))
dt_data_all = dt_data_all[!is.na(dt_data_all$qdata), ]

#**********************************************************
# 3 Total Analyse & App------------------------------------
#**********************************************************
# GO ON FROM HERE TOMORROW----

kombi = left_join(dt_data_all, trans_soft, by = "w")
# and again deleting NAs -> so why left join
kombi = kombi[complete.cases(kombi$t), ]

# single totals
total_soft = trans_soft %>%
  group_by(GIS) %>%
  count(GIS, sort = TRUE)
colnames(total_soft)[1] = "name"
total_soft = as.data.frame(total_soft)


total_trans = trans_soft %>%
  group_by(t) %>%
  count(t, sort = TRUE)
colnames(total_trans)[1] = "name"
total_trans = as.data.frame(total_trans)

total_qdata = dt_data_all %>%
  group_by(qdata) %>%
  count(qdata, sort = TRUE) %>%
  filter(n > 35)
colnames(total_qdata)[1] = "name"
qdata_n = c(total_qdata$name)

# kombi totals----

total_soft_trans = trans_soft %>%
  group_by(GIS, t ) %>%
  count(GIS, sort= TRUE)
total_soft_trans = as.data.frame(total_soft_trans)
colnames(total_soft_trans)[1] = "from"
colnames(total_soft_trans)[2] = "to"


total_trans_qdata = kombi %>%
  group_by(t, qdata) %>%
  count(qdata, sort = TRUE) %>%
  filter(qdata != "Sketch",
         qdata != "Story",
         qdata != "Recording",
         qdata != "Diary",
         qdata != "Q-Survey")

total_trans_qdata = as.data.frame(total_trans_qdata)
colnames(total_trans_qdata)[1] = "from"
colnames(total_trans_qdata)[2] = "to"


#*****
#graph data-----
#*****


df_tri = rbind(total_soft_trans, total_trans_qdata)
df_tri = df_tri[complete.cases(df_tri$to),]

df_tri = df_tri %>%
  filter(n > 3)

meta = bind_rows(
  as.data.frame(mutate_if(total_qdata, is.factor, as.character)),
  mutate_if(total_trans, is.factor, as.character),
  mutate_if(total_soft, is.factor, as.character)
)
levels(meta$name) = gsub(" ", "\n", levels(meta$name))


meta = cbind(meta, "x" = 0)
meta[1:7,]$x[meta[1:7, ]$x == 0] = 1
meta[8:10,]$x[meta[8:10, ]$x == 0] = 2
meta[11:13,]$x[meta[11:13, ]$x == 0] = 3

meta = cbind(meta, "y" = 0)
meta[1:7, ]$y[meta[1:7, ]$y == 0] = rev(seq(10, 150, by = 15))
meta[8:10, ]$y[meta[8:10, ]$y == 0] = rev(seq(65, 110, by = 15))
meta[11:13, ]$y[meta[11:13, ]$y == 0] = rev(seq(70, 130, by = 30)) # ignore error

meta = cbind(meta, "col" = 0)
meta[1:7, ]$col[meta[1:7, ]$col == 0] = "grey"
meta[8:10, ]$col[meta[8:10, ]$col == 0] = "steelblue"
meta[11:13, ]$col[meta[11:13, ]$col == 0] = "indianred"

colnames(df_tri)[3] = "Combinations"

#****
#graph----
#****

library(igraph)
g = graph.data.frame(df_tri, directed = FALSE, vertices = meta)
lo = layout.norm(as.matrix(meta[, 2:3]))

ggraph(g) +
  geom_edge_link(aes(width = Combinations, alpha = Combinations),
                 show.legend = TRUE) +
  geom_node_point(size = meta$n / 5.5, col = meta$col) +
  geom_node_text(aes(label = meta$name), col = "black", vjust = 0, hjust = 0.5,
                 size = 4.5, parse = FALSE) +
  geom_node_label(aes(label = meta$n), size = 4, parse = TRUE, vjust = 1.2) +
  theme(legend.position = "bottom") +
  theme_void()
