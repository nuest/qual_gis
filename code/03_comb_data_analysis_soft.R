# Filename: 03_comb_data_analysis_soft.R (2018-04-30)
#
# TO DO:  Visualize contingency table (data collection methods, GIS methods,
#         used GIS)
#
#
# Author(s): Eric Krueger, Jannes Muenchow
#
#**********************************************************
# CONTENTS-------------------------------------------------
#**********************************************************
#
# 1. ATTACH PACKAGES AND DATA
# 2. DATA PREPARATION
# 3. CONTINGENCY ANALYSIS
# 4. VISUALIZATION
#
#**********************************************************
# 1 ATTACH PACKAGES AND DATA-------------------------------
#**********************************************************

# attach packages
pacman::p_load(forcats, ggplot2, ggthemes, tidyverse,
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
  mutate(fidQualGIS_transfer = ifelse(fidQualGIS_transfer == 7, NA,
                                      fidQualGIS_transfer))
table(relevant$fidQualGIS_transfer)
colSums(is.na(relevant))
# build required tables
transfer = select(relevant, w = WOS, t = fidQualGIS_transfer)
soft = select(relevant, w = WOS, year = year, GIS = fidGIS)
trans_soft = left_join(soft, transfer, by = "w")
# renaming trans_soft levels
trans_soft %<>% mutate_at(.funs = funs(as.factor), .vars = c("GIS", "t"))
levels(trans_soft$GIS) =
  list("No GIS" = "1",
       "ArcGIS" = "4",
       "free GIS" =
         levels(trans_soft$GIS)[!levels(trans_soft$GIS) %in% c("1", "4")])
levels(trans_soft$t) =
  c(NA, "Transfor-\nmations", "Hyperlinks", "GIS extensions")
# spatial reasoning has been deleted
# trans_soft$t[trans_soft$t == 7 ] = "Modelling spatial reasoning"
colSums(is.na(trans_soft))  # ok, there are NAs
# but deleting them is not really a good idea
# trans_soft = trans_soft[complete.cases(trans_soft), ]

# Data collection methods (qdata colums)
qdata = separate(relevant, fidQualData, into = paste0("qdata", 1:8), sep = ";")
qdata = select(qdata, w = WOS, starts_with("qdata"))
qdata = select(relevant, w = WOS,
               qdata = str_split_fixed(relevant$fidQualData, ";", 8))
# reshape
qdata = reshape2::melt(qdata, id.vars = "w")
qdata %<>%
  select(-variable) %>%
  rename(qdata = value) %>%
  mutate(qdata = as.numeric(qdata)) %>%
  # delete NAs (reasonable here)
  filter(!is.na(qdata))

# renaming qdata
qdata %<>% mutate(qdata = as.factor(qdata))
levels(qdata$qdata)
# "22" "23" "24" "25" "26" "27" "28" "29" "30" "31" "32" "33" "34" "36" "37"
levels(qdata$qdata) =
  c("Interview", "Recording", "Photography", "Survey", "Observation",
    "Focus Group", "Story", "Narration", "Survey", "Diary", "Observation",
    "Mapping\nWorkshop", "Sketch", "Q-Survey", NA)
# level condensing
levels(qdata$qdata) %<>%
  fct_collapse("Interview" = c("Interview", "Recording"),
               "Narration" = c("Story", "Narration", "Diary"),
               "Survey" = c("Survey", "Q-Survey"))

colSums(is.na(qdata))  # 37 has been converted into NA
filter(qdata, is.na(qdata))  # 1 NA observation
# I think it's ok to delete it here
qdata = qdata[!is.na(qdata$qdata), ]

#**********************************************************
# 3 CONTINGENCY ANALYSIS (GRAPH PREPARATION)---------------
#**********************************************************

# 3.1 Graph lines==========================================
#**********************************************************
# compute combinations (contingency table) (corresponds to the lines in the
# plot)
total_soft_trans =
  trans_soft %>%
  group_by(GIS, t) %>%
  count(GIS, sort = TRUE) %>%
  as.data.frame %>%
  rename("from" = "GIS", "to" = "t")

total_trans_qdata =
  left_join(qdata, trans_soft, by = "w") %>%
  group_by(t, qdata) %>%
  count(qdata, sort = TRUE) %>%
  as.data.frame %>%
  rename("from" = "t", "to" = "qdata")

# bind it together
lines = rbind(total_soft_trans, total_trans_qdata)
lines %<>% mutate_if(is.factor, as.character) %>%
  rename("Combinations" = "n")
lines[is.na(lines)] = "NA"
# lines = lines[complete.cases(lines$to),]
# lines = lines %>%
#   filter(n > 3)

# 3.2 Graph bubbles========================================
#**********************************************************
# compute totals for qualitative data collection method, used GIS method and
# used GIS (corresponds to the bubbles in the plot)
# data collection method
total_qdata = qdata %>%
  group_by(qdata) %>%
  count(qdata, sort = TRUE) %>%
  rename("name" = "qdata") %>%
  as.data.frame %>%
  # add coordinates and color
  mutate(x = 1,
         y = seq(150, 10, by = -15)[1:nrow(.)],
         col = "grey")

# used GIS software
total_trans = trans_soft %>%
  group_by(t) %>%
  count(t, sort = TRUE) %>%
  rename("name" = t) %>%
  as.data.frame %>%
  mutate(x = 2,
         y = seq(120, 10, -15)[1:nrow(.)],
         col = "steelblue")

# applied GIS method
total_soft = trans_soft %>%
  group_by(GIS) %>%
  count(GIS, sort = TRUE) %>%
  rename("name" = GIS) %>%
  as.data.frame %>%
  mutate(x = 3,
         y = seq(130, 10, -30)[1:nrow(.)],
         col = "indianred")

bubbles = bind_rows(total_qdata, total_trans, total_soft) %>%
  mutate_if(is.factor, as.character)
bubbles[is.na(bubbles)] = "NA"
levels(bubbles$name) = gsub(" ", "\n", levels(bubbles$name))


#**********************************************************
# 4 VISUALIZATION------------------------------------------
#**********************************************************

g = igraph::graph.data.frame(lines, directed = FALSE, vertices = bubbles)
# lo = layout.norm(as.matrix(bubbles[, 2:3]))

fig = ggraph(g) +
  geom_edge_link(aes(width = Combinations, alpha = Combinations),
                 show.legend = FALSE) +
  geom_node_point(size = bubbles$n / 10, col = bubbles$col) +
  geom_node_text(aes(label = bubbles$name), col = "black", vjust = 0,
                 hjust = 0.5, size = 4.5, parse = FALSE) +
  geom_node_label(aes(label = bubbles$n), size = 4,
                  parse = TRUE, vjust = 1.2) +
  theme(legend.position = c("bottom")) +
  theme_void()

# save the output
ggsave("figures/spider.png", fig, width = 31, height = 19,
       units = "cm")
