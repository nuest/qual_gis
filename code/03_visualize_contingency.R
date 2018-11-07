# Filename: 03_visualize_contingency.R (2018-04-30)
#
# TO DO:  Visualize contingency table (data collection methods, GIS methods,
#         used GIS)
#
#
# Author(s): Jannes Muenchow, Eric Krueger
#
#**********************************************************
# CONTENTS-------------------------------------------------
#**********************************************************
#
# 1. ATTACH PACKAGES AND DATA
# 2. DATA PREPARATION
# 3. REPORTED PORTIONS
# 4. CONTINGENCY ANALYSIS
# 5. VISUALIZATION
#
#**********************************************************
# 1 ATTACH PACKAGES AND DATA-------------------------------
#**********************************************************

# attach packages
pacman::p_load(ggthemes, tidyverse, plotly, ggraph, igraph, magrittr)
# attach data
abs_df = readRDS("images/00_abs_df.rds")
qual = readRDS("images/00_qual.rds")
tc = readRDS("images/00_tc.rds")
wos = readRDS("images/00_wos.rds")
gis_all = readRDS("images/00_gis_all.rds")
# load keys
(load("images/00_keys.rda"))

#**********************************************************
# 2 DATA PREPARATION---------------------------------------
#**********************************************************

# join wos and qual
relevant = left_join(wos, qual, by = c("id_citavi" = "fid_citavi"))
table(relevant$fidQualGIS_transfer)
colSums(is.na(relevant))
# build required tables
transfer = select(relevant, w = WOS, t = fidQualGIS_transfer)
soft = select(relevant, w = WOS, year = year, GIS = fidGIS)
trans_soft = left_join(soft, transfer, by = "w")
# renaming trans_soft levels in accordance with gis_key
filter(gis_key, idGIS %in% trans_soft$GIS)
# RAP GIS is not open-source -> CLARIFICATION NEEDED!!
table(trans_soft$GIS)  # RAP-GIS 3 times
trans_soft %<>% mutate(GIS = as.factor(GIS))
levels(trans_soft$GIS) =
  list("No GIS" = "1",
       "ArcGIS" = "4",
       "free GIS" =
         levels(trans_soft$GIS)[!levels(trans_soft$GIS) %in% c("1", "4")])


# applied GIS method
setdiff(trans_soft$t, agis_key$idQualGIS_transfer)  # 0, perfect
# join
trans_soft =
  inner_join(trans_soft, dplyr::select(agis_key, -Description),
             by = c("t" = "idQualGIS_transfer")) %>%
  dplyr::select(-t, t = QualGIS_transfer) %>%
  mutate(t = as.factor(t))
# set "Modelling spatial reasoning to NA (-> is not really a GIS method)
levels(trans_soft$t) =
  c("GIS extensions", "Hyperlinks", NA, NA, "Transfor-\nmations")
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

# renaming qdata in accordance with qdata_key
setdiff(qdata$qdata, qdata_key$idQualData)  # 36 and 37 are missing from the
# key table
setdiff(qdata_key$idQualData, qdata$qdata)  # 0, perfect
# add 36 and 37
qdata_key = add_row(qdata_key,
                    idQualData = 36:37,
                    Qual_Data = c("Q-Survey", NA))
qdata = inner_join(qdata, qdata_key, by = c("qdata" = "idQualData")) %>%
  dplyr::select(-qdata, qdata = Qual_Data) %>%
  mutate(qdata = as.factor(qdata))

# level condensing
levels(qdata$qdata) %<>%
  fct_collapse("Interview" = c("Interview", "Recording"),
               "Narration" = c("Story", "Narration", "Diary"),
               "Survey" = c("Survey", "Questionnaires", "Q-Survey"),
               "Observation" = c("Observation", "Field notes"))
# level renaming
levels(qdata$qdata) %<>%
  fct_recode("Focus Group" = "Focus Group Discussion",
             "Mapping\nWorkshop" = "mapping Workshop",
             "Sketch" = "sketch Maps")

colSums(is.na(qdata))  # 37 has been converted into NA
filter(qdata, is.na(qdata))  # 1 NA observation
# I think it's ok to delete it here
qdata = qdata[!is.na(qdata$qdata), ]

trans_soft_qd = full_join(trans_soft, qdata, by = "w")
saveRDS(trans_soft_qd, file = "images/03_trans_soft_qd.rds")

#**********************************************************
# 3 PORTIONS-----------------------------------------------
#**********************************************************

# these portions are reported in the text

# used qualitative data collection methods (portions)
group_by(qdata, qdata) %>%
  summarize(n = n()) %>%
  mutate(per = n / nrow(qual) * 100) %>%
  arrange(desc(per))

# applied GIS methods (transformations; portions)
group_by(trans_soft, t) %>%
  summarize(n = n()) %>%
  mutate(per = n / sum(n)) %>%
  arrange(desc(per))

# applied GIS software (portions)
group_by(trans_soft, GIS) %>%
  summarize(n = n()) %>%
  mutate(per = n / sum(n)) %>%
  arrange(desc(per))

# reported CAQDAS software
setdiff(relevant$fidCAQDAS, caq_key$idCAQDAS)  # 0, perfet
caq = select(relevant, fidCAQDAS) %>%
  left_join(., select(caq_key, idCAQDAS, CAQDAS_Typ),
             by = c("fidCAQDAS" = "idCAQDAS")) %>%
  group_by(CAQDAS_Typ) %>%
  summarize(n = n()) %>%
  mutate(per = n / sum(n) * 100) %>%
  arrange(desc(per))
filter(caq, CAQDAS_Typ != "NA") %>%
  summarise_at(funs(sum), .vars = c("n", "per"))

# reported database usage
setdiff(relevant$fidGeodatabase, gdb_key$idGeodatabase)
# ok, just keep the first recorded geodatabase
gdb = select(relevant, fidGeodatabase) %>%
  mutate(fidGeodatabase = gsub(";.*", "", fidGeodatabase)) %>%
  mutate(fidGeodatabase = as.integer(fidGeodatabase))
setdiff(gdb$fidGeodatabase, gdb_key$idGeodatabase)
# anti_join(., gdb_key, by = c("fidGeodatabase" = "idGeodatabase"))
# so there are already NAs and there is an undefined class (8)
# I suppose empty entries can be recorded as NAs
filter(gdb, is.na(fidGeodatabase))
gdb %<>%
  mutate(fidGeodatabase = ifelse(is.na(fidGeodatabase), 1, fidGeodatabase))
gdb %<>%
  left_join(., gdb_key, by = c("fidGeodatabase" = "idGeodatabase"))
# add an unknown db (8)
gdb = mutate(gdb,
             Geodatabase = ifelse(fidGeodatabase == 8, "unknown", Geodatabase))

gdb = group_by(gdb, Geodatabase) %>%
  summarize(n = n()) %>%
  mutate(per = n / sum(n) * 100) %>%
  arrange(desc(per))

filter(gdb, Geodatabase != "NA") %>%
  summarize_at(funs(sum), .vars = c("n", "per"))
# however, this also includes a category named "no Database"...

#**********************************************************
# 4 CONTINGENCY ANALYSIS (GRAPH PREPARATION)---------------
#**********************************************************

# 4.1 Graph lines==========================================
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

# 4.2 Graph bubbles========================================
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
# 5 VISUALIZATION------------------------------------------
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
ggsave("figures/03_spider.png", fig, width = 31, height = 19,
       units = "cm")
