# Filename: 01_cits_freq.R (2017-11-21)
#
# TO DO: Figure of publication frequency and times cited
#
# Author(s): Jannes Muenchow, Eric Krueger
#
#**********************************************************
# CONTENTS-------------------------------------------------
#**********************************************************
#
# 1. ATTACH PACKAGES AND DATA
# 2. VISUALIZATION
#
#**********************************************************
# 1 ATTACH PACKAGES AND DATA-------------------------------
#**********************************************************

# attach packages
library("lattice")
library("grid")
library("gridExtra")
library("data.table")
library("tidyverse")

# define directories
dir_fig = "figures"
dir_data = "data"
dir_ima = "images"

# attach data
tc = readRDS(file.path(dir_ima, "00_tc.rds"))
wos = readRDS(file.path(dir_ima, "00_wos.rds"))
qual = readRDS(file.path(dir_ima, "00_qual.rds"))
gis_all = readRDS(file.path(dir_ima, "00_gis_all.rds"))


#**********************************************************
# 2 VISUALIZATION------------------------------------------
#**********************************************************

# construct publication frequency table per year
# left_join to keep all wos records
pubs = left_join(wos, dplyr::select(qual, fid_citavi, Qual_Context),
                 by = c("id_citavi" = "fid_citavi"))
dim(pubs)  # 380
# check
pubs[duplicated(pubs$id_citavi), ]  # 0, perfect
pubs[duplicated(pubs$WOS), ]  # 0, perfect

# construct number of publication-per-year table
pubs_agg = group_by(pubs, year) %>%
  summarize(pubs = sum(length(year))) %>%
  mutate(pub_norm = pubs / sum(pubs)) %>%
  arrange(year)

# aggregate citation-per-year table
tc_agg = group_by(tc, year) %>%
  summarize(tc = sum(tc)) %>%
  mutate(tc_norm = tc / sum(tc))

pub_cits = inner_join(pubs_agg, tc_agg, by = "year") %>%
  filter(year < 2017) %>%
  dplyr::select(year, pubs, tc)
# compute portions
pub_cits = mutate(pub_cits,
                  pubs = pubs / sum(pubs),
                  tc = tc / sum(tc))
pub_cits = melt(pub_cits, id.var = "year")
names(pub_cits) = c("year", "class", "qual_gis")
levels(pub_cits$class) = c("Publications", "Citations")

# compute portions
gis_all = mutate(gis_all,
                 TC = TC / sum(TC),
                 n = n / sum(n))
gis_all = melt(gis_all, id.var = "PY")
names(gis_all) = c("year", "class", "gis_all")
levels(gis_all$class) = c("Citations", "Publications")


# merge
d = full_join(gis_all, pub_cits)

xyplot(gis_all + qual_gis ~ year | class, data = d, type = "b",
       col = c("black", grey(0.5)), ylab = "Portions", xlab = c(),
       aspect = "xy", layout = c(2, 1),
       between = list(x = 0.5),
       scales = list(# y = list(relation = "free"),
         tck = c(1, 0),
         alternating = c(1, 1)),
       strip = strip.custom(bg = 'white'),
       #Create your own legend
       key = list(space = "right", text = list(c("all", "qual"), cex = 0.8),
                  lines = list(col = c("black", grey(0.5)), lwd = 2,
                               cex = 0.6))
)


png(filename = file.path(dir_fig, "freq_cits.png"), width = 14,
    height = 8, units = "cm", res = 300)
p_1
dev.off()

# grid.text("a)", x = unit(0.125, "npc"), y = unit(0.765, "npc"),
#           gp = gpar(font = "bold", fontsize = 10))
# grid.text("b)", x = unit(0.49, "npc"), y = unit(0.765, "npc"),
#           gp = gpar(font = "bold", fontsize = 10))


# cols: year, all_gis, qual_gis, class (2 levels: Publications, Citations)
