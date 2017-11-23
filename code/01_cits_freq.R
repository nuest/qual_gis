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
dir_main = "."
dir_fig = file.path(dir_main, "figures")
dir_data = file.path(dir_main, "data")
dir_ima = file.path(dir_main, "images")

load(file.path(dir_ima, "01_input.Rdata"))

#**********************************************************
# 2 VISUALIZATION------------------------------------------
#**********************************************************

# construct publication frequency table per year
# left_join to keep all wos records
pubs = left_join(wos, dplyr::select(qual, fidCitavi, Qual_Context),
                 by = c("idCitavi" = "fidCitavi"))
dim(pubs)  # 379
# check
pubs[duplicated(pubs$idCitavi), ]
pubs[duplicated(pubs$WOS), ]

# construct number of publication-per-year table
pubs_agg = group_by(pubs, year) %>%
  summarize(pubs = sum(length(year))) %>%
  mutate(pub_norm = pubs / sum(pubs))

# aggregate citation-per-year table
tc_agg = group_by(tc, year) %>%
  summarize(tc = sum(tc)) %>%
  mutate(tc_norm = tc / sum(tc))

pub_cits = inner_join(pubs_agg, tc_agg, by = "year") %>%
  filter(year < 2017) %>%
  dplyr::select(year, pubs, tc)
pub_cits = melt(pub_cits, id.var = "year")

p_1 = xyplot(value ~ year | variable, data = pub_cits, type = "l",
       col = c("black", grey(0.5)), ylab = list("Number", cex = 0.75), xlab = c(), 
       aspect = "xy", layout = c(2, 1),
       between = list(x = 0.5),
       scales = list(
         y = list(relation = "free"),
         tck = c(1, 0), 
         alternating = c(1, 1),
         cex = 0.75),
       strip = strip.custom(
         bg = "white",
         factor.levels = c("Publications per year", "Citations per year")),
       par.strip.text=list(cex = 0.75)
       # Create your own legend
       #  key = list(space = "right", text = list(c("all", "grad"), cex = 0.8),
       #             lines = list(col = c("black", grey(0.5)), lwd = 2,  cex = 0.6))
)

png(filename = file.path(dir_fig, "freq_cits.png"), width = 14,
    height = 8, units = "cm", res = 300)
p_1
dev.off()

# grid.text("a)", x = unit(0.125, "npc"), y = unit(0.765, "npc"), 
#           gp = gpar(font = "bold", fontsize = 10))
# grid.text("b)", x = unit(0.49, "npc"), y = unit(0.765, "npc"),
#           gp = gpar(font = "bold", fontsize = 10))

